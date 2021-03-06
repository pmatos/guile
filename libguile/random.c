/* Copyright 1999-2001,2003,2005-2006,2009-2010,2012-2014,2017-2019,2022
     Free Software Foundation, Inc.

   This file is part of Guile.

   Guile is free software: you can redistribute it and/or modify it
   under the terms of the GNU Lesser General Public License as published
   by the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   Guile is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
   License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with Guile.  If not, see
   <https://www.gnu.org/licenses/>.  */


/* Original Author: Mikael Djurfeldt <djurfeldt@nada.kth.se> */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <math.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>

#include "scm.h"
#if SCM_ENABLE_MINI_GMP
#include "mini-gmp.h"
#else
#include <gmp.h>
#endif

#include "arrays.h"
#include "feature.h"
#include "generalized-vectors.h"
#include "gsubr.h"
#include "list.h"
#include "modules.h"
#include "numbers.h"
#include "numbers.h"
#include "pairs.h"
#include "smob.h"
#include "srfi-4.h"
#include "stime.h"
#include "strings.h"
#include "symbols.h"
#include "variable.h"
#include "vectors.h"

#include "random.h"



/*
 * A plugin interface for RNGs
 *
 * Using this interface, it is possible for the application to tell
 * libguile to use a different RNG.  This is desirable if it is
 * necessary to use the same RNG everywhere in the application in
 * order to prevent interference, if the application uses RNG
 * hardware, or if the application has special demands on the RNG.
 *
 * Look in random.h and how the default generator is "plugged in" in
 * scm_init_random().
 */

scm_t_rng scm_the_rng;


/*
 * The prepackaged RNG
 *
 * This is the MWC (Multiply With Carry) random number generator
 * described by George Marsaglia at the Department of Statistics and
 * Supercomputer Computations Research Institute, The Florida State
 * University (http://stat.fsu.edu/~geo).
 *
 * It uses 64 bits, has a period of 4578426017172946943 (4.6e18), and
 * passes all tests in the DIEHARD test suite
 * (http://stat.fsu.edu/~geo/diehard.html)
 */

typedef struct scm_t_i_rstate {
  scm_t_rstate rstate;
  uint32_t w;
  uint32_t c;
} scm_t_i_rstate;


#define A 2131995753UL

#ifndef M_PI
#define M_PI 3.14159265359
#endif

static uint32_t
scm_i_uniform32 (scm_t_rstate *state)
{
  scm_t_i_rstate *istate = (scm_t_i_rstate*) state;
  uint64_t x = (uint64_t) A * istate->w + istate->c;
  uint32_t w = x & 0xffffffffUL;
  istate->w = w;
  istate->c = x >> 32L;
  return w;
}

static void
scm_i_init_rstate (scm_t_rstate *state, const char *seed, int n)
{
  scm_t_i_rstate *istate = (scm_t_i_rstate*) state;
  uint32_t w = 0L;
  uint32_t c = 0L;
  int i, m;
  for (i = 0; i < n; ++i)
    {
      m = i % 8;
      if (m < 4)
	w += seed[i] << (8 * m);
      else
        c += seed[i] << (8 * (m - 4));
    }
  if ((w == 0 && c == 0) || (w == -1 && c == A - 1))
    ++c;
  istate->w = w;
  istate->c = c;
}

static scm_t_rstate *
scm_i_copy_rstate (scm_t_rstate *state)
{
  scm_t_rstate *new_state;

  new_state = scm_gc_malloc_pointerless (state->rng->rstate_size,
					 "random-state");
  return memcpy (new_state, state, state->rng->rstate_size);
}

SCM_SYMBOL(scm_i_rstate_tag, "multiply-with-carry");

static void
scm_i_rstate_from_datum (scm_t_rstate *state, SCM value)
#define FUNC_NAME "scm_i_rstate_from_datum"
{
  scm_t_i_rstate *istate = (scm_t_i_rstate*) state;
  uint32_t w, c;
  long length;
  
  SCM_VALIDATE_LIST_COPYLEN (SCM_ARG1, value, length);
  SCM_ASSERT (length == 3, value, SCM_ARG1, FUNC_NAME);
  SCM_ASSERT (scm_is_eq (SCM_CAR (value), scm_i_rstate_tag),
              value, SCM_ARG1, FUNC_NAME);
  SCM_VALIDATE_UINT_COPY (SCM_ARG1, SCM_CADR (value), w);
  SCM_VALIDATE_UINT_COPY (SCM_ARG1, SCM_CADDR (value), c);

  istate->w = w;
  istate->c = c;
}
#undef FUNC_NAME

static SCM
scm_i_rstate_to_datum (scm_t_rstate *state)
{
  scm_t_i_rstate *istate = (scm_t_i_rstate*) state;
  return scm_list_3 (scm_i_rstate_tag,
                     scm_from_uint32 (istate->w),
                     scm_from_uint32 (istate->c));
}


/*
 * Random number library functions
 */

scm_t_rstate *
scm_c_make_rstate (const char *seed, int n)
{
  scm_t_rstate *state;

  state = scm_gc_malloc_pointerless (scm_the_rng.rstate_size,
				     "random-state");
  state->rng = &scm_the_rng;
  state->normal_next = 0.0;
  state->rng->init_rstate (state, seed, n);
  return state;
}

scm_t_rstate *
scm_c_rstate_from_datum (SCM datum)
{
  scm_t_rstate *state;

  state = scm_gc_malloc_pointerless (scm_the_rng.rstate_size,
				     "random-state");
  state->rng = &scm_the_rng;
  state->normal_next = 0.0;
  state->rng->from_datum (state, datum);
  return state;
}

scm_t_rstate *
scm_c_default_rstate ()
#define FUNC_NAME "scm_c_default_rstate"
{
  SCM state = SCM_VARIABLE_REF (scm_var_random_state);
  if (!SCM_RSTATEP (state))
    SCM_MISC_ERROR ("*random-state* contains bogus random state", SCM_EOL);
  return SCM_RSTATE (state);
}
#undef FUNC_NAME


double
scm_c_uniform01 (scm_t_rstate *state)
{
  double x = (double) state->rng->random_bits (state) / (double) 0xffffffffUL;
  return ((x + (double) state->rng->random_bits (state))
	  / (double) 0xffffffffUL);
}

double
scm_c_normal01 (scm_t_rstate *state)
{
  if (state->normal_next != 0.0)
    {
      double ret = state->normal_next;

      state->normal_next = 0.0;

      return ret;
    }
  else
    {
      double r, a, n;
      
      r = sqrt (-2.0 * log (scm_c_uniform01 (state)));
      a = 2.0 * M_PI * scm_c_uniform01 (state);
      
      n = r * sin (a);
      state->normal_next = r * cos (a);
      
      return n;
    }
}

double
scm_c_exp1 (scm_t_rstate *state)
{
  return - log (scm_c_uniform01 (state));
}

unsigned char scm_masktab[256];

static inline uint32_t
scm_i_mask32 (uint32_t m)
{
  return (m < 0x100
	  ? scm_masktab[m]
	  : (m < 0x10000
	     ? scm_masktab[m >> 8] << 8 | 0xff
	     : (m < 0x1000000
		? scm_masktab[m >> 16] << 16 | 0xffff
		: ((uint32_t) scm_masktab[m >> 24]) << 24 | 0xffffff)));
}

uint32_t
scm_c_random (scm_t_rstate *state, uint32_t m)
{
  uint32_t r, mask = scm_i_mask32 (m);
  while ((r = state->rng->random_bits (state) & mask) >= m);
  return r;
}

uint64_t
scm_c_random64 (scm_t_rstate *state, uint64_t m)
{
  uint64_t r;
  uint32_t mask;

  if (m <= UINT32_MAX)
    return scm_c_random (state, (uint32_t) m);
  
  mask = scm_i_mask32 (m >> 32);
  while ((r = ((uint64_t) (state->rng->random_bits (state) & mask) << 32)
          | state->rng->random_bits (state)) >= m)
    ;
  return r;
}

/*
  SCM scm_c_random_bignum (scm_t_rstate *state, SCM m)

  Takes a random state (source of random bits) and a bignum m.
  Returns a bignum b, 0 <= b < m.

  It does this by allocating a bignum b with as many base 65536 digits
  as m, filling b with random bits (in 32 bit chunks) up to the most
  significant 1 in m, and, finally checking if the resultant b is too
  large (>= m).  If too large, we simply repeat the process again.  (It
  is important to throw away all generated random bits if b >= m,
  otherwise we'll end up with a distorted distribution.)

*/

SCM
scm_c_random_bignum (scm_t_rstate *state, SCM m)
{
  mpz_t result, zm;
  mpz_init (result);
  mpz_init (zm);
  scm_to_mpz (m, zm);
  const size_t m_bits = mpz_sizeinbase (zm, 2);
  /* how many bits would only partially fill the last uint32_t? */
  const size_t end_bits = m_bits % (sizeof (uint32_t) * SCM_CHAR_BIT);
  uint32_t *random_chunks = NULL;
  const uint32_t num_full_chunks =
    m_bits / (sizeof (uint32_t) * SCM_CHAR_BIT);
  const uint32_t num_chunks = num_full_chunks + ((end_bits) ? 1 : 0);

  /* we know the result will be this big */
  mpz_realloc2 (result, m_bits);

  random_chunks =
    (uint32_t *) scm_gc_calloc (num_chunks * sizeof (uint32_t),
                                     "random bignum chunks");

  do
    {
      uint32_t *current_chunk = random_chunks + (num_chunks - 1);
      uint32_t chunks_left = num_chunks;

      mpz_set_ui (result, 0);
      
      if (end_bits)
        {
          /* generate a mask with ones in the end_bits position, i.e. if
             end_bits is 3, then we'd have a mask of ...0000000111 */
          const uint32_t rndbits = state->rng->random_bits (state);
          int rshift = (sizeof (uint32_t) * SCM_CHAR_BIT) - end_bits;
          uint32_t mask = ((uint32_t)-1) >> rshift;
          uint32_t highest_bits = rndbits & mask;
          *current_chunk-- = highest_bits;
          chunks_left--;
        }
      
      while (chunks_left)
        {
          /* now fill in the remaining uint32_t sized chunks */
          *current_chunk-- = state->rng->random_bits (state);
          chunks_left--;
        }
      mpz_import (result,
                  num_chunks,
                  -1,
                  sizeof (uint32_t),
                  0,
                  0,
                  random_chunks);
      /* if result >= m, regenerate it (it is important to regenerate
	 all bits in order not to get a distorted distribution) */
    } while (mpz_cmp (result, zm) >= 0);
  scm_gc_free (random_chunks,
               num_chunks * sizeof (uint32_t),
               "random bignum chunks");
  mpz_clear (zm);
  SCM ret = scm_from_mpz (result);
  mpz_clear (result);
  return ret;
}

/*
 * Scheme level representation of random states.
 */
 
scm_t_bits scm_tc16_rstate;

static SCM
make_rstate (scm_t_rstate *state)
{
  SCM_RETURN_NEWSMOB (scm_tc16_rstate, state);
}


/*
 * Scheme level interface.
 */

SCM_GLOBAL_VARIABLE_INIT (scm_var_random_state, "*random-state*",
                          scm_seed_to_random_state
                          (scm_from_utf8_string
                           ("URL:http://stat.fsu.edu/~geo/diehard.html")));

SCM_DEFINE (scm_random, "random", 1, 1, 0, 
            (SCM n, SCM state),
            "Return a number in [0, N).\n"
            "\n"
            "Accepts a positive integer or real n and returns a\n"
            "number of the same type between zero (inclusive) and\n"
            "N (exclusive). The values returned have a uniform\n"
            "distribution.\n"
            "\n"
            "The optional argument @var{state} must be of the type produced\n"
	    "by @code{seed->random-state}. It defaults to the value of the\n"
	    "variable @var{*random-state*}. This object is used to maintain\n"
	    "the state of the pseudo-random-number generator and is altered\n"
	    "as a side effect of the random operation.")
#define FUNC_NAME s_scm_random
{
  if (SCM_UNBNDP (state))
    state = SCM_VARIABLE_REF (scm_var_random_state);
  SCM_VALIDATE_RSTATE (2, state);
  if (SCM_I_INUMP (n))
    {
      scm_t_bits m = (scm_t_bits) SCM_I_INUM (n);
      SCM_ASSERT_RANGE (1, n, SCM_I_INUM (n) > 0);
#if SCM_SIZEOF_UINTPTR_T <= 4
      return scm_from_uint32 (scm_c_random (SCM_RSTATE (state),
                                            (uint32_t) m));
#elif SCM_SIZEOF_UINTPTR_T <= 8
      return scm_from_uint64 (scm_c_random64 (SCM_RSTATE (state),
                                              (uint64_t) m));
#else
#error "Cannot deal with this platform's scm_t_bits size"
#endif
    }
  if (SCM_REALP (n))
    return scm_from_double (SCM_REAL_VALUE (n)
			    * scm_c_uniform01 (SCM_RSTATE (state)));

  if (!SCM_BIGP (n))
    SCM_WRONG_TYPE_ARG (1, n);
  return scm_c_random_bignum (SCM_RSTATE (state), n);
}
#undef FUNC_NAME

SCM_DEFINE (scm_copy_random_state, "copy-random-state", 0, 1, 0, 
            (SCM state),
            "Return a copy of the random state @var{state}.")
#define FUNC_NAME s_scm_copy_random_state
{
  if (SCM_UNBNDP (state))
    state = SCM_VARIABLE_REF (scm_var_random_state);
  SCM_VALIDATE_RSTATE (1, state);
  return make_rstate (SCM_RSTATE (state)->rng->copy_rstate (SCM_RSTATE (state)));
}
#undef FUNC_NAME

SCM_DEFINE (scm_seed_to_random_state, "seed->random-state", 1, 0, 0, 
            (SCM seed),
            "Return a new random state using @var{seed}.")
#define FUNC_NAME s_scm_seed_to_random_state
{
  SCM res;
  char *c_str;
  size_t len;

  if (SCM_NUMBERP (seed))
    seed = scm_number_to_string (seed, SCM_UNDEFINED);
  SCM_VALIDATE_STRING (1, seed);

  if (scm_i_is_narrow_string (seed))
    /* This special case of a narrow string, where latin1 is used, is
       for backward compatibility during the 2.2 stable series.  In
       future major releases, we should use UTF-8 uniformly. */
    c_str = scm_to_latin1_stringn (seed, &len);
  else
    c_str = scm_to_utf8_stringn (seed, &len);

  /* 'scm_to_*_stringn' returns a 'size_t' for the length in bytes, but
     'scm_c_make_rstate' accepts an 'int'.  Make sure the length fits in
     an 'int'. */
  if (len > INT_MAX)
    {
      free (c_str);
      SCM_OUT_OF_RANGE (1, seed);
    }

  res = make_rstate (scm_c_make_rstate (c_str, len));
  free (c_str);

  scm_remember_upto_here_1 (seed);
  return res;
  
}
#undef FUNC_NAME

SCM_DEFINE (scm_datum_to_random_state, "datum->random-state", 1, 0, 0, 
            (SCM datum),
            "Return a new random state using @var{datum}, which should have\n"
            "been obtained from @code{random-state->datum}.")
#define FUNC_NAME s_scm_datum_to_random_state
{
  return make_rstate (scm_c_rstate_from_datum (datum));
}
#undef FUNC_NAME

SCM_DEFINE (scm_random_state_to_datum, "random-state->datum", 1, 0, 0, 
            (SCM state),
            "Return a datum representation of @var{state} that may be\n"
            "written out and read back with the Scheme reader.")
#define FUNC_NAME s_scm_random_state_to_datum
{
  SCM_VALIDATE_RSTATE (1, state);
  return SCM_RSTATE (state)->rng->to_datum (SCM_RSTATE (state));
}
#undef FUNC_NAME

SCM_DEFINE (scm_random_uniform, "random:uniform", 0, 1, 0, 
            (SCM state),
	    "Return a uniformly distributed inexact real random number in\n"
	    "[0,1).")
#define FUNC_NAME s_scm_random_uniform
{
  if (SCM_UNBNDP (state))
    state = SCM_VARIABLE_REF (scm_var_random_state);
  SCM_VALIDATE_RSTATE (1, state);
  return scm_from_double (scm_c_uniform01 (SCM_RSTATE (state)));
}
#undef FUNC_NAME

SCM_DEFINE (scm_random_normal, "random:normal", 0, 1, 0, 
            (SCM state),
	    "Return an inexact real in a normal distribution.  The\n"
	    "distribution used has mean 0 and standard deviation 1.  For a\n"
	    "normal distribution with mean m and standard deviation d use\n"
	    "@code{(+ m (* d (random:normal)))}.")
#define FUNC_NAME s_scm_random_normal
{
  if (SCM_UNBNDP (state))
    state = SCM_VARIABLE_REF (scm_var_random_state);
  SCM_VALIDATE_RSTATE (1, state);
  return scm_from_double (scm_c_normal01 (SCM_RSTATE (state)));
}
#undef FUNC_NAME

static void
vector_scale_x (SCM v, double c)
{
  scm_t_array_handle handle;
  scm_t_array_dim const * dims;
  ssize_t i, inc, ubnd;

  scm_array_get_handle (v, &handle);
  dims = scm_array_handle_dims (&handle);
  if (1 == scm_array_handle_rank (&handle))
    {
      ubnd = dims[0].ubnd;
      inc = dims[0].inc;

      if (handle.element_type == SCM_ARRAY_ELEMENT_TYPE_F64)
        {
          double *elts = (double *)(handle.writable_elements) + handle.base;
          for (i = dims[0].lbnd; i <= ubnd; ++i, elts += inc)
            *elts *= c;
          return;
        }
      else if (handle.element_type == SCM_ARRAY_ELEMENT_TYPE_SCM)
        {
          SCM *elts = (SCM *)(handle.writable_elements) + handle.base;
          for (i = dims[0].lbnd; i <= ubnd; ++i, elts += inc)
            SCM_REAL_VALUE (*elts) *= c;
          return;
        }
    }
  scm_array_handle_release (&handle);
  scm_misc_error (NULL, "must be a rank-1 array of type #t or 'f64", scm_list_1 (v));
}

static double
vector_sum_squares (SCM v)
{
  double x, sum = 0.0;
  scm_t_array_handle handle;
  scm_t_array_dim const * dims;
  ssize_t i, inc, ubnd;

  scm_array_get_handle (v, &handle);
  dims = scm_array_handle_dims (&handle);
  if (1 == scm_array_handle_rank (&handle))
    {
      ubnd = dims[0].ubnd;
      inc = dims[0].inc;
      if (handle.element_type == SCM_ARRAY_ELEMENT_TYPE_F64)
        {
          const double *elts = (const double *)(handle.elements) + handle.base;
          for (i = dims[0].lbnd; i <= ubnd; ++i, elts += inc)
            {
              x = *elts;
              sum += x * x;
            }
          return sum;
        }
      else if (handle.element_type == SCM_ARRAY_ELEMENT_TYPE_SCM)
        {
          const SCM *elts = (const SCM *)(handle.elements) + handle.base;
          for (i = dims[0].lbnd; i <= ubnd; ++i, elts += inc)
            {
              x = SCM_REAL_VALUE (*elts);
              sum += x * x;
            }
          return sum;
        }
    }
  scm_array_handle_release (&handle);
  scm_misc_error (NULL, "must be an array of type #t or 'f64", scm_list_1 (v));
}

/* For the uniform distribution on the solid sphere, note that in
 * this distribution the length r of the vector has cumulative
 * distribution r^n; i.e., u=r^n is uniform [0,1], so r can be
 * generated as r=u^(1/n).
 */
SCM_DEFINE (scm_random_solid_sphere_x, "random:solid-sphere!", 1, 1, 0, 
            (SCM v, SCM state),
	    "Fills @var{vect} with inexact real random numbers the sum of\n"
	    "whose squares is less than 1.0.  Thinking of @var{vect} as\n"
	    "coordinates in space of dimension @var{n} @math{=}\n"
	    "@code{(vector-length @var{vect})}, the coordinates are\n"
	    "uniformly distributed within the unit @var{n}-sphere.")
#define FUNC_NAME s_scm_random_solid_sphere_x
{
  if (SCM_UNBNDP (state))
    state = SCM_VARIABLE_REF (scm_var_random_state);
  SCM_VALIDATE_RSTATE (2, state);
  scm_random_normal_vector_x (v, state);
  vector_scale_x (v,
		  pow (scm_c_uniform01 (SCM_RSTATE (state)),
		       1.0 / scm_c_array_length (v))
		  / sqrt (vector_sum_squares (v)));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_random_hollow_sphere_x, "random:hollow-sphere!", 1, 1, 0,
            (SCM v, SCM state),
            "Fills vect with inexact real random numbers\n"
            "the sum of whose squares is equal to 1.0.\n"
            "Thinking of vect as coordinates in space of\n"
            "dimension n = (vector-length vect), the coordinates\n"
            "are uniformly distributed over the surface of the\n"
            "unit n-sphere.")
#define FUNC_NAME s_scm_random_hollow_sphere_x
{
  if (SCM_UNBNDP (state))
    state = SCM_VARIABLE_REF (scm_var_random_state);
  SCM_VALIDATE_RSTATE (2, state);
  scm_random_normal_vector_x (v, state);
  vector_scale_x (v, 1 / sqrt (vector_sum_squares (v)));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_random_normal_vector_x, "random:normal-vector!", 1, 1, 0,
            (SCM v, SCM state),
            "Fills vect with inexact real random numbers that are\n"
            "independent and standard normally distributed\n"
            "(i.e., with mean 0 and variance 1).")
#define FUNC_NAME s_scm_random_normal_vector_x
{
  scm_t_array_handle handle;
  scm_t_array_dim const * dims;
  ssize_t i;

  if (SCM_UNBNDP (state))
    state = SCM_VARIABLE_REF (scm_var_random_state);
  SCM_VALIDATE_RSTATE (2, state);

  scm_array_get_handle (v, &handle);
  if (1 != scm_array_handle_rank (&handle))
    {
      scm_array_handle_release (&handle);
      scm_wrong_type_arg_msg (NULL, 0, v, "rank 1 array");
    }

  dims = scm_array_handle_dims (&handle);

  if (handle.element_type == SCM_ARRAY_ELEMENT_TYPE_SCM)
    {
      SCM *elts = scm_array_handle_writable_elements (&handle);
      for (i = dims->lbnd; i <= dims->ubnd; i++, elts += dims->inc)
        *elts = scm_from_double (scm_c_normal01 (SCM_RSTATE (state)));
    }
  else
    {
      /* must be a f64vector. */
      double *elts = scm_array_handle_f64_writable_elements (&handle);
      for (i = dims->lbnd; i <= dims->ubnd; i++, elts += dims->inc)
        *elts = scm_c_normal01 (SCM_RSTATE (state));
    }

  scm_array_handle_release (&handle);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_random_exp, "random:exp", 0, 1, 0,
            (SCM state),
	    "Return an inexact real in an exponential distribution with mean\n"
	    "1.  For an exponential distribution with mean u use (* u\n"
	    "(random:exp)).")
#define FUNC_NAME s_scm_random_exp
{
  if (SCM_UNBNDP (state))
    state = SCM_VARIABLE_REF (scm_var_random_state);
  SCM_VALIDATE_RSTATE (1, state);
  return scm_from_double (scm_c_exp1 (SCM_RSTATE (state)));
}
#undef FUNC_NAME

/* Return a new random-state seeded from the time, date, process ID, an
   address from a freshly allocated heap cell, an address from the local
   stack frame, and a high-resolution timer if available.  This is only
   to be used as a last resort, when no better source of entropy is
   available. */
static SCM
random_state_of_last_resort (void)
{
  SCM state;
  SCM time_of_day = scm_gettimeofday ();
  SCM sources = scm_list_n
    (scm_from_unsigned_integer (SCM_UNPACK (time_of_day)),  /* heap addr */
     /* Avoid scm_getpid, since it depends on HAVE_POSIX. */
     scm_from_unsigned_integer (getpid ()),                 /* process ID */
     scm_get_internal_real_time (), /* high-resolution process timer */
     scm_from_unsigned_integer ((scm_t_bits) &time_of_day), /* stack addr */
     scm_car (time_of_day), /* seconds since midnight 1970-01-01 UTC */
     scm_cdr (time_of_day), /* microsecond component of the above clock */
     SCM_UNDEFINED);

  /* Concatenate the sources bitwise to form the seed */
  SCM seed = SCM_INUM0;
  while (scm_is_pair (sources))
    {
      seed = scm_logxor (seed, scm_ash (scm_car (sources),
                                        scm_integer_length (seed)));
      sources = scm_cdr (sources);
    }

  /* FIXME The following code belongs in `scm_seed_to_random_state',
     and here we should simply do:

       return scm_seed_to_random_state (seed);

     Unfortunately, `scm_seed_to_random_state' only preserves around 32
     bits of entropy from the provided seed.  I don't know if it's okay
     to fix that in 2.0, so for now we have this workaround. */
  {
    int i, len;
    unsigned char *buf;
    len = scm_to_int (scm_ceiling_quotient (scm_integer_length (seed),
                                            SCM_I_MAKINUM (8)));
    buf = (unsigned char *) malloc (len);
    for (i = len-1; i >= 0; --i)
      {
        buf[i] = scm_to_int (scm_logand (seed, SCM_I_MAKINUM (255)));
        seed = scm_ash (seed, SCM_I_MAKINUM (-8));
      }
    state = make_rstate (scm_c_make_rstate ((char *) buf, len));
    free (buf);
  }
  return state;
}

/* Attempt to fill buffer with random bytes from /dev/urandom.
   Return 1 if successful, else return 0. */
static int
read_dev_urandom (unsigned char *buf, size_t len)
{
  size_t res = 0;
  FILE *f = fopen ("/dev/urandom", "r");
  if (f)
    {
      res = fread(buf, 1, len, f);
      fclose (f);
    }
  return (res == len);
}

/* Fill a buffer with random bytes seeded from a platform-specific
   source of entropy.  /dev/urandom is used if available.  Note that
   this function provides no guarantees about the amount of entropy
   present in the returned bytes. */
void
scm_i_random_bytes_from_platform (unsigned char *buf, size_t len)
{
  if (read_dev_urandom (buf, len))
    return;
  else  /* FIXME: support other platform sources */
    {
      /* When all else fails, use this (rather weak) fallback */
      SCM random_state = random_state_of_last_resort ();
      int i;
      for (i = len-1; i >= 0; --i)
        buf[i] = scm_to_int (scm_random (SCM_I_MAKINUM (256), random_state));
    }
}

SCM_DEFINE (scm_random_state_from_platform, "random-state-from-platform", 0, 0, 0,
            (void),
            "Construct a new random state seeded from a platform-specific\n\
source of entropy, appropriate for use in non-security-critical applications.")
#define FUNC_NAME s_scm_random_state_from_platform
{
  unsigned char buf[32];
  if (read_dev_urandom (buf, sizeof(buf)))
    return make_rstate (scm_c_make_rstate ((char *) buf, sizeof(buf)));
  else
    return random_state_of_last_resort ();
}
#undef FUNC_NAME

void
scm_init_random ()
{
  int i, m;
  /* plug in default RNG */
  scm_t_rng rng =
  {
    sizeof (scm_t_i_rstate),
    scm_i_uniform32,
    scm_i_init_rstate,
    scm_i_copy_rstate,
    scm_i_rstate_from_datum,
    scm_i_rstate_to_datum
  };
  scm_the_rng = rng;

  scm_tc16_rstate = scm_make_smob_type ("random-state", 0);

  for (m = 1; m <= 0x100; m <<= 1)
    for (i = m >> 1; i < m; ++i)
      scm_masktab[i] = m - 1;

#include "random.x"

  scm_add_feature ("random");
}
