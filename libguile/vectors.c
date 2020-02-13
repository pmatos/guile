/* Copyright 1995-1996,1998-2001,2006,2008-2012,2014,2018-2020
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




#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include "bdw-gc.h"
#include "boolean.h"
#include "eq.h"
#include "gsubr.h"
#include "list.h"
#include "numbers.h"
#include "pairs.h"
#include "vectors.h"
#include <string.h>



#define VECTOR_MAX_LENGTH (SCM_T_BITS_MAX >> 8)

#define SCM_VALIDATE_MUTABLE_VECTOR(pos, v)                             \
  do {                                                                  \
    SCM_ASSERT_TYPE (SCM_I_IS_MUTABLE_VECTOR (v), v, pos, FUNC_NAME,    \
                     "mutable vector");                                 \
  } while (0)


int
scm_is_vector (SCM obj)
{
  return SCM_I_IS_VECTOR (obj);
}

const SCM *
scm_vector_elements (SCM vec, size_t *lenp)
#define FUNC_NAME "scm_vector_elements"
{
  SCM_VALIDATE_VECTOR (1, vec);
  if (lenp)
    *lenp = SCM_I_VECTOR_LENGTH (vec);
  return SCM_I_VECTOR_ELTS (vec);
}
#undef FUNC_NAME

SCM *
scm_vector_writable_elements (SCM vec, size_t *lenp)
#define FUNC_NAME "scm_vector_writable_elements"
{
  SCM_VALIDATE_MUTABLE_VECTOR (1, vec);
  if (lenp)
    *lenp = SCM_I_VECTOR_LENGTH (vec);
  return SCM_I_VECTOR_WELTS (vec);
}
#undef FUNC_NAME

SCM_DEFINE (scm_vector_p, "vector?", 1, 0, 0, 
	    (SCM obj),
	    "Return @code{#t} if @var{obj} is a vector, otherwise return\n"
	    "@code{#f}.")
#define FUNC_NAME s_scm_vector_p
{
  return scm_from_bool (scm_is_vector (obj));
}
#undef FUNC_NAME

SCM_DEFINE (scm_vector_length, "vector-length", 1, 0, 0, 
	    (SCM v),
            "Returns the number of elements in @var{vector} as an exact integer.")
#define FUNC_NAME s_scm_vector_length
{
  return scm_from_size_t (scm_c_vector_length (v));
}
#undef FUNC_NAME

size_t
scm_c_vector_length (SCM v)
#define FUNC_NAME s_scm_vector_length
{
  SCM_VALIDATE_VECTOR (1, v);

  return SCM_I_VECTOR_LENGTH (v);
}
#undef FUNC_NAME

SCM_REGISTER_PROC (s_list_to_vector, "list->vector", 1, 0, 0, scm_vector);
/*
	    "Return a newly created vector initialized to the elements of"
	    "the list @var{list}.\n\n"
	    "@lisp\n"
	    "(vector->list '#(dah dah didah)) @result{} (dah dah didah)\n"
	    "(list->vector '(dididit dah)) @result{}   #(dididit dah)\n"
	    "@end lisp")
*/
SCM_DEFINE (scm_vector, "vector", 0, 0, 1, 
	    (SCM l),
	    "@deffnx {Scheme Procedure} list->vector l\n"
	    "Return a newly allocated vector composed of the\n"
	    "given arguments.  Analogous to @code{list}.\n"
	    "\n"
	    "@lisp\n"
	    "(vector 'a 'b 'c) @result{} #(a b c)\n"
	    "@end lisp")
#define FUNC_NAME s_scm_vector
{
  long len;
  SCM_VALIDATE_LIST_COPYLEN (1, l, len);
  
  SCM res = scm_c_make_vector (len, SCM_UNSPECIFIED);
  SCM *data = SCM_I_VECTOR_WELTS (res);

  for (long i=0; i < len; ++i)
    {
      data[i] = SCM_CAR (l);
      l = SCM_CDR (l);
    }

  return res;
}
#undef FUNC_NAME

SCM_DEFINE (scm_vector_ref, "vector-ref", 2, 0, 0, 
	    (SCM vector, SCM k),
            "@var{k} must be a valid index of @var{vector}.\n"
            "@samp{Vector-ref} returns the contents of element @var{k} of\n"
            "@var{vector}.\n\n"
            "@lisp\n"
            "(vector-ref '#(1 1 2 3 5 8 13 21) 5) @result{} 8\n"
            "(vector-ref '#(1 1 2 3 5 8 13 21)\n"
            "    (let ((i (round (* 2 (acos -1)))))\n"
            "      (if (inexact? i)\n"
            "        (inexact->exact i)\n"
            "           i))) @result{} 13\n"
            "@end lisp")
#define FUNC_NAME s_scm_vector_ref
{
  return scm_c_vector_ref (vector, scm_to_size_t (k));
}
#undef FUNC_NAME

SCM
scm_c_vector_ref (SCM v, size_t k)
#define FUNC_NAME s_scm_vector_ref
{
  SCM_VALIDATE_VECTOR (1, v);

  if (k >= SCM_I_VECTOR_LENGTH (v))
    scm_out_of_range (NULL, scm_from_size_t (k));

  return SCM_VECTOR_REF (v, k);
}
#undef FUNC_NAME

SCM_DEFINE (scm_vector_set_x, "vector-set!", 3, 0, 0, 
	    (SCM vector, SCM k, SCM obj),
            "@var{k} must be a valid index of @var{vector}.\n"
            "@code{Vector-set!} stores @var{obj} in element @var{k} of @var{vector}.\n"
            "The value returned by @samp{vector-set!} is unspecified.\n"
            "@lisp\n"
            "(let ((vec (vector 0 '(2 2 2 2) \"Anna\")))\n"
            "  (vector-set! vec 1 '(\"Sue\" \"Sue\"))\n"
            "  vec) @result{}  #(0 (\"Sue\" \"Sue\") \"Anna\")\n"
            "(vector-set! '#(0 1 2) 1 \"doe\") @result{} @emph{error} ; constant vector\n"
            "@end lisp")
#define FUNC_NAME s_scm_vector_set_x
{
  scm_c_vector_set_x (vector, scm_to_size_t (k), obj);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

void
scm_c_vector_set_x (SCM v, size_t k, SCM obj)
#define FUNC_NAME s_scm_vector_set_x
{
  SCM_VALIDATE_MUTABLE_VECTOR (1, v);

  if (k >= SCM_I_VECTOR_LENGTH (v))
    scm_out_of_range (NULL, scm_from_size_t (k)); 

  SCM_VECTOR_SET (v, k, obj);
}
#undef FUNC_NAME

SCM_DEFINE (scm_make_vector, "make-vector", 1, 1, 0,
            (SCM k, SCM fill),
	    "Return a newly allocated vector of @var{k} elements.  If a\n"
	    "second argument is given, then each position is initialized to\n"
	    "@var{fill}.  Otherwise the initial contents of each position is\n"
	    "unspecified.")
#define FUNC_NAME s_scm_make_vector
{
  size_t l = scm_to_unsigned_integer (k, 0, VECTOR_MAX_LENGTH);

  if (SCM_UNBNDP (fill))
    fill = SCM_UNSPECIFIED;
  
  return scm_c_make_vector (l, fill);
}
#undef FUNC_NAME

static SCM
make_vector (size_t size)
{
  return scm_words ((size << 8) | scm_tc7_vector, size + 1);
}

SCM
scm_c_make_vector (size_t k, SCM fill)
#define FUNC_NAME s_scm_make_vector
{
  SCM vector;
  size_t j;

  SCM_ASSERT_RANGE (1, scm_from_size_t (k), k <= VECTOR_MAX_LENGTH);

  vector = make_vector (k);
  for (j = 0; j < k; ++j)
    SCM_VECTOR_SET (vector, j, fill);

  return vector;
}
#undef FUNC_NAME

SCM_DEFINE (scm_vector_copy, "vector-copy", 1, 0, 0,
	    (SCM vec),
	    "Return a copy of @var{vec}.")
#define FUNC_NAME s_scm_vector_copy
{
  SCM_VALIDATE_VECTOR(1, vec);
  size_t len = SCM_I_VECTOR_LENGTH (vec);
  SCM val = make_vector (len);
  memcpy (SCM_I_VECTOR_WELTS (val), SCM_I_VECTOR_ELTS (vec), len * sizeof(SCM));
  return val;
}
#undef FUNC_NAME


SCM_DEFINE (scm_vector_to_list, "vector->list", 1, 0, 0, 
	    (SCM vec),
	    "Return a newly allocated list composed of the elements of @var{vec}.\n"
	    "\n"
	    "@lisp\n"
	    "(vector->list '#(dah dah didah)) @result{}  (dah dah didah)\n"
	    "(list->vector '(dididit dah)) @result{}  #(dididit dah)\n"
	    "@end lisp")
#define FUNC_NAME s_scm_vector_to_list
{
  SCM_VALIDATE_VECTOR(1, vec);
  SCM res = SCM_EOL;
  ssize_t len = SCM_I_VECTOR_LENGTH (vec);
  const SCM * data = SCM_I_VECTOR_ELTS (vec);
  for (ssize_t i = len-1; i >= 0; --i)
    res = scm_cons (data[i], res);
  return res;
}
#undef FUNC_NAME

static SCM scm_vector_fill_partial_x (SCM vec, SCM fill, SCM start, SCM end);

SCM_DEFINE_STATIC (scm_vector_fill_partial_x, "vector-fill!", 2, 2, 0,
            (SCM vec, SCM fill, SCM start, SCM end),
            "Assign the value of every location in vector @var{vec} between\n"
            "@var{start} and @var{end} to @var{fill}.  @var{start} defaults\n"
            "to 0 and @var{end} defaults to the length of @var{vec}.  The value\n"
            "returned by @code{vector-fill!} is unspecified.")
#define FUNC_NAME s_scm_vector_fill_partial_x
{
  SCM_VALIDATE_MUTABLE_VECTOR(1, vec);

  SCM *data;
  size_t i = 0;
  size_t len = SCM_I_VECTOR_LENGTH (vec);

  data = SCM_I_VECTOR_WELTS (vec);

  if (!SCM_UNBNDP (start))
    i = scm_to_unsigned_integer (start, 0, len);

  if (!SCM_UNBNDP (end))
    len = scm_to_unsigned_integer (end, i, len);

  for (; i < len; ++i)
    data[i] = fill;

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM
scm_vector_fill_x (SCM vec, SCM fill)
#define FUNC_NAME s_scm_vector_fill_x
{
  return scm_vector_fill_partial_x (vec, fill, SCM_UNDEFINED, SCM_UNDEFINED);
}
#undef FUNC_NAME


SCM
scm_i_vector_equal_p (SCM x, SCM y)
{
  long i;
  for (i = SCM_I_VECTOR_LENGTH (x) - 1; i >= 0; i--)
    if (scm_is_false (scm_equal_p (SCM_I_VECTOR_ELTS (x)[i],
				   SCM_I_VECTOR_ELTS (y)[i])))
      return SCM_BOOL_F;
  return SCM_BOOL_T;
}

SCM_DEFINE (scm_vector_copy_x, "vector-copy!", 3, 2, 0,
            (SCM target, SCM tstart, SCM source, SCM sstart, SCM send),
            "Copy a block of elements from @var{source} to @var{target}, "
            "both of which must be vectors, starting in @var{target} at "
            "@var{tstart} and starting in @var{source} at @var{sstart}, ending "
            "when @var{send} - @var{sstart} elements have been copied.\n\n"
            "It is an error for @var{target} to have a length less than "
            "@var{tstart} + (@var{send} - @var{sstart}).  @var{sstart} defaults "
            "to 0 and @var{send} defaults to the length of @var{source}.\n\n"
            "If @var{target} and @var{source} are the same vector, then copying takes "
            "place as though the elements in @var{source} are first copied into a "
            "temporary vector, and that temporary vector is then copied to @var{target}.")
#define FUNC_NAME s_scm_vector_copy_x
{
  size_t slen, tlen;
  const SCM *s = scm_vector_elements (source, &slen);
  SCM *t = scm_vector_writable_elements (target, &tlen);

  size_t t0, s0, len;
  t0 = scm_to_unsigned_integer (tstart, 0, tlen);
  s0 = (SCM_UNBNDP (sstart)) ? 0 : scm_to_unsigned_integer (sstart, 0, slen);
  len = ((SCM_UNBNDP (send)) ? slen : scm_to_unsigned_integer (send, s0, slen)) - s0;
  SCM_ASSERT_RANGE (SCM_ARG3, source, t0+len <= tlen);

  memmove(t + t0, s + s0, len * sizeof(SCM));

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_vector_move_left_x, "vector-move-left!", 5, 0, 0, 
            (SCM vec1, SCM start1, SCM end1, SCM vec2, SCM start2),
	    "Copy elements from @var{vec1}, positions @var{start1} to @var{end1},\n"
	    "to @var{vec2} starting at position @var{start2}.  @var{start1} and\n"
	    "@var{start2} are inclusive indices; @var{end1} is exclusive.\n\n"
	    "@code{vector-move-left!} copies elements in leftmost order.\n"
	    "Therefore, in the case where @var{vec1} and @var{vec2} refer to the\n"
	    "same vector, @code{vector-move-left!} is usually appropriate when\n"
	    "@var{start1} is greater than @var{start2}.")
#define FUNC_NAME s_scm_vector_move_left_x
{
  size_t len1, len2;
  const SCM *elts1 = scm_vector_elements (vec1, &len1);
  SCM *elts2 = scm_vector_writable_elements (vec2, &len2);

  size_t i, j, e;
  i = scm_to_unsigned_integer (start1, 0, len1);
  e = scm_to_unsigned_integer (end1, i, len1);
  SCM_ASSERT_RANGE (SCM_ARG3, end1, (e-i) <= len2);
  j = scm_to_unsigned_integer (start2, 0, len2);
  SCM_ASSERT_RANGE (SCM_ARG5, start2, j <= len2 - (e - i));
  
  for (; i < e; ++i, ++j)
    elts2[j] = elts1[i];

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_vector_move_right_x, "vector-move-right!", 5, 0, 0, 
            (SCM vec1, SCM start1, SCM end1, SCM vec2, SCM start2),
	    "Copy elements from @var{vec1}, positions @var{start1} to @var{end1},\n"
	    "to @var{vec2} starting at position @var{start2}.  @var{start1} and\n"
	    "@var{start2} are inclusive indices; @var{end1} is exclusive.\n\n"
	    "@code{vector-move-right!} copies elements in rightmost order.\n"
	    "Therefore, in the case where @var{vec1} and @var{vec2} refer to the\n"
	    "same vector, @code{vector-move-right!} is usually appropriate when\n"
	    "@var{start1} is less than @var{start2}.")
#define FUNC_NAME s_scm_vector_move_right_x
{
  size_t len1, len2;
  const SCM *elts1 = scm_vector_elements (vec1, &len1);
  SCM *elts2 = scm_vector_writable_elements (vec2, &len2);

  size_t i, j, e;
  i = scm_to_unsigned_integer (start1, 0, len1);
  e = scm_to_unsigned_integer (end1, i, len1);
  SCM_ASSERT_RANGE (SCM_ARG3, end1, (e-i) <= len2);
  j = scm_to_unsigned_integer (start2, 0, len2);
  SCM_ASSERT_RANGE (SCM_ARG5, start2, j <= len2 - (e - i));
  
  j += (e - i);
  
  while (i < e)
    {
      --e;
      --j;
      elts2[j] = elts1[e];
    }

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

void
scm_init_vectors ()
{
#include "vectors.x"
}

