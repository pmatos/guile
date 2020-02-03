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

// You're next
#include "array-handle.h"
#include "generalized-vectors.h"



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
scm_vector_elements (SCM vec)
#define FUNC_NAME "scm_vector_elements"
{
  SCM_VALIDATE_VECTOR (1, vec);
  return SCM_I_VECTOR_ELTS (vec);
}
#undef FUNC_NAME

SCM *
scm_vector_writable_elements (SCM vec)
#define FUNC_NAME "scm_vector_writable_elements"
{
  SCM_VALIDATE_MUTABLE_VECTOR (1, vec);
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
  SCM res;
  SCM *data;
  long i, len;

  SCM_VALIDATE_LIST_COPYLEN (1, l, len);

  res = scm_c_make_vector (len, SCM_UNSPECIFIED);
  data = SCM_I_VECTOR_WELTS (res);
  i = 0;
  while (scm_is_pair (l) && i < len) 
    {
      data[i] = SCM_CAR (l);
      l = SCM_CDR (l);
      i += 1;
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

// These functions are used by vector-copy!
// FIXME split into vector- and array- (?)

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
  scm_t_array_handle handle1;
  scm_array_get_handle (vec1, &handle1);
  if (1 != scm_array_handle_rank (&handle1))
    {
      scm_array_handle_release (&handle1);
      SCM_WRONG_TYPE_ARG (1, vec1);
    }
  else
    {
      scm_t_array_handle handle2;
      scm_array_get_handle (vec2, &handle2);
      if (1 != scm_array_handle_rank (&handle2))
        {
          scm_array_handle_release (&handle1);
          scm_array_handle_release (&handle2);
          SCM_WRONG_TYPE_ARG (4, vec2);
        }
      else
        {
          const SCM *elts1 = scm_array_handle_elements (&handle1);
          SCM *elts2 = scm_array_handle_writable_elements (&handle2);
          scm_t_array_dim *dims1 = scm_array_handle_dims (&handle1);
          scm_t_array_dim *dims2 = scm_array_handle_dims (&handle2);
          size_t len1 = dims1->ubnd + 1 - dims1->lbnd;
          size_t len2 = dims2->ubnd + 1 - dims2->lbnd;
          ssize_t inc1 = dims1->inc;
          ssize_t inc2 = dims2->inc;

          size_t i, j, e;
          i = scm_to_unsigned_integer (start1, 0, len1);
          e = scm_to_unsigned_integer (end1, i, len1);
          SCM_ASSERT_RANGE (SCM_ARG3, end1, (e-i) <= len2);
          j = scm_to_unsigned_integer (start2, 0, len2);
          SCM_ASSERT_RANGE (SCM_ARG5, start2, j <= len2 - (e - i));
  
          i *= inc1;
          e *= inc1;
          j *= inc2;
          for (; i < e; i += inc1, j += inc2)
            elts2[j] = elts1[i];

          scm_array_handle_release (&handle2);
          scm_array_handle_release (&handle1);
        }
    }
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
  scm_t_array_handle handle1;
  scm_array_get_handle (vec1, &handle1);
  if (1 != scm_array_handle_rank (&handle1))
    {
      scm_array_handle_release (&handle1);
      SCM_WRONG_TYPE_ARG (1, vec1);
    }
  else
    {
      scm_t_array_handle handle2;
      scm_array_get_handle (vec2, &handle2);
      if (1 != scm_array_handle_rank (&handle2))
        {
          scm_array_handle_release (&handle1);
          scm_array_handle_release (&handle2);
          SCM_WRONG_TYPE_ARG (4, vec2);
        }
      else
        {
          const SCM *elts1 = scm_array_handle_elements (&handle1);
          SCM *elts2 = scm_array_handle_writable_elements (&handle2);
          scm_t_array_dim *dims1 = scm_array_handle_dims (&handle1);
          scm_t_array_dim *dims2 = scm_array_handle_dims (&handle2);
          size_t len1 = dims1->ubnd + 1 - dims1->lbnd;
          size_t len2 = dims2->ubnd + 1 - dims2->lbnd;
          ssize_t inc1 = dims1->inc;
          ssize_t inc2 = dims2->inc;

          size_t i, j, e;
          i = scm_to_unsigned_integer (start1, 0, len1);
          e = scm_to_unsigned_integer (end1, i, len1);
          SCM_ASSERT_RANGE (SCM_ARG3, end1, (e-i) <= len2);
          j = scm_to_unsigned_integer (start2, 0, len2);
          SCM_ASSERT_RANGE (SCM_ARG5, start2, j <= len2 - (e - i));
  
          j += (e - i);
  
          i *= inc1;
          e *= inc1;
          j *= inc2;
          while (i < e)
            {
              e -= inc1;
              j -= inc2;
              elts2[j] = elts1[e];
            }

          scm_array_handle_release (&handle2);
          scm_array_handle_release (&handle1);
        }
    }
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_VECTOR_IMPLEMENTATION (SCM_ARRAY_ELEMENT_TYPE_SCM, scm_make_vector)


void
scm_init_vectors ()
{
#include "vectors.x"
}

