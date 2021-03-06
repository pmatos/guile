#ifndef SCM_ARRAY_HANDLE_H
#define SCM_ARRAY_HANDLE_H

/* Copyright 1995-1997,1999-2001,2004,2006,2008-2009,2011,2013-2014,2018,2021
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



#include "libguile/error.h"
#include "libguile/inline.h"
#include "libguile/numbers.h"
#include "libguile/arrays.h"



typedef SCM (*scm_t_vector_ref) (SCM, size_t);
typedef void (*scm_t_vector_set) (SCM, size_t, SCM);

typedef enum
  {
    SCM_ARRAY_ELEMENT_TYPE_SCM = 0,   /* SCM values */
    SCM_ARRAY_ELEMENT_TYPE_CHAR = 1,  /* characters */
    SCM_ARRAY_ELEMENT_TYPE_BIT = 2,   /* packed numeric values */
    SCM_ARRAY_ELEMENT_TYPE_VU8 = 3,
    SCM_ARRAY_ELEMENT_TYPE_U8 = 4,
    SCM_ARRAY_ELEMENT_TYPE_S8 = 5,
    SCM_ARRAY_ELEMENT_TYPE_U16 = 6,
    SCM_ARRAY_ELEMENT_TYPE_S16 = 7,
    SCM_ARRAY_ELEMENT_TYPE_U32 = 8,
    SCM_ARRAY_ELEMENT_TYPE_S32 = 9,
    SCM_ARRAY_ELEMENT_TYPE_U64 = 10,
    SCM_ARRAY_ELEMENT_TYPE_S64 = 11,
    SCM_ARRAY_ELEMENT_TYPE_F32 = 12,
    SCM_ARRAY_ELEMENT_TYPE_F64 = 13,
    SCM_ARRAY_ELEMENT_TYPE_C32 = 14,
    SCM_ARRAY_ELEMENT_TYPE_C64 = 15,
    SCM_ARRAY_ELEMENT_TYPE_LAST = 15
  } scm_t_array_element_type;

SCM_INTERNAL SCM scm_i_array_element_types[];


typedef struct scm_t_array_handle {
  SCM array;

  /* `Base' is an offset into elements or writable_elements, corresponding to
     the first element in the array. It would be nicer just to adjust the
     elements/writable_elements pointer, but we can't because that element might
     not even be byte-addressable, as is the case with bitvectors. A nicer
     solution would be, well, nice.
   */
  size_t base;
  size_t ndims; /* ndims == the rank of the array */
  scm_t_array_dim *dims;
  scm_t_array_dim dim0;
  scm_t_array_element_type element_type;
  const void *elements;
  void *writable_elements;

  /* The backing store for the array, and its accessors.  */
  SCM vector;
  scm_t_vector_ref vref;
  scm_t_vector_set vset;
} scm_t_array_handle;

#define scm_array_handle_rank(h) ((h)->ndims)
#define scm_array_handle_dims(h) ((h)->dims)

SCM_API void scm_array_get_handle (SCM array, scm_t_array_handle *h);
SCM_API ssize_t scm_array_handle_pos (scm_t_array_handle *h, SCM indices);
SCM_API ssize_t scm_array_handle_pos_1 (scm_t_array_handle *h, ssize_t idx0);
SCM_API ssize_t scm_array_handle_pos_2 (scm_t_array_handle *h, ssize_t idx0, ssize_t idx1);
SCM_API SCM scm_array_handle_element_type (scm_t_array_handle *h);
SCM_API void scm_array_handle_release (scm_t_array_handle *h);
SCM_API const SCM* scm_array_handle_elements (scm_t_array_handle *h);
SCM_API SCM* scm_array_handle_writable_elements (scm_t_array_handle *h);


SCM_INLINE SCM scm_array_handle_ref (scm_t_array_handle *h, ssize_t pos);
SCM_INLINE void scm_array_handle_set (scm_t_array_handle *h, ssize_t pos, SCM val);

#if SCM_CAN_INLINE || defined SCM_INLINE_C_IMPLEMENTING_INLINES
/* Either inlining, or being included from inline.c.  */

SCM_INLINE_IMPLEMENTATION SCM
scm_array_handle_ref (scm_t_array_handle *h, ssize_t p)
{
  if (SCM_UNLIKELY (p < 0 && ((size_t)-p) > h->base))
    /* catch overflow */
    scm_out_of_range (NULL, scm_from_ssize_t (p));
  /* perhaps should catch overflow here too */
  return h->vref (h->vector, h->base + p);
}

SCM_INLINE_IMPLEMENTATION void
scm_array_handle_set (scm_t_array_handle *h, ssize_t p, SCM v)
{
  if (SCM_UNLIKELY (p < 0 && ((size_t)-p) > h->base))
    /* catch overflow */
    scm_out_of_range (NULL, scm_from_ssize_t (p));
  /* perhaps should catch overflow here too */
  h->vset (h->vector, h->base + p, v);
}

#endif


SCM_INTERNAL void scm_init_array_handle (void);


#endif  /* SCM_ARRAY_HANDLE_H */
