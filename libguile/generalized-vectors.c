/* Copyright 1995-1998,2000-2006,2009-2014,2018
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
#  include <config.h>
#endif

#include "error.h"
#include "gsubr.h"

#include "generalized-vectors.h"
#include "array-handle.h"
#include "bitvectors.h"
#include "strings.h"
#include "vectors.h"
#include "srfi-4.h"

struct scm_t_vector_ctor
{
  SCM tag;
  SCM (*ctor)(SCM, SCM);
};

#define VECTOR_CTORS_N_STATIC_ALLOC 20
static struct scm_t_vector_ctor vector_ctors[VECTOR_CTORS_N_STATIC_ALLOC];
static int num_vector_ctors_registered = 0;

void
scm_i_register_vector_constructor (SCM type, SCM (*ctor)(SCM, SCM))
{
  if (num_vector_ctors_registered >= VECTOR_CTORS_N_STATIC_ALLOC)
    /* need to increase VECTOR_CTORS_N_STATIC_ALLOC, buster */
    abort ();
  else
    { 
      vector_ctors[num_vector_ctors_registered].tag = type;
      vector_ctors[num_vector_ctors_registered].ctor = ctor;
      num_vector_ctors_registered++;
    }
}

SCM_DEFINE (scm_make_generalized_vector, "make-generalized-vector", 2, 1, 0,
            (SCM type, SCM len, SCM fill),
            "Make a generalized vector")
#define FUNC_NAME s_scm_make_generalized_vector
{
  int i;
  for (i = 0; i < num_vector_ctors_registered; i++)
    if (scm_is_eq (vector_ctors[i].tag, type))
      return vector_ctors[i].ctor(len, fill);
  scm_wrong_type_arg_msg (FUNC_NAME, SCM_ARG1, type, "array type");
}
#undef FUNC_NAME

#define SCM_VECTOR_IMPLEMENTATION(type, ctor)                   \
  SCM_SNARF_INIT (scm_i_register_vector_constructor             \
                  (scm_i_array_element_types[type], ctor))


SCM_VECTOR_IMPLEMENTATION (SCM_ARRAY_ELEMENT_TYPE_SCM, scm_make_vector)
SCM_VECTOR_IMPLEMENTATION (SCM_ARRAY_ELEMENT_TYPE_BIT, scm_make_bitvector)
SCM_VECTOR_IMPLEMENTATION (SCM_ARRAY_ELEMENT_TYPE_CHAR, scm_make_string)

void
scm_init_generalized_vectors ()
{
#define REGISTER(tag, TAG)                                      \
  scm_i_register_vector_constructor                             \
    (scm_i_array_element_types[SCM_ARRAY_ELEMENT_TYPE_##TAG],   \
     scm_make_##tag##vector)

  REGISTER (u8, U8); 
  REGISTER (s8, S8); 
  REGISTER (u16, U16);
  REGISTER (s16, S16);
  REGISTER (u32, U32);
  REGISTER (s32, S32);
  REGISTER (u64, U64);
  REGISTER (s64, S64);
  REGISTER (f32, F32);
  REGISTER (f64, F64);
  REGISTER (c32, C32);
  REGISTER (c64, C64);
  
#include "generalized-vectors.x"
}
