#ifndef SCM_PRINT_H
#define SCM_PRINT_H

/* Copyright 1995-1996,1998,2000-2001,2003-2004,2006,2008,2010,2012,2017-2018
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



#include "libguile/chars.h" 
#include <libguile/error.h>
#include <libguile/gc.h>
#include "libguile/options.h"



/* State information passed around during printing.
 */
#define SCM_PRINT_STATE_P(obj) (SCM_STRUCTP(obj) \
				&& (scm_is_eq (SCM_STRUCT_VTABLE(obj), \
				               scm_print_state_vtable)))
#define SCM_PRINT_STATE(obj) ((scm_print_state *) SCM_STRUCT_DATA (obj))

#define RESET_PRINT_STATE(pstate) \
do { \
  pstate->list_offset = 0; \
  pstate->top = 0; \
} while (0)

#define SCM_WRITINGP(pstate) ((pstate)->writingp)
#define SCM_SET_WRITINGP(pstate, x) { (pstate)->writingp = (x); }

#define SCM_PORT_WITH_PS_P(p)    SCM_TYP16_PREDICATE (scm_tc16_port_with_ps, p)
#define SCM_PORT_WITH_PS_PORT(p) SCM_CELL_OBJECT_1 (p)
#define SCM_PORT_WITH_PS_PS(p)   SCM_CELL_OBJECT_2 (p)

#define SCM_COERCE_OUTPORT(p) \
  (SCM_PORT_WITH_PS_P (p) ? SCM_PORT_WITH_PS_PORT (p) : p)

#define SCM_VALIDATE_OPORT_VALUE(pos, port) \
  do { \
    SCM_ASSERT (scm_valid_oport_value_p (port), port, pos, FUNC_NAME); \
  } while (0)

#define SCM_VALIDATE_PRINTSTATE(pos, a) \
  SCM_MAKE_VALIDATE_MSG(pos, a, PRINT_STATE_P, "print-state")

#define SCM_PRINT_STATE_LAYOUT "pwuwuwuwuwuwpwuwuwuwpwpw"
typedef struct scm_print_state {
  SCM handle;			/* Struct handle */
  int revealed;                 /* Has the state escaped to Scheme? */
  unsigned long writingp;	/* Writing? */
  unsigned long fancyp;		/* Fancy printing? */
  unsigned long level;		/* Max level */
  unsigned long length;		/* Max number of objects per level */
  SCM hot_ref;			/* Hot reference */
  unsigned long list_offset;
  unsigned long top;		/* Top of reference stack */
  unsigned long ceiling;	/* Max size of reference stack */
  SCM ref_vect;	 	        /* Stack of references used during
				   circular reference detection;
				   a vector. */
  SCM highlight_objects;        /* List of objects to be highlighted */
} scm_print_state;

SCM_API SCM scm_print_state_vtable;

SCM_API scm_t_bits scm_tc16_port_with_ps;

SCM_API SCM scm_print_options (SCM setting);
SCM_API SCM scm_make_print_state (void);
SCM_API void scm_free_print_state (SCM print_state);
SCM_INTERNAL SCM scm_i_port_with_print_state (SCM port, SCM print_state);
SCM_API void scm_intprint (intmax_t n, int radix, SCM port);
SCM_API void scm_uintprint (uintmax_t n, int radix, SCM port);
SCM_API void scm_ipruk (char *hdr, SCM ptr, SCM port);
SCM_API void scm_iprlist (char *hdr, SCM exp, int tlr, SCM port, scm_print_state *pstate);
SCM_API void scm_print_symbol_name (const char *str, size_t len, SCM port);
SCM_API void scm_prin1 (SCM exp, SCM port, int writingp);
SCM_API void scm_iprin1 (SCM exp, SCM port, scm_print_state *pstate);
SCM_API SCM scm_write (SCM obj, SCM port);
SCM_API SCM scm_display (SCM obj, SCM port);
SCM_API SCM scm_simple_format (SCM port, SCM message, SCM args);
SCM_API SCM scm_newline (SCM port);
SCM_API SCM scm_write_char (SCM chr, SCM port);
SCM_API SCM scm_printer_apply (SCM proc, SCM exp, SCM port, scm_print_state *);
SCM_API SCM scm_port_with_print_state (SCM port, SCM pstate);
SCM_API SCM scm_get_print_state (SCM port);
SCM_API int scm_valid_oport_value_p (SCM val);
SCM_INTERNAL void scm_init_print (void);

#ifdef GUILE_DEBUG
SCM_API SCM scm_current_pstate (void);
#endif 

#endif  /* SCM_PRINT_H */
