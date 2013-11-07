/* Copyright (C) 2001, 2009, 2010, 2011, 2012, 2013 Free Software Foundation, Inc.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 3 of
 * the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA
 */

#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdlib.h>
#include <string.h>
#include "_scm.h"
#include "frames.h"
#include <verify.h>

/* Make sure assumptions on the layout of `struct scm_vm_frame' hold.  */
verify (sizeof (SCM) == sizeof (SCM *));
verify (sizeof (struct scm_vm_frame) == 5 * sizeof (SCM));
verify (offsetof (struct scm_vm_frame, dynamic_link) == 0);


#define RELOC(frame, val)				\
  (((SCM *) (val)) + SCM_VM_FRAME_OFFSET (frame))

SCM
scm_c_make_frame (SCM stack_holder, SCM *fp, SCM *sp,
                  scm_t_uint8 *ip, scm_t_ptrdiff offset)
{
  struct scm_frame *p = scm_gc_malloc (sizeof (struct scm_frame),
                                       "vmframe");
  p->stack_holder = stack_holder;
  p->fp = fp;
  p->sp = sp;
  p->ip = ip;
  p->offset = offset;
  return scm_cell (scm_tc7_frame, (scm_t_bits)p);
}

void
scm_i_frame_print (SCM frame, SCM port, scm_print_state *pstate)
{
  scm_puts_unlocked ("#<frame ", port);
  scm_uintprint (SCM_UNPACK (frame), 16, port);
  scm_putc_unlocked (' ', port);
  scm_write (scm_frame_procedure (frame), port);
  /* don't write args, they can get us into trouble. */
  scm_puts_unlocked (">", port);
}


/* Scheme interface */

SCM_DEFINE (scm_frame_p, "frame?", 1, 0, 0,
	    (SCM obj),
	    "")
#define FUNC_NAME s_scm_frame_p
{
  return scm_from_bool (SCM_VM_FRAME_P (obj));
}
#undef FUNC_NAME

SCM_DEFINE (scm_frame_procedure, "frame-procedure", 1, 0, 0,
	    (SCM frame),
	    "")
#define FUNC_NAME s_scm_frame_procedure
{
  SCM_VALIDATE_VM_FRAME (1, frame);
  return SCM_FRAME_PROGRAM (SCM_VM_FRAME_FP (frame));
}
#undef FUNC_NAME

SCM_DEFINE (scm_frame_arguments, "frame-arguments", 1, 0, 0,
	    (SCM frame),
	    "")
#define FUNC_NAME s_scm_frame_arguments
{
  static SCM var = SCM_BOOL_F;
  
  SCM_VALIDATE_VM_FRAME (1, frame);

  if (scm_is_false (var))
    var = scm_c_module_lookup (scm_c_resolve_module ("system vm frame"),
                               "frame-arguments");

  return scm_call_1 (SCM_VARIABLE_REF (var), frame);
}
#undef FUNC_NAME

SCM_DEFINE (scm_frame_source, "frame-source", 1, 0, 0,
	    (SCM frame),
	    "")
#define FUNC_NAME s_scm_frame_source
{
  SCM proc;

  SCM_VALIDATE_VM_FRAME (1, frame);

  proc = scm_frame_procedure (frame);

  if (SCM_PROGRAM_P (proc) || SCM_RTL_PROGRAM_P (proc))
    return scm_program_source (scm_frame_procedure (frame),
                               scm_frame_instruction_pointer (frame),
                               SCM_UNDEFINED);

  return SCM_BOOL_F;
}
#undef FUNC_NAME

/* The number of locals would be a simple thing to compute, if it weren't for
   the presence of not-yet-active frames on the stack. So we have a cheap
   heuristic to detect not-yet-active frames, and skip over them. Perhaps we
   should represent them more usefully.
*/
SCM_DEFINE (scm_frame_num_locals, "frame-num-locals", 1, 0, 0,
	    (SCM frame),
	    "")
#define FUNC_NAME s_scm_frame_num_locals
{
  SCM *fp, *sp, *p;
  unsigned int n = 0;

  SCM_VALIDATE_VM_FRAME (1, frame);

  fp = SCM_VM_FRAME_FP (frame);
  sp = SCM_VM_FRAME_SP (frame);
  p = SCM_FRAME_STACK_ADDRESS (SCM_VM_FRAME_FP (frame));

  if (SCM_RTL_PROGRAM_P (fp[-1]))
    /* The frame size of an RTL program is fixed, except in the case of
       passing a wrong number of arguments to the program.  So we do
       need to use an SP for determining the number of locals.  */
    return scm_from_ptrdiff_t (sp + 1 - p);

  sp = SCM_VM_FRAME_SP (frame);
  p = SCM_FRAME_STACK_ADDRESS (SCM_VM_FRAME_FP (frame));
  while (p <= sp)
    {
      if (SCM_UNPACK (p[0]) == 0)
        /* skip over not-yet-active frame */
        p += 3;
      else
        {
          p++;
          n++;
        }
    }
  return scm_from_uint (n);
}
#undef FUNC_NAME

/* Need same not-yet-active frame logic here as in frame-num-locals */
SCM_DEFINE (scm_frame_local_ref, "frame-local-ref", 2, 0, 0,
	    (SCM frame, SCM index),
	    "")
#define FUNC_NAME s_scm_frame_local_ref
{
  SCM *sp, *p;
  unsigned int n = 0;
  unsigned int i;

  SCM_VALIDATE_VM_FRAME (1, frame);
  SCM_VALIDATE_UINT_COPY (2, index, i);

  sp = SCM_VM_FRAME_SP (frame);
  p = SCM_FRAME_STACK_ADDRESS (SCM_VM_FRAME_FP (frame));
  while (p <= sp)
    {
      if (SCM_UNPACK (p[0]) == 0)
        /* skip over not-yet-active frame */
        p += 3;
      else if (n == i)
        return *p;
      else
        {
          p++;
          n++;
        }
    }
  SCM_OUT_OF_RANGE (SCM_ARG2, index);
}
#undef FUNC_NAME

/* Need same not-yet-active frame logic here as in frame-num-locals */
SCM_DEFINE (scm_frame_local_set_x, "frame-local-set!", 3, 0, 0,
	    (SCM frame, SCM index, SCM val),
	    "")
#define FUNC_NAME s_scm_frame_local_set_x
{
  SCM *sp, *p;
  unsigned int n = 0;
  unsigned int i;

  SCM_VALIDATE_VM_FRAME (1, frame);
  SCM_VALIDATE_UINT_COPY (2, index, i);

  sp = SCM_VM_FRAME_SP (frame);
  p = SCM_FRAME_STACK_ADDRESS (SCM_VM_FRAME_FP (frame));
  while (p <= sp)
    {
      if (SCM_UNPACK (p[0]) == 0)
        /* skip over not-yet-active frame */
        p += 3;
      else if (n == i)
        {
          *p = val;
          return SCM_UNSPECIFIED;
        }
      else
        {
          p++;
          n++;
        }
    }
  SCM_OUT_OF_RANGE (SCM_ARG2, index);
}
#undef FUNC_NAME

SCM_DEFINE (scm_frame_address, "frame-address", 1, 0, 0,
	    (SCM frame),
	    "Return the frame pointer for @var{frame}.")
#define FUNC_NAME s_scm_frame_address
{
  SCM_VALIDATE_VM_FRAME (1, frame);
  return scm_from_uintptr_t ((scm_t_uintptr) SCM_VM_FRAME_FP (frame));
}
#undef FUNC_NAME

SCM_DEFINE (scm_frame_stack_pointer, "frame-stack-pointer", 1, 0, 0,
	    (SCM frame),
	    "")
#define FUNC_NAME s_scm_frame_stack_pointer
{
  SCM_VALIDATE_VM_FRAME (1, frame);

  return scm_from_uintptr_t ((scm_t_uintptr) SCM_VM_FRAME_SP (frame));
}
#undef FUNC_NAME

SCM_DEFINE (scm_frame_instruction_pointer, "frame-instruction-pointer", 1, 0, 0,
	    (SCM frame),
	    "")
#define FUNC_NAME s_scm_frame_instruction_pointer
{
  SCM program;
  const struct scm_objcode *c_objcode;

  SCM_VALIDATE_VM_FRAME (1, frame);
  program = scm_frame_procedure (frame);

  if (SCM_RTL_PROGRAM_P (program))
    return scm_from_ptrdiff_t (SCM_VM_FRAME_IP (frame) -
                               (scm_t_uint8 *) SCM_RTL_PROGRAM_CODE (program));

  if (!SCM_PROGRAM_P (program))
    return SCM_INUM0;

  c_objcode = SCM_PROGRAM_DATA (program);
  return scm_from_unsigned_integer ((SCM_VM_FRAME_IP (frame)
                                     - SCM_C_OBJCODE_BASE (c_objcode)));
}
#undef FUNC_NAME

SCM_DEFINE (scm_frame_return_address, "frame-return-address", 1, 0, 0,
	    (SCM frame),
	    "")
#define FUNC_NAME s_scm_frame_return_address
{
  SCM_VALIDATE_VM_FRAME (1, frame);
  return scm_from_uintptr_t ((scm_t_uintptr) (SCM_FRAME_RETURN_ADDRESS
                                              (SCM_VM_FRAME_FP (frame))));
}
#undef FUNC_NAME

SCM_DEFINE (scm_frame_mv_return_address, "frame-mv-return-address", 1, 0, 0,
	    (SCM frame),
	    "")
#define FUNC_NAME s_scm_frame_mv_return_address
{
  SCM_VALIDATE_VM_FRAME (1, frame);
  return scm_from_uintptr_t ((scm_t_uintptr)
                             (SCM_FRAME_MV_RETURN_ADDRESS
                              (SCM_VM_FRAME_FP (frame))));
}
#undef FUNC_NAME

SCM_DEFINE (scm_frame_dynamic_link, "frame-dynamic-link", 1, 0, 0,
	    (SCM frame),
	    "")
#define FUNC_NAME s_scm_frame_dynamic_link
{
  SCM_VALIDATE_VM_FRAME (1, frame);
  /* fixme: munge fp if holder is a continuation */
  return scm_from_uintptr_t
    ((scm_t_uintptr)
     RELOC (frame,
            SCM_FRAME_DYNAMIC_LINK (SCM_VM_FRAME_FP (frame))));
}
#undef FUNC_NAME

SCM_DEFINE (scm_frame_previous, "frame-previous", 1, 0, 0,
	    (SCM frame),
	    "")
#define FUNC_NAME s_scm_frame_previous
{
  SCM *this_fp, *new_fp, *new_sp;
  SCM proc;

  SCM_VALIDATE_VM_FRAME (1, frame);

 again:
  this_fp = SCM_VM_FRAME_FP (frame);
  new_fp = SCM_FRAME_DYNAMIC_LINK (this_fp);
  if (new_fp) 
    {
      new_fp = RELOC (frame, new_fp);
      new_sp = SCM_FRAME_LOWER_ADDRESS (this_fp) - 1;
      frame = scm_c_make_frame (SCM_VM_FRAME_STACK_HOLDER (frame),
                                new_fp, new_sp,
                                SCM_FRAME_RETURN_ADDRESS (this_fp),
                                SCM_VM_FRAME_OFFSET (frame));
      proc = scm_frame_procedure (frame);

      if ((SCM_PROGRAM_P (proc) || SCM_RTL_PROGRAM_P (proc))
          && SCM_PROGRAM_IS_BOOT (proc))
        goto again;
      else
        return frame;
    }
  else
    return SCM_BOOL_F;
}
#undef FUNC_NAME


void
scm_init_frames (void)
{
#ifndef SCM_MAGIC_SNARFER
#include "libguile/frames.x"
#endif
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
