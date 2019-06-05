#ifndef SCM_SCM_H
#define SCM_SCM_H

/* Copyright 1995-2004,2006-2015,2017-2018
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

/* This is the central header for Guile that defines how Scheme values
   are represented.  Enjoy the read!  */



#include <stdint.h>

#include "libguile/scmconfig.h"




/* The value of SCM_DEBUG determines the default for most of the not yet
   defined debugging options.  This allows, for example, to enable most
   of the debugging options by simply defining SCM_DEBUG as 1.  */
#ifndef SCM_DEBUG
#define SCM_DEBUG 0
#endif

/* If SCM_DEBUG_PAIR_ACCESSES is set to 1, accesses to cons cells will
   be exhaustively checked.  Note:  If this option is enabled, guile
   will run slower than normally.  */
#ifndef SCM_DEBUG_PAIR_ACCESSES
#define SCM_DEBUG_PAIR_ACCESSES SCM_DEBUG
#endif

/* If SCM_DEBUG_REST_ARGUMENT is set to 1, functions that take rest
   arguments will check whether the rest arguments are actually passed
   as a proper list.  Otherwise, if SCM_DEBUG_REST_ARGUMENT is 0,
   functions that take rest arguments will take it for granted that
   these are passed as a proper list.  */
#ifndef SCM_DEBUG_REST_ARGUMENT
#define SCM_DEBUG_REST_ARGUMENT SCM_DEBUG
#endif

/* The macro SCM_DEBUG_TYPING_STRICTNESS indicates what level of type
   checking shall be performed with respect to the use of the SCM
   datatype.  The macro may be defined to one of the values 0, 1 and 2.

   A value of 0 means that there will be no compile time type checking,
   since the SCM datatype will be declared as an integral type.  This
   setting should only be used on systems, where casting from integral
   types to pointers may lead to loss of bit information.

   A value of 1 means that there will an intermediate level of compile
   time type checking, since the SCM datatype will be declared as a
   pointer to an undefined struct.  This setting is the default, since
   it does not cost anything in terms of performance or code size.

   A value of 2 provides a maximum level of compile time type checking
   since the SCM datatype will be declared as a struct.  This setting
   should be used for _compile time_ type checking only, since the
   compiled result is likely to be quite inefficient.  The right way to
   make use of this option is to do a 'make clean; make
   CFLAGS=-DSCM_DEBUG_TYPING_STRICTNESS=2', fix your errors, and then do
   'make clean; make'.  */
#ifndef SCM_DEBUG_TYPING_STRICTNESS
#define SCM_DEBUG_TYPING_STRICTNESS 1
#endif



/* Guile as of today can only work on systems which fulfill at least the
   following requirements:

   - scm_t_bits and SCM variables have at least 32 bits.
     Guile's type system is based on this assumption.

   - sizeof (scm_t_bits) >= sizeof (void*) and sizeof (SCM) >= sizeof (void*)
     Guile's type system is based on this assumption, since it must be
     possible to store pointers to cells on the heap in scm_t_bits and
     SCM variables.

   - sizeof (scm_t_bits) >= 4 and sizeof (scm_t_bits) is a power of 2.
     Guile's type system is based on this assumption.  In particular, it
     is assumed that cells, i. e. pairs of scm_t_bits variables, are
     eight-byte aligned.  This is because three bits of a scm_t_bits
     variable that is holding a pointer to a cell on the heap must be
     available for storing type data.

   - sizeof (scm_t_bits) <= sizeof (void*) and sizeof (SCM) <= sizeof (void*)
     In some parts of guile, scm_t_bits and SCM variables are passed to
     functions as void* arguments.  Together with the requirement above,
     this requires a one-to-one correspondence between the size of a
     void* and the sizes of scm_t_bits and SCM variables.

   - numbers are encoded using two's complement.
     The implementation of the bitwise Scheme-level operations is based on
     this assumption.  */



/* In the beginning was the Word:

   For the representation of scheme objects and their handling, Guile
   provides two types: scm_t_bits and SCM.

   - scm_t_bits values can hold bit patterns of non-objects and objects:

     Non-objects -- in this case the value may not be changed into a SCM
     value in any way.

     Objects -- in this case the value may be changed into a SCM value
     using the SCM_PACK macro.

   - SCM values can hold proper scheme objects only.  They can be
     changed into a scm_t_bits value using the SCM_UNPACK macro.

   When working in the domain of scm_t_bits values, programmers must
   keep track of any scm_t_bits value they create that is not a proper
   scheme object.  This makes sure that in the domain of SCM values
   developers can rely on the fact that they are dealing with proper
   scheme objects only.  Thus, the distinction between scm_t_bits and
   SCM values helps to identify those parts of the code where special
   care has to be taken not to create bad SCM values.  */

/* For dealing with the bit level representation of scheme objects we
   define scm_t_bits.  */
typedef intptr_t  scm_t_signed_bits;
typedef uintptr_t scm_t_bits;

#define SCM_T_SIGNED_BITS_MAX INTPTR_MAX
#define SCM_T_SIGNED_BITS_MIN INTPTR_MIN
#define SCM_T_BITS_MAX        UINTPTR_MAX


/* But as external interface, we define SCM, which may, according to the
   desired level of type checking, be defined in several ways.  */
#if (SCM_DEBUG_TYPING_STRICTNESS == 2)
  typedef union SCM { struct { scm_t_bits n; } n; } SCM;
# define SCM_UNPACK(x) ((x).n.n)
# define SCM_PACK(x) ((SCM) { { (scm_t_bits) (x) } })
#elif (SCM_DEBUG_TYPING_STRICTNESS == 1)
/* This is the default, which provides an intermediate level of compile
   time type checking while still resulting in very efficient code.  */
  typedef struct scm_unused_struct { char scm_unused_field; } *SCM;

/* The 0?: constructions makes sure that the code is never executed, and
   that there is no performance hit.  However, the alternative is
   compiled, and does generate a warning when used with the wrong
   pointer type.  We use a volatile pointer type to avoid warnings from
   clang.

   The Tru64 and ia64-hp-hpux11.23 compilers fail on `case (0?0=0:x)'
   statements, so for them type-checking is disabled.  */
# if defined __DECC || defined __HP_cc
#  define SCM_UNPACK(x) ((scm_t_bits) (x))
# else
#  define SCM_UNPACK(x) ((scm_t_bits) (0? (*(volatile SCM *)0=(x)): x))
# endif

/* There is no typechecking on SCM_PACK, since all kinds of types
   (unsigned long, void*) go in SCM_PACK.  */
# define SCM_PACK(x) ((SCM) (x))

#else
/* This should be used as a fall back solution for machines on which
   casting to a pointer may lead to loss of bit information, e. g. in
   the three least significant bits.  */
  typedef scm_t_bits SCM;
# define SCM_UNPACK(x) (x)
# define SCM_PACK(x) ((SCM) (x))
#endif

/* Packing SCM objects into and out of pointers.  */
#define SCM_UNPACK_POINTER(x) ((scm_t_bits *) (SCM_UNPACK (x)))
#define SCM_PACK_POINTER(x) (SCM_PACK ((scm_t_bits) (x)))

/* SCM values can not be compared by using the operator ==.  Use the
   following macro instead, which is the equivalent of the scheme
   predicate 'eq?'.  */
#define scm_is_eq(x, y) (SCM_UNPACK (x) == SCM_UNPACK (y))




/* Representation of scheme objects:

   Guile's type system is designed to work on systems where scm_t_bits
   and SCM variables consist of at least 32 bits.  The objects that a
   SCM variable can represent belong to one of the following two major
   categories:

   - Immediates -- meaning that the SCM variable contains an entire
     Scheme object.  That means, all the object's data (including the
     type tagging information that is required to identify the object's
     type) must fit into 32 bits.

   - Heap objects -- meaning that the SCM variable holds a pointer into
     the heap.  On systems where a pointer needs more than 32 bits this
     means that scm_t_bits and SCM variables need to be large enough to
     hold such pointers.  In contrast to immediates, the data associated
     with a heap object can consume arbitrary amounts of memory.

   The 'heap' is the memory area that is under control of Guile's
   garbage collector.  It holds allocated memory of various sizes.  The
   impact on the runtime type system is that Guile needs to be able to
   determine the type of an object given the pointer.  Usually the way
   that Guile does this is by storing a "type tag" in the first word of
   the object.

   Some objects are common enough that they get special treatment.
   Since Guile guarantees that the address of a GC-allocated object on
   the heap is 8-byte aligned, Guile can play tricks with the lower 3
   bits.  That is, since heap objects encode a pointer to an
   8-byte-aligned pointer, the three least significant bits of a SCM can
   be used to store additional information.  The bits are used to store
   information about the object's type and thus are called tc3-bits,
   where tc stands for type-code.

   For a given SCM value, the distinction whether it holds an immediate
   or heap object is based on the tc3-bits (see above) of its scm_t_bits
   equivalent: If the tc3-bits equal #b000, then the SCM value holds a
   heap object, and the scm_t_bits variable's value is just the pointer
   to the heap cell.

   Summarized, the data of a scheme object that is represented by a SCM
   variable consists of a) the SCM variable itself, b) in case of heap
   objects memory that the SCM object points to, c) in case of heap
   objects potentially additional data outside of the heap (like for
   example malloc'ed data), and d) in case of heap objects potentially
   additional data inside of the heap, since data stored in b) and c)
   may hold references to other cells.


   Immediates

   Operations on immediate objects can typically be processed faster
   than on heap objects.  The reason is that the object's data can be
   extracted directly from the SCM variable (or rather a corresponding
   scm_t_bits variable), instead of having to perform additional memory
   accesses to obtain the object's data from the heap.  In order to get
   the best possible performance frequently used data types should be
   realized as immediates.  This is, as has been mentioned above, only
   possible if the objects can be represented with 32 bits (including
   type tagging).

   In Guile, the following data types and special objects are realized
   as immediates: booleans, characters, small integers (see below), the
   empty list, the end of file object, the 'unspecified' object (which
   is delivered as a return value by functions for which the return
   value is unspecified), a 'nil' object used in the elisp-compatibility
   mode and certain other 'special' objects which are only used
   internally in Guile.

   Integers in Guile can be arbitrarily large.  On the other hand,
   integers are one of the most frequently used data types.  Especially
   integers with less than 32 bits are commonly used.  Thus, internally
   and transparently for application code guile distinguishes between
   small and large integers.  Whether an integer is a large or a small
   integer depends on the number of bits needed to represent its value.
   Small integers are those which can be represented as immediates.
   Since they don't require more than a fixed number of bits for their
   representation, they are also known as 'fixnums'.

   The tc3-combinations #b010 and #b110 are used to represent small
   integers, which allows to use the most significant bit of the
   tc3-bits to be part of the integer value being represented.  This
   means that all integers with up to 30 bits (including one bit for the
   sign) can be represented as immediates.  On systems where SCM and
   scm_t_bits variables hold more than 32 bits, the amount of bits
   usable for small integers will even be larger.  The tc3-code #b100 is
   shared among booleans, characters and the other special objects
   listed above.


   Heap Objects

   All object types not mentioned above in the list of immedate objects
   are represented as heap objects.  The amount of memory referenced by
   a heap object depends on the object's type, namely on the set of
   attributes that have to be stored with objects of that type.  Every
   heap object type is allowed to define its own layout and
   interpretation of the data stored in its cell (with some
   restrictions, see below).

   One of the design goals of guile's type system is to make it possible
   to store a scheme pair with as little memory usage as possible.  The
   minimum amount of memory that is required to store two scheme objects
   (car and cdr of a pair) is the amount of memory required by two
   scm_t_bits or SCM variables.  Therefore pairs in guile are stored in
   two words, and are tagged with a bit pattern in the SCM value, not
   with a type tag on the heap.


   Garbage collection

   During garbage collection, unreachable objects on the heap will be
   freed.  To determine the set of reachable objects, by default, the GC
   just traces all words in all heap objects.  It is possible to
   register custom tracing ("marking") procedures.

   If an object is unreachable, by default, the GC just notes this fact
   and moves on.  Later allocations will clear out the memory associated
   with the object, and re-use it.  It is possible to register custom
   finalizers, however.


   Run-time type introspection

   Guile's type system is designed to make it possible to determine a
   the type of a heap object from the object's first scm_t_bits
   variable.  (Given a SCM variable X holding a heap object, the macro
   SCM_CELL_TYPE(X) will deliver the corresponding object's first
   scm_t_bits variable.)

   If the object holds a scheme pair, then we already know that the
   first scm_t_bits variable of the cell will hold a scheme object with
   one of the following tc3-codes: #b000 (heap object), #b010 (small
   integer), #b110 (small integer), #b100 (non-integer immediate).  All
   these tc3-codes have in common, that their least significant bit is
   #b0.  This fact is used by the garbage collector to identify cells
   that hold pairs.  The remaining tc3-codes are assigned as follows:
   #b001 (class instance or, more precisely, a struct, of which a class
   instance is a special case), #b011 (closure), #b101/#b111 (all
   remaining heap object types).


   Summary of type codes of scheme objects (SCM variables)

   Here is a summary of tagging bits as they might occur in a scheme
   object.  The notation is as follows: tc stands for type code as
   before, tc<n> with n being a number indicates a type code formed by
   the n least significant bits of the SCM variables corresponding
   scm_t_bits value.

   Note that (as has been explained above) tc1==1 can only occur in the
   first scm_t_bits variable of a cell belonging to a heap object that
   is not a pair.  For an explanation of the tc tags with tc1==1, see
   the next section with the summary of the type codes on the heap.

   tc1:
     0:  For scheme objects, tc1==0 must be fulfilled.
    (1:  This can never be the case for a scheme object.)

   tc2:
     00:  Either a heap object or some non-integer immediate
    (01:  This can never be the case for a scheme object.)
     10:  Small integer
    (11:  This can never be the case for a scheme object.)

   tc3:
     000:  a heap object (pair, closure, class instance etc.)
    (001:  This can never be the case for a scheme object.)
     010:  an even small integer (least significant bit is 0).
    (011:  This can never be the case for a scheme object.)
     100:  Non-integer immediate
    (101:  This can never be the case for a scheme object.)
     110:  an odd small integer (least significant bit is 1).
    (111:  This can never be the case for a scheme object.)

   The remaining bits of the heap objects form the pointer to the heap
   cell.  The remaining bits of the small integers form the integer's
   value and sign.  Thus, the only scheme objects for which a further
   subdivision is of interest are the ones with tc3==100.

   tc8 (for objects with tc3==100):
     00000-100:  special objects ('flags')
     00001-100:  characters
     00010-100:  unused
     00011-100:  unused


   Summary of type codes on the heap

   Here is a summary of tagging in scm_t_bits values as they might occur
   in the first scm_t_bits variable of a heap cell.

   tc1:
     0:  the cell belongs to a pair.
     1:  the cell belongs to a non-pair.

   tc2:
     00:  the cell belongs to a pair with no short integer in its car.
     01:  the cell belongs to a non-pair (struct or some other heap object).
     10:  the cell belongs to a pair with a short integer in its car.
     11:  the cell belongs to a non-pair (closure or some other heap object).

   tc3:
     000:  the cell belongs to a pair with a heap object in its car.
     001:  the cell belongs to a struct
     010:  the cell belongs to a pair with an even short integer in its car.
     011:  the cell belongs to a closure
     100:  the cell belongs to a pair with a non-integer immediate in its car.
     101:  the cell belongs to some other heap object.
     110:  the cell belongs to a pair with an odd short integer in its car.
     111:  the cell belongs to some other heap object.

   tc7 (for tc3==1x1):
     See below for the list of types.  Three special tc7-codes are of
     interest: numbers, ports and smobs in fact each represent
     collections of types, which are subdivided using tc16-codes.

   tc16 (for tc7==scm_tc7_smob):
     The largest part of the space of smob types is not subdivided in a
     predefined way, since smobs can be added arbitrarily by user C
     code.  */



/* Checking if a SCM variable holds an immediate or a heap object.  */
#define SCM_IMP(x) 		(7 & SCM_UNPACK (x))
#define SCM_NIMP(x) 		(!SCM_IMP (x))
#define SCM_HEAP_OBJECT_P(x)    (SCM_NIMP (x))

/* Checking if a SCM variable holds a pair (for historical reasons, in
   Guile also known as a cons-cell): This is done by first checking that
   the SCM variable holds a heap object, and second, by checking that
   tc1==0 holds for the SCM_CELL_TYPE of the SCM variable.  */
#define SCM_I_CONSP(x) \
  (!SCM_IMP (x) && ((15 & SCM_CELL_TYPE (x)) != scm_tc4_non_pair_heap_object))



/* Definitions for immediate numbers: */

#define scm_fixnum_tag           15
#define scm_fixnum_tag_mask      15
#define scm_fixnum_tag_size      4

#define scm_fixrat_tag           7
#define scm_fixrat_tag_mask      15
#define scm_fixrat_tag_size      4
#define scm_fixrat_rank_size     6


/* Definitions for tc3: */

#define SCM_ITAG3(x) 		 (7 & SCM_UNPACK (x))
#define SCM_TYP3(x) 		 (7 & SCM_CELL_TYPE (x))

#define scm_tc3_cons		 0
#define scm_tc3_imm24		 6
#define scm_tcs_fixnums		 7
#define scm_tcs_iflo		 1: case 2: case 3: case 4: case 5


/* Definitions for tc4: */

#define scm_tc4_non_pair_heap_object  0xe


/* Definitions for tc5: */

#define scm_tc5_struct          (scm_tc4_non_pair_heap_object + 0x10)


/* Definitions for tc6: */

#define scm_tc6_misc_heap       (scm_tc4_non_pair_heap_object + 0x20)


/* Definitions for tc7: */

#define scm_tc7_smob            (scm_tc4_non_pair_heap_object + 0x40)

#define SCM_ITAG7(x) 		(0x7f & SCM_UNPACK (x))
#define SCM_TYP7(x) 		(0x7f & SCM_CELL_TYPE (x))
#define SCM_HAS_TYP7(x, tag)    (SCM_NIMP (x) && SCM_TYP7 (x) == (tag))


/* Definitions for tc11: */

#define SCM_ITAG11(x) 		(0x7ff & SCM_UNPACK (x))
#define SCM_TYP11(x) 		(0x7ff & SCM_CELL_TYPE (x))
#define SCM_HAS_HEAP_TYPE(x, type, tag)                         \
  (SCM_NIMP (x) && type (x) == (tag))
#define SCM_HAS_TYP11(x, tag)   (SCM_HAS_HEAP_TYPE (x, SCM_TYP11, tag))

#define SCM_MAKE_HEAP_TYPE(x)   (((x) << 6) + scm_tc6_misc_heap)

/* These type codes form part of the ABI and cannot be changed in a
   stable series.  If you do change them in a development series,
   change them also in (system vm assembler) and (system base types).
   Bonus points if you change the build to define these tag values
   in only one place!  */

#define scm_tc11_symbol           SCM_MAKE_HEAP_TYPE (0)
#define scm_tc11_variable         SCM_MAKE_HEAP_TYPE (1)
#define scm_tc11_vector           SCM_MAKE_HEAP_TYPE (2)
#define scm_tc11_wvect            SCM_MAKE_HEAP_TYPE (3)
#define scm_tc11_string           SCM_MAKE_HEAP_TYPE (4)
#define scm_tc11_number           SCM_MAKE_HEAP_TYPE (5)
#define scm_tc11_hashtable        SCM_MAKE_HEAP_TYPE (6)
#define scm_tc11_pointer          SCM_MAKE_HEAP_TYPE (7)
#define scm_tc11_fluid            SCM_MAKE_HEAP_TYPE (8)
#define scm_tc11_stringbuf        SCM_MAKE_HEAP_TYPE (9)
#define scm_tc11_dynamic_state    SCM_MAKE_HEAP_TYPE (10)
#define scm_tc11_frame            SCM_MAKE_HEAP_TYPE (11)
#define scm_tc11_keyword          SCM_MAKE_HEAP_TYPE (12)
#define scm_tc11_atomic_box       SCM_MAKE_HEAP_TYPE (13)
#define scm_tc11_syntax           SCM_MAKE_HEAP_TYPE (14)
#define scm_tc11_values           SCM_MAKE_HEAP_TYPE (15)
#define scm_tc11_program          SCM_MAKE_HEAP_TYPE (16)
#define scm_tc11_vm_cont          SCM_MAKE_HEAP_TYPE (17)
#define scm_tc11_bytevector       SCM_MAKE_HEAP_TYPE (18)
#define scm_tc11_weak_set         SCM_MAKE_HEAP_TYPE (19)
#define scm_tc11_weak_table       SCM_MAKE_HEAP_TYPE (20)
#define scm_tc11_array            SCM_MAKE_HEAP_TYPE (21)
#define scm_tc11_bitvector        SCM_MAKE_HEAP_TYPE (22)
#define scm_tc11_port             SCM_MAKE_HEAP_TYPE (23)
#define scm_tc11_unused_24        SCM_MAKE_HEAP_TYPE (24)
#define scm_tc11_unused_25        SCM_MAKE_HEAP_TYPE (25)
#define scm_tc11_unused_26        SCM_MAKE_HEAP_TYPE (26)
#define scm_tc11_unused_27        SCM_MAKE_HEAP_TYPE (27)
#define scm_tc11_unused_28        SCM_MAKE_HEAP_TYPE (28)
#define scm_tc11_unused_29        SCM_MAKE_HEAP_TYPE (29)
#define scm_tc11_unused_30        SCM_MAKE_HEAP_TYPE (30)
#define scm_tc11_unused_31        SCM_MAKE_HEAP_TYPE (31)


/* Definitions for tc16: */
#define SCM_TYP16(x) 		(0xffff & SCM_CELL_TYPE (x))
#define SCM_HAS_TYP16(x, tag)   (SCM_HAS_HEAP_TYPE (x, SCM_TYP16, tag))
#define SCM_TYP16_PREDICATE(tag, x) (SCM_HAS_TYP16 (x, tag))




/* Immediate values (besides fixnums).  */

enum scm_tc8_tags
{
  scm_tc8_flag = scm_tc3_imm24 + 0x00,  /* special objects ('flags') */
  scm_tc8_char = scm_tc3_imm24 + 0x10,  /* characters */
  scm_tc8_unused_0 = scm_tc3_imm24 + 0x20,
  scm_tc8_unused_1 = scm_tc3_imm24 + 0x30
};

#define SCM_ITAG8(X)		(SCM_UNPACK (X) & 0xff)
#define SCM_MAKE_ITAG8_BITS(X, TAG) (((X) << 8) + TAG)
#define SCM_MAKE_ITAG8(X, TAG)	(SCM_PACK (SCM_MAKE_ITAG8_BITS (X, TAG)))
#define SCM_ITAG8_DATA(X)	(SCM_UNPACK (X) >> 8)



/* Flags (special objects).  The indices of the flags must agree with
   the declarations in print.c: iflagnames.  */

#define SCM_IFLAGP(n)    (SCM_ITAG8 (n) == scm_tc8_flag)
#define SCM_MAKIFLAG_BITS(n)  (SCM_MAKE_ITAG8_BITS ((n), scm_tc8_flag))
#define SCM_IFLAGNUM(n)  (SCM_ITAG8_DATA (n))

/*
   IMPORTANT NOTE regarding IFLAG numbering!!!

   Several macros depend upon careful IFLAG numbering of SCM_BOOL_F,
   SCM_BOOL_T, SCM_ELISP_NIL, SCM_EOL, and the two SCM_XXX_*_DONT_USE
   constants.  In particular:

   - SCM_BOOL_F and SCM_BOOL_T must differ in exactly one bit position.
     (used to implement scm_is_bool_and_not_nil, aka scm_is_bool)

   - SCM_ELISP_NIL and SCM_BOOL_F must differ in exactly one bit
     position.  (used to implement scm_is_false_or_nil and
     scm_is_true_and_not_nil)

   - SCM_ELISP_NIL and SCM_EOL must differ in exactly one bit position.
     (used to implement scm_is_null_or_nil)

   - SCM_ELISP_NIL, SCM_BOOL_F, SCM_EOL,
     SCM_XXX_ANOTHER_LISP_FALSE_DONT_USE must all be equal except for
     two bit positions.  (used to implement scm_is_lisp_false)

   - SCM_ELISP_NIL, SCM_BOOL_F, SCM_BOOL_T,
     SCM_XXX_ANOTHER_BOOLEAN_DONT_USE_0 must all be equal except for two
     bit positions.  (used to implement scm_is_bool_or_nil)

   These properties allow the aforementioned macros to be implemented by
   bitwise ANDing with a mask and then comparing with a constant, using
   as a common basis the macro SCM_MATCHES_BITS_IN_COMMON, defined
   below.  The properties are checked at compile-time using `verify'
   macros near the top of boolean.c and pairs.c.  */
#define SCM_BOOL_F_BITS		SCM_MAKIFLAG_BITS (0)
#define SCM_ELISP_NIL_BITS	SCM_MAKIFLAG_BITS (1)

#define SCM_BOOL_F		SCM_PACK (SCM_BOOL_F_BITS)
#define SCM_ELISP_NIL		SCM_PACK (SCM_ELISP_NIL_BITS)

#ifdef BUILDING_LIBGUILE
#define SCM_XXX_ANOTHER_LISP_FALSE_DONT_USE	SCM_MAKIFLAG_BITS (2)
#endif

#define SCM_EOL_BITS		SCM_MAKIFLAG_BITS (3)
#define SCM_BOOL_T_BITS 	SCM_MAKIFLAG_BITS (4)

#define SCM_EOL			SCM_PACK (SCM_EOL_BITS)
#define SCM_BOOL_T 		SCM_PACK (SCM_BOOL_T_BITS)

#ifdef BUILDING_LIBGUILE
#define SCM_XXX_ANOTHER_BOOLEAN_DONT_USE_0	SCM_MAKIFLAG_BITS (5)
#define SCM_XXX_ANOTHER_BOOLEAN_DONT_USE_1	SCM_MAKIFLAG_BITS (6)
#define SCM_XXX_ANOTHER_BOOLEAN_DONT_USE_2	SCM_MAKIFLAG_BITS (7)
#endif

#define SCM_UNSPECIFIED_BITS	SCM_MAKIFLAG_BITS (8)
#define SCM_UNDEFINED_BITS	SCM_MAKIFLAG_BITS (9)
#define SCM_EOF_VAL_BITS 	SCM_MAKIFLAG_BITS (10)

#define SCM_UNSPECIFIED		SCM_PACK (SCM_UNSPECIFIED_BITS)
#define SCM_UNDEFINED	 	SCM_PACK (SCM_UNDEFINED_BITS)
#define SCM_EOF_VAL 		SCM_PACK (SCM_EOF_VAL_BITS)

#define SCM_UNBNDP(x)		(scm_is_eq ((x), SCM_UNDEFINED))

/* SCM_MATCHES_BITS_IN_COMMON(x,a,b) returns 1 if and only if x matches
   both a and b in every bit position where a and b are equal; otherwise
   it returns 0.  Bit positions where a and b differ are ignored.

   This is used to efficiently compare against two values which differ
   in exactly one bit position, or against four values which differ in
   exactly two bit positions.  It is the basis for the following macros:

     scm_is_null_or_nil,
     scm_is_false_or_nil,
     scm_is_true_and_not_nil,
     scm_is_lisp_false,
     scm_is_lisp_true,
     scm_is_bool_and_not_nil (aka scm_is_bool)
     scm_is_bool_or_nil.  */
#define SCM_MATCHES_BITS_IN_COMMON(x,a,b)				\
  ((SCM_UNPACK(x) & ~(SCM_UNPACK(a) ^ SCM_UNPACK(b))) ==		\
   (SCM_UNPACK(a) & SCM_UNPACK(b)))

/* These macros are used for compile-time verification that the
   constants have the properties needed for the above macro to work
   properly.  */
#ifdef BUILDING_LIBGUILE
#define SCM_WITH_LEAST_SIGNIFICANT_1_BIT_CLEARED(x)  ((x) & ((x)-1))
#define SCM_HAS_EXACTLY_ONE_BIT_SET(x)					\
  ((x) != 0 && SCM_WITH_LEAST_SIGNIFICANT_1_BIT_CLEARED (x) == 0)
#define SCM_HAS_EXACTLY_TWO_BITS_SET(x)					\
  (SCM_HAS_EXACTLY_ONE_BIT_SET (SCM_WITH_LEAST_SIGNIFICANT_1_BIT_CLEARED (x)))

#define SCM_BITS_DIFFER_IN_EXACTLY_ONE_BIT_POSITION(a,b)		\
  (SCM_HAS_EXACTLY_ONE_BIT_SET ((a) ^ (b)))
#define SCM_BITS_DIFFER_IN_EXACTLY_TWO_BIT_POSITIONS(a,b,c,d)		\
  (SCM_HAS_EXACTLY_TWO_BITS_SET (((a) ^ (b)) |                          \
                                 ((b) ^ (c)) |                          \
                                 ((c) ^ (d))))
#endif /* BUILDING_LIBGUILE */




/* Dispatching aids:

   When switching on SCM_TYP11 of a SCM value, use these fake case
   labels to catch types that use fewer than 11 bits for tagging.  */

/* Smobs.  */
#define scm_tcs_smob \
       scm_tc7_smob + 0x000:  case scm_tc7_smob + 0x080:\
  case scm_tc7_smob + 0x100:  case scm_tc7_smob + 0x180:\
  case scm_tc7_smob + 0x200:  case scm_tc7_smob + 0x280:\
  case scm_tc7_smob + 0x300:  case scm_tc7_smob + 0x380:\
  case scm_tc7_smob + 0x400:  case scm_tc7_smob + 0x480:\
  case scm_tc7_smob + 0x500:  case scm_tc7_smob + 0x580:\
  case scm_tc7_smob + 0x600:  case scm_tc7_smob + 0x680:\
  case scm_tc7_smob + 0x700:  case scm_tc7_smob + 0x780

/* Structs.  */
#define scm_tcs_struct \
       scm_tc5_struct + 0x00:   case scm_tc5_struct + 0x20:\
  case scm_tc5_struct + 0x40:   case scm_tc5_struct + 0x60:\
  case scm_tc5_struct + 0x80:   case scm_tc5_struct + 0xa0:\
  case scm_tc5_struct + 0xc0:   case scm_tc5_struct + 0xe0:\
  case scm_tc5_struct + 0x100:  case scm_tc5_struct + 0x120:\
  case scm_tc5_struct + 0x140:  case scm_tc5_struct + 0x160:\
  case scm_tc5_struct + 0x180:  case scm_tc5_struct + 0x1a0:\
  case scm_tc5_struct + 0x1c0:  case scm_tc5_struct + 0x1e0:\
  case scm_tc5_struct + 0x200:  case scm_tc5_struct + 0x220:\
  case scm_tc5_struct + 0x240:  case scm_tc5_struct + 0x260:\
  case scm_tc5_struct + 0x280:  case scm_tc5_struct + 0x2a0:\
  case scm_tc5_struct + 0x2c0:  case scm_tc5_struct + 0x2e0:\
  case scm_tc5_struct + 0x300:  case scm_tc5_struct + 0x320:\
  case scm_tc5_struct + 0x340:  case scm_tc5_struct + 0x360:\
  case scm_tc5_struct + 0x380:  case scm_tc5_struct + 0x3a0:\
  case scm_tc5_struct + 0x3c0:  case scm_tc5_struct + 0x3e0:\
  case scm_tc5_struct + 0x400:  case scm_tc5_struct + 0x420:\
  case scm_tc5_struct + 0x440:  case scm_tc5_struct + 0x460:\
  case scm_tc5_struct + 0x480:  case scm_tc5_struct + 0x4a0:\
  case scm_tc5_struct + 0x4c0:  case scm_tc5_struct + 0x4e0:\
  case scm_tc5_struct + 0x500:  case scm_tc5_struct + 0x520:\
  case scm_tc5_struct + 0x540:  case scm_tc5_struct + 0x560:\
  case scm_tc5_struct + 0x580:  case scm_tc5_struct + 0x5a0:\
  case scm_tc5_struct + 0x5c0:  case scm_tc5_struct + 0x5e0:\
  case scm_tc5_struct + 0x600:  case scm_tc5_struct + 0x620:\
  case scm_tc5_struct + 0x640:  case scm_tc5_struct + 0x660:\
  case scm_tc5_struct + 0x680:  case scm_tc5_struct + 0x6a0:\
  case scm_tc5_struct + 0x6c0:  case scm_tc5_struct + 0x6e0:\
  case scm_tc5_struct + 0x700:  case scm_tc5_struct + 0x720:\
  case scm_tc5_struct + 0x740:  case scm_tc5_struct + 0x760:\
  case scm_tc5_struct + 0x780:  case scm_tc5_struct + 0x7a0:\
  case scm_tc5_struct + 0x7c0:  case scm_tc5_struct + 0x7e0




/* If SCM_ENABLE_DEPRECATED is set to 1, deprecated code will be
   included in Guile, as well as some functions to issue run-time
   warnings about uses of deprecated functions.  */
#ifndef SCM_ENABLE_DEPRECATED
#define SCM_ENABLE_DEPRECATED 0
#endif



/* SCM_API is a macro prepended to all function and data definitions
   which should be exported from libguile. */
#if defined BUILDING_LIBGUILE && defined HAVE_VISIBILITY
# define SCM_API extern __attribute__((__visibility__("default")))
#elif defined BUILDING_LIBGUILE && defined _MSC_VER
# define SCM_API __declspec(dllexport) extern
#elif defined _MSC_VER
# define SCM_API __declspec(dllimport) extern
#else
# define SCM_API extern
#endif

/* The SCM_INTERNAL macro makes it possible to explicitly declare a
   function as having "internal" linkage.  However our current tack on
   this problem is to use GCC 4's -fvisibility=hidden, making functions
   internal by default, and then SCM_API marks them for export.  */
#define SCM_INTERNAL  extern

/* The SCM_DEPRECATED macro is used in declarations of deprecated
   functions or variables.  Defining `SCM_BUILDING_DEPRECATED_CODE'
   allows deprecated functions to be implemented in terms of deprecated
   functions, and allows deprecated functions to be referred to by
   `scm_c_define_gsubr ()'.  */
#if !defined (SCM_BUILDING_DEPRECATED_CODE) && defined __GNUC__
# define SCM_DEPRECATED  SCM_API __attribute__ ((__deprecated__))
#else
# define SCM_DEPRECATED  SCM_API
#endif

/* The SCM_NORETURN macro indicates that a function will never return.
   Examples:
     1) int foo (char arg) SCM_NORETURN;  */
#ifdef __GNUC__
# define SCM_NORETURN __attribute__ ((__noreturn__))
#else
# define SCM_NORETURN
#endif

/* The SCM_UNUSED macro indicates that a function, function argument or
   variable may potentially be unused.
   Examples:
     1) static int unused_function (char arg) SCM_UNUSED;
     2) int foo (char unused_argument SCM_UNUSED);
     3) int unused_variable SCM_UNUSED;  */
#ifdef __GNUC__
# define SCM_UNUSED __attribute__ ((unused))
#else
# define SCM_UNUSED
#endif

/* The SCM_MALLOC macro can be used in function declarations to tell the
   compiler that a function may be treated as if any non-NULL pointer it
   returns cannot alias any other pointer valid when the function
   returns.  */
#ifdef __GNUC__
# define SCM_MALLOC  __attribute__ ((__malloc__))
#else
# define SCM_MALLOC
#endif

/* The SCM_EXPECT macros provide branch prediction hints to the
   compiler.  To use only in places where the result of the expression
   under "normal" circumstances is known.  */
#ifdef __GNUC__
# define SCM_EXPECT    __builtin_expect
#else
# define SCM_EXPECT(_expr, _value) (_expr)
#endif

#define SCM_LIKELY(_expr)    SCM_EXPECT ((_expr), 1)
#define SCM_UNLIKELY(_expr)  SCM_EXPECT ((_expr), 0)

/* The SCM_ALIGNED macro, when defined, can be used to instruct the
   compiler to honor the given alignment constraint.  Sun Studio
   supports alignment since Sun Studio 12.  */
#if defined __GNUC__ || (defined( __SUNPRO_C ) && (__SUNPRO_C - 0 >= 0x590))
# define SCM_ALIGNED(x)  __attribute__ ((aligned (x)))
#elif defined __INTEL_COMPILER
# define SCM_ALIGNED(x)  __declspec (align (x))
#else
# undef SCM_ALIGNED
#endif

/* Thread-local storage (TLS).  */
#ifdef SCM_HAVE_THREAD_STORAGE_CLASS
# define SCM_THREAD_LOCAL __thread
#else
# define SCM_THREAD_LOCAL
#endif




/* The type of subrs, i.e., Scheme procedures implemented in C.  Empty
   function declarators are used internally for pointers to functions of
   any arity.  However, these are equivalent to `(void)' in C++, are
   obsolescent as of C99, and trigger `strict-prototypes' GCC warnings
   (bug #23681).  */
#ifdef BUILDING_LIBGUILE
typedef SCM (* scm_t_subr) ();
#else
typedef void *scm_t_subr;
#endif

typedef struct scm_dynamic_state scm_t_dynamic_state;
typedef struct scm_print_state scm_print_state;
typedef struct scm_dynstack scm_t_dynstack;
typedef int32_t scm_t_wchar;
struct scm_frame;
struct scm_vm;
union scm_vm_stack_element;
typedef struct scm_thread scm_thread;



#ifdef CHAR_BIT
# define SCM_CHAR_BIT CHAR_BIT
#else
# define SCM_CHAR_BIT 8
#endif

#ifdef LONG_BIT
# define SCM_LONG_BIT LONG_BIT
#else
# define SCM_LONG_BIT (SCM_SIZEOF_LONG * 8)
#endif



/* Cast pointer through (void *) in order to avoid compiler warnings
   when strict aliasing is enabled */
typedef long SCM_STACKITEM;
#define SCM_STACK_PTR(ptr) ((SCM_STACKITEM *) (void *) (ptr))


#endif  /* SCM_SCM_H */
