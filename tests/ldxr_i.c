#include "test.h"

static uint32_t data[] = { 0xffffffff, 0x00000000, 0x42424242 };

static void
run_test(jit_state_t *j, uint8_t *arena_base, size_t arena_size)
{
  jit_begin(j, arena_base, arena_size);
  jit_load_args_2(j, jit_operand_gpr (JIT_OPERAND_ABI_POINTER, JIT_R0),
                  jit_operand_gpr (JIT_OPERAND_ABI_INTMAX, JIT_R1));

  jit_ldxr_i(j, JIT_R0, JIT_R0, JIT_R1);
  jit_retr(j, JIT_R0);

  uintmax_t (*f)(void*, uintmax_t) = jit_end(j, NULL);

  ASSERT(f(data, 0) == -1);
  ASSERT(f(data, 4) == 0);
  ASSERT(f(data, 8) == 0x42424242);
}

int
main (int argc, char *argv[])
{
  return main_helper(argc, argv, run_test);
}
