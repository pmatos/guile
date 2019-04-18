#include "test.h"

static void
run_test(jit_state_t *j, uint8_t *arena_base, size_t arena_size)
{
  jit_begin(j, arena_base, arena_size);
  jit_load_args_1(j, jit_operand_gpr (JIT_OPERAND_ABI_INTMAX, JIT_R1));

  jit_extr_c(j, JIT_R0, JIT_R1);
  jit_retr(j, JIT_R0);

  uintmax_t (*f)(uintmax_t) = jit_end(j, NULL);

  ASSERT(f(0) == 0);
  ASSERT(f(1) == 1);
  ASSERT(f(0xf) == 0xf);
  ASSERT(f(0xff) == -1);
  ASSERT(f(0xfff) == -1);
}

int
main (int argc, char *argv[])
{
  return main_helper(argc, argv, run_test);
}
