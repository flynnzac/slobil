SUFFIXES = .pshm .arb
.pshm.c:
	cat $< | pushmac > $@
# .arb.log:
# 	./$< > $@


bin_PROGRAMS = arbel
arbel_SOURCES = src/arbel.c src/arg.c src/compute.c src/copy.c src/memory.c src/parse.c src/print.c src/register.c src/registry.c src/save.c src/state.c src/statement.c src/type.c src/utility.c src/data.c

if PUSHMAC
arbel_SOURCES += src/operator.pshm
else
arbel_SOURCES += src/operator.expand.c
endif

man1_MANS = docs/arbel.1

TESTS = test/built_ins/op_arithmetic.arb
TESTS += test/built_ins/op_answer.arb
TESTS += test/built_ins/op_comparison.arb
TESTS += test/built_ins/op_delete.arb
TESTS += test/built_ins/op_exist.arb
TESTS += test/built_ins/op_get.arb
TESTS += test/built_ins/op_if.arb
TESTS += test/built_ins/op_list.arb
TESTS += test/built_ins/op_move.arb
TESTS += test/built_ins/op_registry.arb
TESTS += test/built_ins/op_set.arb
TESTS += test/built_ins/op_string_append.arb
TESTS += test/built_ins/op_string_length.arb


