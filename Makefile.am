ACLOCAL_AMFLAGS=-I m4
SUBDIRS = pushmac

SUFFIXES = .pshm .arb
.pshm.c:
	cat $< | pushmac/pushmac > $@
# .arbel.log:
# 	./$< > $@


bin_PROGRAMS = arbel
arbel_SOURCES = src/arbel.c
arbel_DEPENDENCIES = libarbel.la
arbel_CFLAGS = $(AM_CFLAGS) -larbel

man1_MANS = docs/arbel.1

noinst_PROGRAMS = test/unit/test_arg test/unit/test_copy


TESTS = test/ops/op_arithmetic.arb
TESTS += test/ops/op_answer.arb
TESTS += test/ops/op_comparison.arb
TESTS += test/ops/op_delete.arb
TESTS += test/ops/op_exist.arb
TESTS += test/ops/op_get.arb
TESTS += test/ops/op_if.arb
TESTS += test/ops/op_list.arb
TESTS += test/ops/op_move.arb
TESTS += test/ops/op_registry.arb
TESTS += test/ops/op_set.arb
TESTS += test/ops/op_string_append.arb
TESTS += test/ops/op_string_length.arb
TESTS += test/ops/op_do.arb
TESTS += test/ops/op_next.arb
TESTS += test/ops/op_last.arb
TESTS += test/ops/op_is_registry.arb
TESTS += test/ops/op_import.arb
TESTS += test/ops/op_filter.arb
TESTS += test/ops/op_range.arb
TESTS += test/ops/op_to_string.arb
TESTS += test/ops/op_is_integer.arb
TESTS += test/ops/op_is_real.arb
TESTS += test/ops/op_log.arb
TESTS += test/ops/op_exp.arb
TESTS += test/ops/op_power.arb
TESTS += test/ops/op_mod.arb
TESTS += test/ops/op_incr.arb
TESTS += test/ops/op_floor.arb
TESTS += test/ops/op_ceiling.arb
TESTS += test/ops/op_is_boolean.arb
TESTS += test/ops/op_in.arb
TESTS += test/ops/op_while.arb
TESTS += test/ops/op_collapse.arb
TESTS += test/ops/op_code.arb
TESTS += test/ops/op_call.arb
TESTS += test/ops/op_op.arb
TESTS += test/ops/op_is_instruction.arb
TESTS += test/ops/op_repeat.arb
TESTS += test/ops/op_to_register.arb
TESTS += test/ops/op_register_eq.arb
TESTS += test/ops/op_is_register.arb
TESTS += test/ops/op_string_compare.arb
TESTS += test/unit/test_arg
TESTS += test/unit/test_copy




lib_LTLIBRARIES = libarbel.la
libarbel_la_SOURCES = src/arg.c src/compute.c src/copy.c src/memory.c src/parse.c src/print.c src/register.c src/registry.c src/save.c src/state.c src/statement.c src/type.c src/utility.c src/data.c src/task.c src/conversion.c

libarbel_la_SOURCES += src/operator.pshm

test_unit_test_arg_SOURCES = test/unit/test_arg.c
test_unit_test_arg_DEPENDENCIES = libarbel.la
test_unit_test_arg_CFLAGS = $(AM_CFLAGS) -lcheck_pic -lsubunit -lrt -Isrc -larbel

test_unit_test_copy_SOURCES = test/unit/test_copy.c
test_unit_test_copy_DEPENDENCIES = libarbel.la
test_unit_test_copy_CFLAGS = $(AM_CFLAGS) -lcheck_pic -lsubunit -lrt -Isrc -larbel


