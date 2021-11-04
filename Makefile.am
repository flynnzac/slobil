ACLOCAL_AMFLAGS=-I m4
SUBDIRS = pushmac

SUFFIXES = .pshm .brpl
.pshm.c:
	cat $< | pushmac/pushmac > $@
# .brpl.log:
# 	./$< > $@


bin_PROGRAMS = briple
briple_SOURCES = src/briple.c
briple_DEPENDENCIES = libbriple.la
briple_CFLAGS = $(AM_CFLAGS) -L. -lbriple

man1_MANS = docs/briple.1

noinst_PROGRAMS = test/unit/test_arg test/unit/test_copy


TESTS = test/ops/op_arithmetic.brpl
TESTS += test/ops/op_answer.brpl
TESTS += test/ops/op_comparison.brpl
TESTS += test/ops/op_delete.brpl
TESTS += test/ops/op_exist.brpl
TESTS += test/ops/op_get.brpl
TESTS += test/ops/op_if.brpl
TESTS += test/ops/op_list.brpl
TESTS += test/ops/op_move.brpl
TESTS += test/ops/op_registry.brpl
TESTS += test/ops/op_set.brpl
TESTS += test/ops/op_string_append.brpl
TESTS += test/ops/op_string_length.brpl
TESTS += test/ops/op_do.brpl
TESTS += test/ops/op_next.brpl
TESTS += test/ops/op_last.brpl
TESTS += test/ops/op_is_registry.brpl
TESTS += test/ops/op_import.brpl
TESTS += test/ops/op_filter.brpl
TESTS += test/ops/op_range.brpl
TESTS += test/ops/op_to_string.brpl
TESTS += test/ops/op_is_integer.brpl
TESTS += test/ops/op_is_real.brpl
TESTS += test/ops/op_log.brpl
TESTS += test/ops/op_exp.brpl
TESTS += test/ops/op_power.brpl
TESTS += test/ops/op_mod.brpl
TESTS += test/ops/op_incr.brpl
TESTS += test/ops/op_floor.brpl
TESTS += test/ops/op_ceiling.brpl
TESTS += test/ops/op_is_boolean.brpl
TESTS += test/ops/op_in.brpl
TESTS += test/ops/op_while.brpl
TESTS += test/ops/op_collapse.brpl
TESTS += test/ops/op_code.brpl
TESTS += test/ops/op_call.brpl
TESTS += test/ops/op_op.brpl
TESTS += test/ops/op_is_instruction.brpl
TESTS += test/ops/op_repeat.brpl
TESTS += test/ops/op_to_register.brpl
TESTS += test/ops/op_register_eq.brpl
TESTS += test/ops/op_is_register.brpl
TESTS += test/ops/op_string_compare.brpl
TESTS += test/unit/test_arg
TESTS += test/unit/test_copy




lib_LTLIBRARIES = libbriple.la
libbriple_la_SOURCES = src/arg.c src/compute.c src/copy.c src/memory.c src/parse.c src/print.c src/register.c src/registry.c src/save.c src/state.c src/statement.c src/type.c src/utility.c src/data.c src/task.c src/conversion.c

libbriple_la_SOURCES += src/operator.pshm

test_unit_test_arg_SOURCES = test/unit/test_arg.c
test_unit_test_arg_DEPENDENCIES = libbriple.la
test_unit_test_arg_CFLAGS = $(AM_CFLAGS) -lcheck_pic -lsubunit -lrt -Isrc -lbriple

test_unit_test_copy_SOURCES = test/unit/test_copy.c
test_unit_test_copy_DEPENDENCIES = libbriple.la
test_unit_test_copy_CFLAGS = $(AM_CFLAGS) -lcheck_pic -lsubunit -lrt -Isrc -lbriple


