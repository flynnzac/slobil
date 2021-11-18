ACLOCAL_AMFLAGS=-I m4
SUBDIRS = pushmac

SUFFIXES = .pshm .onbu
.pshm.c:
	cat $< | pushmac/pushmac > $@
# .onbu.log:
# 	./$< > $@


bin_PROGRAMS = onbu
onbu_SOURCES = src/onbu.c
onbu_DEPENDENCIES = libonbu.la
onbu_CFLAGS = $(AM_CFLAGS) -lonbu

man1_MANS = docs/onbu.1

noinst_PROGRAMS = test/unit/test_arg test/unit/test_copy


TESTS = test/ops/op_arithmetic.onbu
TESTS += test/ops/op_answer.onbu
TESTS += test/ops/op_comparison.onbu
TESTS += test/ops/op_delete.onbu
TESTS += test/ops/op_exist.onbu
TESTS += test/ops/op_get.onbu
TESTS += test/ops/op_if.onbu
TESTS += test/ops/op_list.onbu
TESTS += test/ops/op_move.onbu
TESTS += test/ops/op_registry.onbu
TESTS += test/ops/op_set.onbu
TESTS += test/ops/op_string_append.onbu
TESTS += test/ops/op_string_length.onbu
TESTS += test/ops/op_do.onbu
TESTS += test/ops/op_next.onbu
TESTS += test/ops/op_last.onbu
TESTS += test/ops/op_is_registry.onbu
TESTS += test/ops/op_import.onbu
TESTS += test/ops/op_filter.onbu
TESTS += test/ops/op_range.onbu
TESTS += test/ops/op_to_string.onbu
TESTS += test/ops/op_is_integer.onbu
TESTS += test/ops/op_is_real.onbu
TESTS += test/ops/op_log.onbu
TESTS += test/ops/op_exp.onbu
TESTS += test/ops/op_power.onbu
TESTS += test/ops/op_mod.onbu
TESTS += test/ops/op_incr.onbu
TESTS += test/ops/op_floor.onbu
TESTS += test/ops/op_ceiling.onbu
TESTS += test/ops/op_is_boolean.onbu
TESTS += test/ops/op_in.onbu
TESTS += test/ops/op_while.onbu
TESTS += test/ops/op_collapse.onbu
TESTS += test/ops/op_code.onbu
TESTS += test/ops/op_call.onbu
TESTS += test/ops/op_op.onbu
TESTS += test/ops/op_is_instruction.onbu
TESTS += test/ops/op_repeat.onbu
TESTS += test/ops/op_to_register.onbu
TESTS += test/ops/op_register_eq.onbu
TESTS += test/ops/op_is_register.onbu
TESTS += test/ops/op_string_compare.onbu
TESTS += test/unit/test_arg
TESTS += test/unit/test_copy




lib_LTLIBRARIES = libonbu.la
libonbu_la_SOURCES = src/arg.c src/compute.c src/copy.c src/memory.c src/parse.c src/print.c src/register.c src/registry.c src/save.c src/state.c src/statement.c src/type.c src/utility.c src/data.c src/task.c src/conversion.c

libonbu_la_SOURCES += src/operator.pshm

test_unit_test_arg_SOURCES = test/unit/test_arg.c
test_unit_test_arg_DEPENDENCIES = libonbu.la
test_unit_test_arg_CFLAGS = $(AM_CFLAGS) -lcheck_pic -lsubunit -lrt -Isrc -lonbu

test_unit_test_copy_SOURCES = test/unit/test_copy.c
test_unit_test_copy_DEPENDENCIES = libonbu.la
test_unit_test_copy_CFLAGS = $(AM_CFLAGS) -lcheck_pic -lsubunit -lrt -Isrc -lonbu


