ACLOCAL_AMFLAGS=-I m4
SUBDIRS = pushmac

SUFFIXES = .pshm .wob
.pshm.c:
	cat $< | pushmac/pushmac > $@
# .wob.log:
# 	./$< > $@


bin_PROGRAMS = wob
wob_SOURCES = src/wob.c
wob_DEPENDENCIES = libwob.la
wob_CFLAGS = $(AM_CFLAGS) -L. -lwob

man1_MANS = docs/wob.1

noinst_PROGRAMS = test/unit/test_arg test/unit/test_copy


TESTS = test/ops/op_arithmetic.wob
TESTS += test/ops/op_answer.wob
TESTS += test/ops/op_comparison.wob
TESTS += test/ops/op_delete.wob
TESTS += test/ops/op_exist.wob
TESTS += test/ops/op_get.wob
TESTS += test/ops/op_if.wob
TESTS += test/ops/op_list.wob
TESTS += test/ops/op_move.wob
TESTS += test/ops/op_registry.wob
TESTS += test/ops/op_set.wob
TESTS += test/ops/op_string_append.wob
TESTS += test/ops/op_string_length.wob
TESTS += test/ops/op_do.wob
TESTS += test/ops/op_next.wob
TESTS += test/ops/op_last.wob
TESTS += test/ops/op_is_registry.wob
TESTS += test/ops/op_import.wob
TESTS += test/ops/op_filter.wob
TESTS += test/ops/op_range.wob
TESTS += test/ops/op_to_string.wob
TESTS += test/ops/op_is_integer.wob
TESTS += test/ops/op_is_real.wob
TESTS += test/ops/op_log.wob
TESTS += test/ops/op_exp.wob
TESTS += test/ops/op_power.wob
TESTS += test/ops/op_mod.wob
TESTS += test/ops/op_incr.wob
TESTS += test/ops/op_floor.wob
TESTS += test/ops/op_ceiling.wob
TESTS += test/ops/op_is_boolean.wob
TESTS += test/ops/op_in.wob
TESTS += test/ops/op_while.wob
TESTS += test/ops/op_collapse.wob
TESTS += test/unit/test_arg
TESTS += test/unit/test_copy




lib_LTLIBRARIES = libwob.la
libwob_la_SOURCES = src/arg.c src/compute.c src/copy.c src/memory.c src/parse.c src/print.c src/register.c src/registry.c src/save.c src/state.c src/statement.c src/type.c src/utility.c src/data.c src/task.c src/conversion.c

libwob_la_SOURCES += src/operator.pshm

test_unit_test_arg_SOURCES = test/unit/test_arg.c
test_unit_test_arg_DEPENDENCIES = libwob.la
test_unit_test_arg_CFLAGS = $(AM_CFLAGS) -lcheck_pic -lsubunit -lrt -Isrc -lwob

test_unit_test_copy_SOURCES = test/unit/test_copy.c
test_unit_test_copy_DEPENDENCIES = libwob.la
test_unit_test_copy_CFLAGS = $(AM_CFLAGS) -lcheck_pic -lsubunit -lrt -Isrc -lwob


