ACLOCAL_AMFLAGS=-I m4
SUBDIRS = pushmac

SUFFIXES = .pshm .arb
.pshm.c:
	cat $< | pushmac/pushmac > $@
# .arbel.log:
# 	./$< > $@


bin_PROGRAMS = slobil
slobil_SOURCES = src/slobil.c
slobil_DEPENDENCIES = libslobil.la
slobil_CFLAGS = $(AM_CFLAGS) -lslobil

man1_MANS = docs/slobil.1

noinst_PROGRAMS = test/unit/test_arg test/unit/test_copy


TESTS = test/ops/op_arithmetic.slob
TESTS += test/ops/op_answer.slob
TESTS += test/ops/op_comparison.slob
TESTS += test/ops/op_delete.slob
TESTS += test/ops/op_exist.slob
TESTS += test/ops/op_get.slob
TESTS += test/ops/op_if.slob
TESTS += test/ops/op_list.slob
TESTS += test/ops/op_move.slob
TESTS += test/ops/op_registry.slob
TESTS += test/ops/op_set.slob
TESTS += test/ops/op_combine.slob
TESTS += test/ops/op_length.slob
TESTS += test/ops/op_do.slob
TESTS += test/ops/op_next.slob
TESTS += test/ops/op_last.slob
TESTS += test/ops/op_is_registry.slob
TESTS += test/ops/op_import.slob
TESTS += test/ops/op_filter.slob
TESTS += test/ops/op_range.slob
TESTS += test/ops/op_to_string.slob
TESTS += test/ops/op_is_integer.slob
TESTS += test/ops/op_is_real.slob
TESTS += test/ops/op_log.slob
TESTS += test/ops/op_exp.slob
TESTS += test/ops/op_power.slob
TESTS += test/ops/op_mod.slob
TESTS += test/ops/op_incr.slob
TESTS += test/ops/op_floor.slob
TESTS += test/ops/op_ceiling.slob
TESTS += test/ops/op_is_boolean.slob
TESTS += test/ops/op_in.slob
TESTS += test/ops/op_while.slob
TESTS += test/ops/op_collapse.slob
TESTS += test/ops/op_code.slob
TESTS += test/ops/op_call.slob
TESTS += test/ops/op_op.slob
TESTS += test/ops/op_is_instruction.slob
TESTS += test/ops/op_repeat.slob
TESTS += test/ops/op_to_slot.slob
TESTS += test/ops/op_slot_eq.slob
TESTS += test/ops/op_is_slot.slob
TESTS += test/ops/op_string_compare.slob
TESTS += test/unit/test_arg
TESTS += test/unit/test_copy




lib_LTLIBRARIES = libslobil.la
libslobil_la_SOURCES = src/arg.c src/compute.c src/copy.c src/memory.c src/parse.c src/print.c src/slot.c src/object.c src/save.c src/state.c src/statement.c src/type.c src/utility.c src/data.c src/task.c src/conversion.c

libslobil_la_SOURCES += src/operator.pshm

test_unit_test_arg_SOURCES = test/unit/test_arg.c
test_unit_test_arg_DEPENDENCIES = libslobil.la
test_unit_test_arg_CFLAGS = $(AM_CFLAGS) -lcheck_pic -lsubunit -lrt -Isrc -lslobil

test_unit_test_copy_SOURCES = test/unit/test_copy.c
test_unit_test_copy_DEPENDENCIES = libslobil.la
test_unit_test_copy_CFLAGS = $(AM_CFLAGS) -lcheck_pic -lsubunit -lrt -Isrc -lslobil


