ACLOCAL_AMFLAGS=-I m4
SUBDIRS = pushmac

SUFFIXES = .pshm .wob
.pshm.c:
	cat $< | pushmac/pushmac > $@
# .wob.log:
# 	./$< > $@


bin_PROGRAMS = wob
wob_SOURCES = src/wob.c src/arg.c src/compute.c src/copy.c src/memory.c src/parse.c src/print.c src/register.c src/registry.c src/save.c src/state.c src/statement.c src/type.c src/utility.c src/data.c src/task.c
wob_CFLAGS = $(AM_CFLAGS)

wob_SOURCES += src/operator.pshm

man1_MANS = docs/wob.1

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


lib_LTLIBRARIES = libwob.la
libwob_la_SOURCES = src/arg.c src/compute.c src/copy.c src/memory.c src/parse.c src/print.c src/register.c src/registry.c src/save.c src/state.c src/statement.c src/type.c src/utility.c src/data.c src/task.c

libwob_la_SOURCES += src/operator.pshm


