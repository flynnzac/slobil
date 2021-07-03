ACLOCAL_AMFLAGS=-I m4

SUFFIXES = .pshm .arb
.pshm.c:
	cat $< | pushmac > $@
# .arb.log:
# 	./$< > $@


bin_PROGRAMS = wob
wob_SOURCES = src/wob.c src/arg.c src/compute.c src/copy.c src/memory.c src/parse.c src/print.c src/register.c src/registry.c src/save.c src/state.c src/statement.c src/type.c src/utility.c src/data.c src/task.c
wob_CFLAGS = $(AM_CFLAGS)

if PUSHMAC
wob_SOURCES += src/operator.pshm
else
wob_SOURCES += src/operator.expand.c
endif

man1_MANS = docs/wob.1

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


lib_LTLIBRARIES = libwob.la
libwob_la_SOURCES = src/arg.c src/compute.c src/copy.c src/memory.c src/parse.c src/print.c src/register.c src/registry.c src/save.c src/state.c src/statement.c src/type.c src/utility.c src/data.c src/task.c

if PUSHMAC
libwob_la_SOURCES += src/operator.pshm
else
libwob_la_SOURCES += src/operator.expand.c
endif


