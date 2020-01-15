SUFFIXES = .pshm
.pshm.c:
	cat $< | pushmac > $@

bin_PROGRAMS = arbel
arbel_SOURCES = src/arbel.c src/arg.c src/compute.c src/copy.c src/memory.c src/parse.c src/print.c src/register.c src/registry.c src/save.c src/state.c src/statement.c src/type.c src/utility.c src/data.c

if PUSHMAC
arbel_SOURCES += src/operator.pshm
else
arbel_SOURCES += src/operator.expand.c
endif

man1_MANS = docs/arbel.1

TESTS=test/test_ops.sh
