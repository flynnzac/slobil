SUFFIXES = .pushmac
.pushmac.c:
	cat $< | pushmac > $@

bin_PROGRAMS = arbel
arbel_SOURCES = src/arbel.c src/arg.c src/compute.c src/copy.c src/memory.c src/parse.c src/print.c src/register.c src/registry.c src/save.c src/state.c src/statement.c src/type.c src/utility.c src/data.c src/operator.pushmac	

