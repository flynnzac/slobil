#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT([WOB], [0.1.0-rc1], [github.com/flynnzac/wob])
AC_CONFIG_SRCDIR([src/wob.c])
AC_CONFIG_MACRO_DIRS([m4])
AC_CONFIG_SUBDIRS([pushmac])

LT_INIT

# Checks for programs.
AC_PROG_CC
AC_PROG_INSTALL
AC_PROG_MAKE_SET

# Check if we are using the garbage collector or not
AC_ARG_ENABLE([greedy-gc], AS_HELP_STRING([--enable-greedy-gc], [Disable automatic garbage with libgc and instead use greedy malloc-free pairs]))
AS_IF([test "x$enable_greedy_gc" = "xyes"], [],[
	    AC_CHECK_LIB([gc], [GC_malloc])
	    AC_DEFINE([GARBAGE])
	    ])

# Checks for libraries.


AC_CHECK_LIB([dl], [dlopen])
AC_CHECK_LIB([pthread], [pthread_create])
AC_CHECK_LIB([m], [floor])
AC_CHECK_LIB([readline], [readline])
AC_CHECK_LIB([unistring], [u8_next])
AC_CHECK_LIB([gmp], [__gmpz_init])
AC_CHECK_LIB([z], [gzopen])

# Checks for header files.
AC_CHECK_HEADERS([stdlib.h string.h unistd.h])


# Checks for typedefs, structures, and compiler characteristics.
AC_CHECK_HEADER_STDBOOL
AC_TYPE_SIZE_T
AC_TYPE_SSIZE_T

# Checks for library functions.
AC_FUNC_MALLOC
AC_FUNC_REALLOC
AC_CHECK_FUNCS([floor pow regcomp])

AM_INIT_AUTOMAKE([foreign subdir-objects])
AC_CONFIG_FILES([Makefile])


AC_OUTPUT
