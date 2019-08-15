/* 
   ARBEL is a REGISTER BASED ENVIRONMENT AND LANGUAGE
   Copyright 2019 Zach Flynn

   This file is part of ARBEL.

   ARBEL is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   ARBLE is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with ARBEL (in COPYING file).  If not, see <https://www.gnu.org/licenses/>.
   
*/


#ifndef REGULAR_H
#define REGULAR_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <readline/readline.h>
#include <readline/history.h>
#include <math.h>

enum data_type
  {
   INTEGER,
   DECIMAL,
   STRING,
   REGISTER,
   REGISTRY,
   INSTRUCTION,
   ACTIVE_INSTRUCTION,
   OPERATION,
   REFERENCE,
   ARBEL_FILE,
   NOTHING
  };

typedef enum data_type data_type;

struct data
{
  void* data;
  data_type type;
};
typedef struct data data;

struct registry
{
  data* value;
  char* key;
  struct registry* right;
  struct registry* left;
  struct registry* up;
};

typedef struct registry registry;

typedef void (*operation)(registry*);

typedef char* regstr;

struct ref
{
  char* key;
  registry* reg;
};

typedef struct ref ref;


struct command
{
  char* code;
  registry* reg;
};

typedef struct command command;

struct parser_state
{
  char buffer[1024];
  int arg_n; /* argument number */
  int i; /* location in buffer */
  int in_instr; /* whether in command */
  int after_instr; /* whether command just ended. */
  int in_quote; /* whether in a quote */
  int after_quote; /* whether in a quote */
  int in_comment;
  char open_paren;
  int print_out;
};

char*
argument_name (int n);

int
is_integer (const char* str);

void
assign_dec (data** d, const double num);

void
assign_int (data** d, const int num);

void
assign_str (data** d, const char* str);

void
assign_op (data** d, const operation op);

void
assign_registry (data** d, registry* r);

void
assign_regstr (data** d, const char* name);

void
assign_ref (data** d, registry* reg, const char* name);

void
assign_file (data** d, FILE* f);

int
is_numeric (data* d);

void
free_data (data* d);

void
free_registry (registry* reg);

registry*
head (registry* reg);

registry*
tail (registry* reg);

void
set (registry* reg, data* d, const char* name);

data*
get (registry* reg, const char* name, int recursive);

void
mov (registry* reg, const char* name, const char* new_name);

void
del (registry* reg, const char* name, int del_data);

data*
get_data_in_registry (registry* reg, const regstr name);

void
do_error (const char* msg);

void
null_ans (registry* reg);

int
parse (FILE* f, registry* reg, registry** arg_reg,
       struct parser_state* state);

void
print_data (data* d, int print_cmd);

int
is_register (const char* str);

int
is_reference (const char* str);

void
str_shift_left (char* buffer);

void
add_basic_ops (registry* reg);

int
is_whitespace (const char c);

void
print_registry (registry* reg);

char*
append_nl (char* str);

data*
copy_data (data* d_in);

void
shift_arguments (registry* reg);

int
is_error (int e);

int
is_exit (int e);

int
is_init_reg (registry* r);

registry*
new_registry (registry* parent);

void
ret (registry* reg, data* d, const char* name);

void
ret_ans (registry* reg, data* d);

void
relabel (registry* reg, const char* name, const char* new_name);

void
assign_instr (data** d, const char* str);

struct parser_state
fresh_state ();

registry*
copy_registry(registry* r0);

int
is_retval (const int r);

int
is_decimal (const char* str);

void
assign_active (data** d, const char* str);

data*
lookup (registry* reg, const char* name, int recursive);

void
compute (registry* reg);

int
save_registry (FILE* f, registry* reg);

int
read_registry (FILE* f, registry* reg);

void
op_list (registry* reg);

void
op_call (registry* reg);

char*
vector_name (const char* lead, int n);

/* global variables */
data* top_registry;
data* up_registry;
registry* current_parse_registry;
char* source_code;

#endif
