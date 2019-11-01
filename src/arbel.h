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


#ifndef ARBEL_H
#define ARBEL_H
#define _GNU_SOURCE
#define ARBEL_HASH_SIZE 37
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <readline/readline.h>
#include <readline/history.h>
#include <math.h>
#include <dlfcn.h>
#include <regex.h>
#include <unistd.h>
#include <time.h>

enum data_type
  {
   INTEGER,
   REAL,
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

struct args;

struct data
{
  void* data;
  data_type type;
};
typedef struct data data;


struct content
{
  data* value;
  unsigned long key;
  char* name;
  struct content* right;
  struct content* left;
  int do_not_free_data;
};

typedef struct content content;

struct registry
{
  content* objects[ARBEL_HASH_SIZE];
  struct registry* up;
};

typedef struct registry registry;

typedef void (*operation)(struct args, registry*);

struct regstr
{
  char* name;
  unsigned long key;
};

typedef struct regstr regstr;

struct ref
{
  char** name;
  registry* reg;
  unsigned long* key;
  int levels;
  int* is_regstr;
};

typedef struct ref ref;

struct command
{
  char* code;
  registry* reg;
};

typedef struct command command;

struct statement;
struct element;
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
  struct element* cur_elem;
  struct statement* cur_stmt;
  
};

typedef struct parser_state parser_state;

struct element
{
  data* data;
  char** name;
  struct statement* s;
  int literal;
  int statement;
  unsigned long* hash_name;
  struct element* right;
  int levels;
  int* is_regstr;
};

typedef struct element element;

struct args
{
  data** arg_array;
  int* free_data;
  size_t length;
};

typedef struct args args;

struct statement
{
  element* head;
  args arg;
  struct statement* right;
  registry* arg_reg;
  unsigned long* hash_bins;
  size_t* location;
};

typedef struct statement statement;

struct instruction
{
  statement* stmt;
  char* code;
};

typedef struct instruction instruction;

char*
argument_name (int n);

int
is_integer (const char* str);

void
assign_real (data** d, const double num);

void
assign_int (data** d, const int num);

void
assign_str (data** d, const char* str, int copy);

void
assign_op (data** d, const operation op);

void
assign_registry (data** d, registry* r);

void
assign_regstr (data** d, const char* name, unsigned long key);

void
assign_ref (data** d, registry* reg,
            char** names,
            const unsigned long* keys,
            const int levels,
            const int* is_regstr);

void
assign_nothing (data** d);

void
assign_file (data** d, FILE* f);

int
is_numeric (data* d);

void
free_data (data* d);

void
free_registry (registry* reg);

content*
head (content* reg);

content*
tail (content* reg);

content*
set (registry* reg, data* d, const char* name);

data*
get (registry* reg, unsigned long hash_name, int recursive);

content*
mov (registry* reg, regstr* old, regstr* new);

content*
del (registry* reg, unsigned long hash_name, int del_data);

data*
get_data_in_registry (registry* reg, const regstr name);

void
do_error (const char* msg);

void
null_ans (registry* reg);

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
is_init_reg (content* r);

registry*
new_registry (registry* parent);

void
ret (registry* reg, data* d, const char* name);

void
ret_ans (registry* reg, data* d);

void
relabel (registry* reg, const char* name, const char* new_name);

void
assign_instr (data** d, statement* s, const char* code);

struct parser_state
fresh_state ();

registry*
copy_registry(registry* r0);

int
is_retval (const int r);

int
is_real (const char* str);

void
assign_active (data** d, statement* s);

data*
lookup (registry* reg, unsigned long hash_name, int recursive);

void
compute (data* cmd, registry* reg, args arg);

int
save_registry (FILE* f, registry* reg);

int
read_registry (FILE* f, registry* reg);

void
op_list (registry* reg);

void
op_call (registry* reg);

void
op_add (registry* reg);

char*
vector_name (const char* lead, int n);

void
execute_statement (statement* s, registry* reg);

statement*
append_statement (statement* current, element* head);

element*
append_argument_element (element* current, char** name,
                         unsigned long* hash_name, const int levels,
                         int* is_regstr);

element*
append_literal_element (element* current, data* d);

element*
append_statement_element (element* current, statement* s);

int
parse (FILE* f, parser_state* state, statement** s);

void
execute_code (statement* s, registry* reg);

element*
parse_stmt (FILE* f, parser_state* state, int* complete);

int
interact (FILE* f, parser_state* state, registry* reg);

element*
copy_elements (element* e);

statement*
copy_statement (statement* s);

void
free_statement (statement* s);

char*
escape_str(char* str);

void
mark_do_not_free (registry* reg, unsigned long hash_name);

unsigned long
hash_str(const char *str);

char**
split_by_colon (const char* name, int* cnt, int** is_regstr);

char**
copy_names (char** name, int levels);

unsigned long*
copy_hashes (unsigned long* hashes, int levels);

int*
copy_isregstr (int* is_regstr, int levels);

data*
get_by_levels (registry* reg, unsigned long* hash_name, int levels, int* is_regstr, char** name);

const char*
str_type (data_type type);

registry*
shift_list_down (registry* reg);

int
save_content (FILE* f, content* reg);

content*
new_content ();

content*
right_n (content* c, size_t n);

void
free_arg_array_data (args* a);

/* global variables */
registry* current_parse_registry;
char* source_code;

void** arbel_ll;
int arbel_ll_cnt;

unsigned long arbel_hash_ans;
unsigned long arbel_hash_0;
unsigned long arbel_hash_1;
unsigned long arbel_hash_2;
unsigned long arbel_hash_3;
unsigned long arbel_hash_4;
unsigned long arbel_hash_data;
unsigned long arbel_hash_up;
unsigned long arbel_hash_class;

data* last_ans;

int arbel_error;

#endif
