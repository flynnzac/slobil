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
#define ARBEL_HASH_SIZE 31
#define ARBEL_LOAD_FACTOR 0.75
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
#include <stdbool.h>
#include <unicode/ustring.h>

enum data_type
  {
   INTEGER = 1,
   REAL = 2,
   STRING = 4,
   REGISTER = 8,
   REGISTRY = 16,
   INSTRUCTION = 32,
   ACTIVE_INSTRUCTION = 64,
   OPERATION = 128,
   ARBEL_FILE = 256,
   BOOLEAN = 512,
   NOTHING = 1024
  };

typedef enum data_type data_type;

struct arg;

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
  UChar* name;
  struct content* right;
  struct content* left;
  int do_not_free_data;
};

typedef struct content content;

struct registry
{
  content** objects;
  size_t hash_size;
  size_t elements;
  struct registry* up;
};

typedef struct registry registry;

typedef void (*operation)(struct arg, registry*);

struct regstr
{
  UChar* name;
  unsigned long key;
};

typedef struct regstr regstr;

struct command
{
  UChar* code;
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
  UChar** name;
  struct statement* s;
  int literal;
  int statement;
  unsigned long* hash_name;
  struct element* right;
  int levels;
  int* is_regstr;
};

typedef struct element element;

struct arg
{
  data** arg_array;
  int* free_data;
  size_t length;
};

typedef struct arg arg;

struct statement
{
  element* head;
  arg arg;
  struct statement* right;
  registry* arg_reg;
  unsigned long* hash_bins;
  size_t* location;
};

typedef struct statement statement;

struct instruction
{
  statement* stmt;
  UChar* code;
  bool being_called;
};

typedef struct instruction instruction;

enum trash_type
  {
   TRASH_C,
   TRASH_DATA,
   TRASH_REGISTRY
  };

struct trash_heap
{
  void* garbage;
  size_t sz;
  struct trash_heap* right;
  enum trash_type type;
};

typedef struct trash_heap trash_heap;

UChar*
argument_name (int n);

bool
is_integer (const UChar* str);

void
assign_real (data** d, const double num);

void
assign_int (data** d, const int num);

void
assign_str (data** d, const UChar* str, int copy);

void
assign_op (data** d, const operation op);

void
assign_registry (data** d, registry* r);

void
assign_regstr (data** d, const UChar* name, unsigned long key);

void
assign_boolean (data** d, bool val);

void
assign_nothing (data** d);

void
assign_file (data** d, FILE* f);

bool
is_numeric (data* d);

bool
is_boolean (const UChar* str);


void
free_data (data* d);

void
free_registry (registry* reg);

content*
head (content* reg);

content*
tail (content* reg);

content*
set (registry* reg, data* d, const UChar* name, int rehash);

data*
get (registry* reg, unsigned long hash_name, int recursive);

content*
mov (registry* reg, regstr* old, regstr* new);

content*
del (registry* reg, unsigned long hash_name, int del_data);

data*
get_data_in_registry (registry* reg, const regstr name);

void
do_error (const UChar* msg);

void
null_ans (registry* reg);

void
print_data (data* d, int print_cmd);

bool
is_register (const UChar* str);

void
str_shift_left (UChar* buffer);

void
add_basic_ops (registry* reg);

bool
is_whitespace (const UChar c);

void
print_registry (registry* reg);

UChar*
append_nl (UChar* str);

data*
copy_data (data* d_in);

void
shift_arguments (registry* reg);

int
is_error (int e);

int
is_exit (int e);

bool
is_init_reg (content* r);

registry*
new_registry (registry* parent, size_t hash_size);

void
ret (registry* reg, data* d, const UChar* name);

void
ret_ans (registry* reg, data* d);

void
relabel (registry* reg, const UChar* name, const UChar* new_name);

void
assign_instr (data** d, statement* s, const UChar* code);

struct parser_state
fresh_state ();

registry*
copy_registry(registry* r0);

int
is_retval (const int r);

bool
is_real (const UChar* str);

void
assign_active (data** d, statement* s);

data*
lookup (registry* reg, unsigned long hash_name, int recursive);

void
compute (data* cmd, registry* reg, arg arg);

int
save_registry (FILE* f, registry* reg);

int
read_registry (FILE* f, registry* reg);

UChar*
vector_name (const UChar* lead, int n);

void
execute_statement (statement* s, registry* reg);

statement*
append_statement (statement* current, element* head);

element*
append_argument_element (element* current, UChar** name,
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

UChar*
escape_str(UChar* str);

void
mark_do_not_free (registry* reg, unsigned long hash_name);

unsigned long
hash_str(const char *str);

UChar**
split_by_colon (const UChar* name, int* cnt, int** is_regstr);

UChar**
copy_names (UChar** name, int levels);

unsigned long*
copy_hashes (unsigned long* hashes, int levels);

int*
copy_isregstr (int* is_regstr, int levels);

data*
get_by_levels (registry* reg, unsigned long* hash_name, int levels, int* is_regstr, UChar** name);

const UChar*
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
free_arg_array_data (arg* a, int n);

void
free_arg (arg* a);

arg
gen_arg (int length, int def_free);

data*
resolve (data* arg, registry* reg);

void
_op_call (arg a, registry* reg, const int explicit);

size_t
new_hash_size (size_t elements);

void
check_length (arg* a, int length);

int
update_hash_size (size_t elements, size_t hash_size);

void
rehash (registry* r0);

void
execute_0 (data* instr, registry* reg);

void
print_statement (statement* s);

#define CHECK_ARGS(a,length) check_length(&a, length+1); if (is_error(-1)) return;

/* global variables */
registry* current_parse_registry;
UChar* source_code;

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

size_t arbel_stop_error_threshold;

data* last_ans;

int arbel_error;

#endif
