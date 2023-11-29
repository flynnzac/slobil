/*
  SLOBIL
  Copyright 2023 Zach Flynn

  Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

  1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

  2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

  3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS “AS IS” AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
   
*/


#ifndef SLOBIL_H
#define SLOBIL_H
#define _GNU_SOURCE
#define SLOBIL_HASH_SIZE 31
#define SLOBIL_LOAD_FACTOR 0.75


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
#include <unistr.h>
#include <unistdio.h>
#include <gmp.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <pthread.h>
#include <signal.h>
#include <zlib.h>
#include <endian.h>

/**
 * Data type enum, contains integer codes for each data type. They are powers of 2 to allow bitwise OR for accepting multiple argument types
 */
enum data_type
  {
    Integer = 1,
    Real = 2,
    String = 4,
    Slot = 8,
    Object = 16,
    Instruction = 32,
    Expression = 64,
    Operation = 128,
    File = 256,
    Boolean = 512,
    Nothing = 1024,
    NotAType = 2048,
    Task = 4096
  };

typedef enum data_type data_type;
/**
 * C representation of a slot.
 */
struct slot
{
  char* name;
  unsigned long key;
};

typedef struct slot slot;

struct arg;
struct object;

/**
 * Generic data object, contains a type and a pointer to the actual data
 */
struct data
{
  void* data;
  data_type type;
};
typedef struct data data;

/**
 * Contains data, a name for the data, in a linked-list structure.
 */
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

struct object;
/**
 * An object to hold the current state of a task as well as task-level variables
 */
struct task_vars
{
  struct object* current_parse_object;
  char* source_code;
  void** slobil_ll;
  int slobil_ll_cnt;

  size_t slobil_stop_error_threshold;

  data* last_ans;
  int slobil_error;
  int do_exit;
  
  struct object* slobil_options;

  slot slobil_slot_ans;
  bool reading;
};

typedef struct task_vars task_vars;

struct task;
/**
 * C representation of an object.  It consists of an array of linked lists equal to hash_size in length.  Items are sorted into buckets by hashing the slot string.  If the object inherits from another one that is marked explicitly. up gives the object above it for scoping.
 *
 */
struct object
{
  content** objects;
  size_t hash_size;
  size_t elements;
  struct task* task;
  struct object* up;
  struct object* inherit;
  bool being_modified;
};

typedef struct object object;

/**
 * An iterator for an object's elements.
 */
struct object_iter
{
  object* obj;
  content* cur;
  int bucket;
  bool done;
};
typedef struct object_iter object_iter;

typedef void (*operation)(struct arg, object*);


/**
 * Wrapper for both C-based operations and instructions masquerading as operations
 */
struct op_wrapper
{
  operation op;
  data* instr;
  data** args;
  int n_arg;
};

typedef struct op_wrapper op_wrapper;

struct statement;
struct element;

/**
 * Maintains parser state.
 */
struct parser_state
{
  char* buffer;
  size_t buffer_sz;
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
  
};

typedef struct parser_state parser_state;

/**
 * Element in a statement.  Contains pointers to lots of things the element might be.  Most of which
 * are NULL for a given element.
 */
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
  int* is_slot;
};

typedef struct element element;

/**
 * Argument object, contains the data from a statement.
 */
struct arg
{
  data** arg_array;
  int* free_data;
  size_t length;
};

typedef struct arg arg;

/**
 * Statements are a linked list of elements and also a linked list of statements.
 */
struct statement
{
  element* head;
  arg arg;
  struct statement* right;
  object* arg_reg;
  unsigned long* hash_bins;
  size_t* location;
};

typedef struct statement statement;

/**
 * C-representation of instruction which are statements (effectively, a semi-compiled version of the
 * code) and the code itself (for printing and saving).
 */
struct instruction
{
  statement* stmt;
  char* code;
  char* help;
  bool being_called;
};

typedef struct instruction instruction;

/**
 * C representation of a task object.  Contains a task_vars structure, its own state (an object), a
 * mutex, and thread objects for multithreading.
 */
struct task
{
  task_vars* task;
  object* state;
  instruction* code;

  object* queued_instruction;
  pthread_mutex_t lock;
  int pid;
  pthread_t* thread;
};

typedef struct task task;

enum print_settings
  {
    PRINT_PLAIN = 0,
    PRINT_ANSWER = 1,
    PRINT_NEWLINE = 2,
    PRINT_QUOTES = 4
  };

typedef enum print_settings print_settings;

char*
argument_name (int n);

char*
custom_argument_name (int n, char* s);

bool
is_integer (const char* str);

void
assign_real (data** d, const double num);

void
assign_int (data** d, const mpz_t num);

void
assign_str (data** d, const uint32_t* str, int copy);

void
assign_op (data** d, const operation op,
           data* instr, data** args,
           int n_arg);

void
assign_object (data** d, object* r, bool copy, task* task);

void
assign_slot (data** d, const char* name, unsigned long key);

void
assign_boolean (data** d, bool val);

void
assign_nothing (data** d);

void
assign_file (data** d, FILE* f);

bool
is_numeric (data* d);

bool
is_boolean (const char* str);


void
free_data (data* d);

void
free_object (object* reg);

content*
head (content* reg);

content*
tail (content* reg);

content*
set (object* reg, data* d, char* name, int rehash);

data*
get (object* reg, slot* sl, int recursive);

content*
mov (object* reg, slot* old, slot* new);

content*
del (object* reg, slot* sl, int del_data, bool hard_free);

data*
get_data_in_object (object* reg, const slot name);

void
do_error (const char* msg, task_vars* t);

void
null_ans (object* reg);

void
print_data (data* d, print_settings settings);

bool
is_slot (const char* str);

void
str_shift_left (char* buffer);

void
add_basic_ops (object* reg);

bool
is_whitespace (const char c);

void
print_object (object* reg, bool initial);

char*
append_nl (char* str);

data*
copy_data (data* d_in);

void
shift_arguments (object* reg);

int
is_error (int e, task_vars* t);

int
is_exit (int e, task_vars* t);

bool
is_init_content (content* c);

object*
new_object (object* parent, size_t hash_size, task* t);

void
ret (object* reg, data* d, char* name);

void
ret_ans (object* reg, data* d);

void
relabel (object* reg, const char* name, const char* new_name);

void
assign_instr (data** d, statement* s, const char* code,
              const char* help);

struct parser_state
fresh_state ();

void
free_state (struct parser_state* state);

object*
copy_object(object* r0);

int
is_retval (const int r);

bool
is_real (const char* str);

bool
is_nothing (const char* str);

void
assign_expression (data** d, statement* s);

data*
lookup (object* reg, slot* sl, int recursive);

void
compute (data* cmd, object* reg, arg arg);

int
save_object (gzFile f, object* reg);

void
read_outer (gzFile f, object* reg);

int
read_object (gzFile f, object* reg);

char*
vector_name (const char* lead, int n);

void
execute_statement (statement* s, object* reg);

statement*
append_statement (statement* current, element* head);

element*
append_argument_element (element* current, char** name,
                         unsigned long* hash_name, const int levels,
                         int* is_slot);

element*
append_literal_element (element* current, data* d);

element*
append_statement_element (element* current, statement* s);

int
parse (FILE* f, parser_state* state, statement** s, task_vars* task);

void
execute_code (statement* s, object* reg);

element*
parse_stmt (FILE* f, parser_state* state, int* complete, task_vars* task);

int
interact (FILE* f, parser_state* state, object* reg);

element*
copy_elements (element* e);

statement*
copy_statement (statement* s);

void
free_statement (statement* s);

char*
escape_str(char* str);

void
mark_do_not_free (object* reg, unsigned long hash_name);

unsigned long
hash_str(const char *str);

char**
split_by_colon (const char* name, int* cnt, int** is_slot);

char**
copy_names (char** name, int levels);

unsigned long*
copy_hashes (unsigned long* hashes, int levels);

int*
copy_isslot (int* is_slot, int levels);

data*
get_by_levels (object* reg, unsigned long* hash, int levels, int* is_slot, char** name, bool error_on_not_found);

const char*
str_type (data_type type);

object*
shift_list_down (object* reg);

int
save_content (gzFile f, content* reg);

void
save_outer (object* reg, char* fname);

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
resolve (data* arg, object* reg);

void
_op_call (arg a, object* obj, object* obj_ans, const int explicit);

void
auto_set (arg a, object* reg);

void
method_call (arg a, object* reg);

size_t
new_hash_size (size_t elements);

void
check_length (arg* a, int length, char* op, task_vars* t);

bool
update_hash_size (size_t elements, size_t hash_size);

void
rehash (object* r0);

void
execute_0 (data* instr, object* reg);

void
print_statement (statement* s);

data*
new_data();

void
slobil_location(mpz_t loc, int n);

int
digits (int n);

instruction*
copy_instruction (instruction* inst0);

task_vars*
copy_task_vars (task_vars* task0);

arg
shift_arg_left(arg a);

bool
object_from_args (arg a, object* obj, int arg_start, task_vars* t);

/* task is an interpreter states */

task_vars*
new_task (task* t0);

int
end_task (task_vars* t);

void
run_task_readline (task_vars* task,
                   bool save_code,
                   struct parser_state* state,
                   int echo);

void
run_task_socket (task_vars* task,
                 int port,
                 bool save_code,
                 struct parser_state* state,
                 int echo);

void
run_task (data* t);

void*
run_task_thread (void* input);

void
free_task (task* t);

void
free_task_vars (task_vars* t);

/* string conversions */

uint32_t*
slobil_u8_to_u32 (const uint8_t* s, size_t n);

uint8_t*
slobil_u32_to_u8 (const uint32_t* s, size_t n);

uint32_t*
u32_str_to_le (const uint32_t* str);

uint32_t*
u32_str_to_h (const uint32_t* str);

object_iter
get_object_iter (object* obj);

void
object_next_iter (object_iter* iter);

/* interpreter internal object */

slot
make_slot(char* name);

void
replace_all_non_literals(statement* stmt, object* obj);



#ifdef GARBAGE
#define GC_PTHREADS
#define GC_THREADS
#include <gc/gc.h>
#define pthread_create GC_pthread_create
#define pthread_exit GC_pthread_exit
#define malloc(x) GC_MALLOC(x)
#define realloc(x,y) GC_REALLOC(x,y)
#define free(x)
#define free_statement(x)
#define free_instruction(x)
#define free_data(x)
#define free_object(x)
#define free_arg_array_data(x,n)
#define free_arg(x)  
#endif

#endif
