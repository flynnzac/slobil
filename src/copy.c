/* 
   SLOBIL is a Object Based Environment and Language

   Copyright 2021 Zach Flynn

   This file is part of SLOBIL.

   SLOBIL is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   SLOBIL is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with SLOBIL (in COPYING file).  If not, see <https://www.gnu.org/licenses/>.
   
*/

#include "slobil.h"

/* Assignment functions */

/**
 * @file copy.c
 * @brief Functions for copying and assigning new data objects for SLOBIL types
 */


/**
 * Assigns a double-precision floating point as a Real SLOBIL value.
 * @param d a pointer to a pointer of data.  The data object will be allocated after the call.  On exit, *d will point to the newly-allocated memory.
 * @param num a double-precision floating point value to assign to the Real data.
 */
void
assign_real (data** d, const double num)
{
  *d = new_data();
  (*d)->type = Real;
  (*d)->data = malloc(sizeof(double));
  *((double*) (*d)->data) = num;
}

/**
 * Assigns a big int (mpz_t) to an Integer SLOBIL value.
 * @param d a pointer to a pointer of data.  The data object will be allocated after the call.  On exit, *d will point to the newly-allocated memory.
 * @param num A big-integer to assign to a data object.
 */
void
assign_int (data** d, const mpz_t num)
{
  *d = new_data();
  (*d)->type = Integer;
  (*d)->data = malloc(sizeof(mpz_t));
  
  mpz_init(*((mpz_t*) (*d)->data));
  mpz_set(*((mpz_t*) (*d)->data), num);
  
}

/**
 * Assigns a UTF-32 string to a String SLOBIL value.
 * @param d a pointer to a pointer of data.  The data object will be allocated after the call.  On exit, *d will point to the newly-allocated memory.
 * @param str a UTF-32 null-terminated string.
 * @param copy if copy != 0, then copy the string to the new object.  If copy=0, then do not copy, just set the string to the pointer str.
 */
void
assign_str (data** d, const uint32_t* str, int copy)
{
  *d = new_data();
  (*d)->type = String;
  
  if (copy)
    {
      (*d)->data = malloc(sizeof(uint32_t)*(u32_strlen(str)+1));
      u32_strcpy((uint32_t*) (*d)->data, str);
    }
  else
    {
      (*d)->data = (uint32_t*) str;
    }
}

/**
 * Assigns statements to an instruction.
 *
 * @param d a pointer to a pointer of data.  The data object will be allocated after the call.  On exit, *d will point to the newly-allocated memory.
 * @param s A linked list of statements (pointer is to head) to add to instruction object
 * @param code the code the instruction runs as a UTF-8 string
 */
void
assign_instr (data** d, statement* s, const char* code, const char* help)
{
  *d = new_data();
  (*d)->type = Instruction;
  (*d)->data = malloc(sizeof(instruction));
  ((instruction*) (*d)->data)->stmt = copy_statement(s);
  ((instruction*) (*d)->data)->code = malloc(sizeof(char)*(strlen(code)+1));
  strcpy(((instruction*) (*d)->data)->code, code);
    ((instruction*) (*d)->data)->help = malloc(sizeof(char)*(strlen(help)+1));
  strcpy(((instruction*) (*d)->data)->help, help);
  ((instruction*) (*d)->data)->being_called = false;
}

/**
 * Assigns statements to an expression
 *
 * @param d a pointer to a pointer of data.  The data object will be allocated after the call.  On exit, *d will point to the newly-allocated memory.
 * @param s a linked list of statements (pointer is to head) to add to instruction object
 */
void
assign_expression (data** d, statement* s)
{
  *d = new_data();
  (*d)->type = Expression;
  (*d)->data = malloc(sizeof(instruction));
  ((instruction*) (*d)->data)->stmt = copy_statement(s);
  ((instruction*) (*d)->data)->being_called = false;
}

/**
 * Assigns an operation, either from a C-function or from an instruction and argument specification
 *
 * @param d a pointer to a pointer of data.  The data object will be allocated after the call.  On exit, *d will point to the newly-allocated memory.
 * @param op a C-operation function pointer
 * @param instr a data object which is an instruction
 * @param args a list of data objects describing the slots to insert data into
 * @param n_arg the number of arguments
 */
void
assign_op (data** d, const operation op,
           data* instr, data** args,
           int n_arg)
{
  *d = new_data();
  (*d)->type = Operation;
  (*d)->data = malloc(sizeof(op_wrapper));
  ((op_wrapper*) (*d)->data)->op = op;
  ((op_wrapper*) (*d)->data)->instr = NULL;
  ((op_wrapper*) (*d)->data)->args = NULL;
  ((op_wrapper*) (*d)->data)->n_arg = n_arg;

  if (args != NULL)
    {
      ((op_wrapper*) (*d)->data)->args =
        malloc(sizeof(data*)*n_arg);

      for (int i = 0; i < n_arg; i++)
        {
          ((op_wrapper*) (*d)->data)->args[i] =
            copy_data(args[i]);
        }
    }

  if (instr != NULL)
    {
      ((op_wrapper*) (*d)->data)->instr = copy_data(instr);
    }
}

/**
 * Assigns an object to data
 *
 * @param d a pointer to a pointer of data.  The data object will be allocated after the call.  On exit, *d will point to the newly-allocated memory.
 * @param obj a pointer to an object to assign to the data element
 * @param copy if true, copy the object, if false have the data point to the object as passed in
 * @param t a task to associate with the object
 */
void
assign_object (data** d, object* obj, bool copy, task* t)
{
  *d = new_data();
  (*d)->type = Object;
  if (obj == NULL)
    {
      (*d)->data = new_object(NULL, SLOBIL_HASH_SIZE, t);
      ((object*) (*d)->data)->inherit = NULL;
    }
  else if (copy)
    {
      (*d)->data = copy_object(obj);
    }
  else
    {
      (*d)->data  = obj;
    }
}

/**
 * Assign a slot to data
 *
 * @param d a pointer to a pointer of data.  The data object will be allocated after the call.  On exit, *d will point to the newly-allocated memory.
 * @param name the slot location, a UTF-8 string
 * @param key the hashed string
 */
void
assign_slot (data** d, const char* name, unsigned long key)
{
  *d = new_data();
  (*d)->type = Slot;
  (*d)->data = malloc(sizeof(slot));
  ((slot*) (*d)->data)->name = malloc(sizeof(char)*(strlen(name)+1));
  strcpy(((slot*) (*d)->data)->name, name);
  ((slot*) (*d)->data)->key = key;
  
}

/**
 * Assigns a file to data
 *
 * @param d a pointer to a pointer of data.  The data object will be allocated after the call.  On exit, *d will point to the newly-allocated memory.
 * @param f a FILE to stream
 */
void
assign_file (data** d, FILE* f)
{
  *d = new_data();
  (*d)->type = File;
  (*d)->data = f;
  
}

/**
 * Assigns a boolean to data
 *
 * @param d a pointer to a pointer of data.  The data object will be allocated after the call.  On exit, *d will point to the newly-allocated memory.
 * @param val a boolean value to assign to the data
 */
void
assign_boolean (data** d, bool val)
{
  *d = new_data();
  (*d)->type = Boolean;
  (*d)->data = malloc(sizeof(bool));
  *((bool*) (*d)->data) = val;
}


/**
 * Assigns a task to the data.
 *
 * @param d a pointer to a pointer of data.  The data object will be allocated after the call.  On exit, *d will point to the newly-allocated memory.
 * @param t the task to assign
 */
void
assign_task (data** d, task* t)
{
  *d = new_data();
  (*d)->type = Task;
  (*d)->data = malloc(sizeof(task));
  ((task*) (*d)->data)->code = copy_instruction(t->code);
  ((task*) (*d)->data)->state = copy_object(t->state);
  ((task*) (*d)->data)->task = copy_task_vars(t->task);
  ((task*) (*d)->data)->pid = t->pid;

}

/**
 * Assign Nothing to a data object
 *
 * @param a pointer to a pointer of data.  The data object will be allocated after the call.  On exit, *d will point to the newly-allocated memory.
 */
void
assign_nothing (data** d)
{
  *d = new_data();
  (*d)->type = Nothing;
  (*d)->data = NULL;
}


/* copy functions */

/**
 * Copy elements of a statement
 *
 * @param e a linked-list of elements (pointer is to head)
 * @return a pointer to the head of the copied list
 */
element*
copy_elements (element* e)
{
  element* new_first = NULL;
  element* el = NULL;
  data* d;
  statement* s;

  while (e != NULL)
    {
      if (e->literal)
        {
          d = copy_data(e->data);
          el = append_literal_element(el, d);
        }
      else
        {
          if (e->statement)
            {
              s = copy_statement(e->s);
              el = append_statement_element(el, s);
            }
          else
            {
              el = append_argument_element(el,
                                           copy_names(e->name,
                                                      e->levels),
                                           copy_hashes(e->hash_name,
                                                       e->levels),
                                           e->levels,
                                           copy_isslot(e->is_slot,
                                                         e->levels));
            }
        }
      if (new_first == NULL)
        {
          new_first = el;
        }
      e = e->right;
    }

  return new_first;
}

/**
 * Copy a linked list of statements
 *
 * @param s a linked-list of statements (pointer is to head)
 * @return a pointer to the head of a copy of the linked list
 */
statement*
copy_statement (statement* s)
{
  if (s == NULL) return NULL;
  
  statement* stmt = NULL;
  statement* new_first = NULL;
  element* head;
  while (s != NULL)
    {
      head = copy_elements(s->head);
      if (new_first == NULL)
        {
          new_first = append_statement(NULL, head);
          stmt = new_first;
        }
      else
        {
          stmt = append_statement(stmt, head);
        }
      s = s->right;
    }

  return new_first;
}

/**
 * Copy an object
 *
 * @param obj
 * @return a pointer to the newly allocated object
 */
object*
copy_object(object* obj0)
{
  object* obj1;
  if (update_hash_size(obj0->elements, obj0->hash_size))
    obj1 = new_object(obj0->up, 2*obj0->hash_size, obj0->task);
  else
    obj1 = new_object(obj0->up, obj0->hash_size, obj0->task);
  
  data* d;

  object_iter iter = get_object_iter(obj0);
  while (!iter.done)
    {
      d = copy_data(iter.cur->value);
      set(obj1, d, iter.cur->name, 0);
      object_next_iter(&iter);
    }

  obj1->inherit = obj0->inherit;
  return obj1;
}

/**
 * Copy an instruction
 *
 * @param instruction to copy
 * @return a pointer to the newly allocated instruction
 */
instruction*
copy_instruction (instruction* inst0)
{
  instruction* inst1 = malloc(sizeof(instruction));
  inst1->stmt = copy_statement(inst0->stmt);
  inst1->code = malloc(sizeof(char)*(strlen(inst0->code)+1));
  strcpy(inst1->code, inst0->code);
  inst1->being_called = false;

  return inst1;
}

/**
 * Copy a task_vars object
 *
 * @param task0 task_vars object to copy
 * @return a pointer to the newly allocated task_vars object
 */
task_vars*
copy_task_vars (task_vars* task0)
{
  task_vars* task1 = malloc(sizeof(task_vars));

  task1->current_parse_object =
    copy_object(task0->current_parse_object);

  task1->source_code = malloc(sizeof(char)*(strlen(task0->source_code)+1));
  strcpy(task1->source_code, task0->source_code);

  task1->slobil_ll = malloc(sizeof(void*)*task0->slobil_ll_cnt);
  for (int i=0; i < task0->slobil_ll_cnt; i++)
    {
      task1->slobil_ll[i] = task0->slobil_ll[i];
    }

  task1->slobil_ll_cnt = task0->slobil_ll_cnt;
  return task1;
}

/**
 * High-level function to copy a data object.  Switches over the data's type.
 *
 * @param d_in a pointer to the data to copy.
 * @return a pointer to a newly-allocated data object.
 */
data*
copy_data (data* d_in)
{
  data* d = NULL;

  switch (d_in->type)
    {
    case Integer:
      assign_int(&d, *((mpz_t*) d_in->data));
      break;
    case Real:
      assign_real(&d, *((double*) d_in->data));
      break;
    case String:
      assign_str(&d, (uint32_t*) d_in->data, 1);
      break;
    case Slot:
      assign_slot(&d, ((slot*) d_in->data)->name,
                    ((slot*) d_in->data)->key);
      break;
    case Object:
      assign_object(&d, (object*) d_in->data, true,
                      ((object*) d_in->data)->task);
      break;
    case Operation:
      assign_op(&d, ((op_wrapper*) d_in->data)->op,
                ((op_wrapper*) d_in->data)->instr,
                ((op_wrapper*) d_in->data)->args,
                ((op_wrapper*) d_in->data)->n_arg);
      break;
    case Instruction:
      assign_instr(&d, ((instruction*) d_in->data)->stmt,
                   ((instruction*) d_in->data)->code,
                   ((instruction*) d_in->data)->help);
      break;
    case Expression:
      assign_expression(&d, ((instruction*) d_in->data)->stmt);
      break;
    case File:
      assign_file(&d, (FILE*) d_in->data);
      break;
    case Boolean:
      assign_boolean(&d, *((bool*) d_in->data));
      break;
    case Nothing:
      assign_nothing(&d);
      break;
    }

  return d;
}
