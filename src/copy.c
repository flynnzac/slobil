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

#include "arbel.h"

/* Assignment functions */

void
assign_real (data** d, const double num)
{
  *d = new_data();
  (*d)->type = Real;
  (*d)->data = malloc(sizeof(double));
  *((double*) (*d)->data) = num;
}
          
void
assign_int (data** d, const mpz_t num)
{
  *d = new_data();
  (*d)->type = Integer;
  (*d)->data = malloc(sizeof(mpz_t));
  
  mpz_init(*((mpz_t*) (*d)->data));
  mpz_set(*((mpz_t*) (*d)->data), num);
  
}

void
assign_str (data** d, const char* str, int copy)
{
  *d = new_data();
  (*d)->type = String;
  
  if (copy)
    {
      (*d)->data = malloc(sizeof(char)*(strlen(str)+1));
      strcpy((char*) (*d)->data, str);
    }
  else
    {
      (*d)->data = (char*) str;
    }
}

void
assign_instr (data** d, statement* s, const char* code)
{
  *d = new_data();
  (*d)->type = Instruction;
  (*d)->data = malloc(sizeof(instruction));
  ((instruction*) (*d)->data)->stmt = copy_statement(s);
  ((instruction*) (*d)->data)->code = malloc(sizeof(char)*(strlen(code)+1));
  strcpy(((instruction*) (*d)->data)->code, code);
  ((instruction*) (*d)->data)->being_called = false;
}

void
assign_active (data** d, statement* s)
{
  *d = new_data();
  (*d)->type = Active_Instruction;
  (*d)->data = malloc(sizeof(instruction));
  ((instruction*) (*d)->data)->stmt = copy_statement(s);
  ((instruction*) (*d)->data)->being_called = false;
}

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

void
assign_registry (data** d, registry* r, bool copy, arbel_task* task)
{
  *d = new_data();
  (*d)->type = Registry;
  if (r == NULL)
    {
      (*d)->data = new_registry(NULL, ARBEL_HASH_SIZE, task);
    }
  else if (copy)
    {
      (*d)->data = copy_registry(r);
    }
  else
    {
      (*d)->data  = r;
    }
}

void
assign_regstr (data** d, const char* name, unsigned long key)
{
  *d = new_data();
  (*d)->type = Register;
  (*d)->data = malloc(sizeof(regstr));
  ((regstr*) (*d)->data)->name = malloc(sizeof(char)*(strlen(name)+1));
  strcpy(((regstr*) (*d)->data)->name, name);
  ((regstr*) (*d)->data)->key = key;
  
}

void
assign_file (data** d, FILE* f)
{
  *d = new_data();
  (*d)->type = File;
  (*d)->data = f;
  
}

void
assign_boolean (data** d, bool val)
{
  *d = new_data();
  (*d)->type = Boolean;
  (*d)->data = malloc(sizeof(bool));
  *((bool*) (*d)->data) = val;
}


void
assign_task (data** d, task* t)
{
  *d = new_data();
  (*d)->type = Task;
  (*d)->data = malloc(sizeof(task));
  ((task*) (*d)->data)->inst = copy_instruction(task->inst);
  ((task*) (*d)->data)->state = copy_registry(task->state);
  ((task*) (*d)->data)->task = copy_arbel_task(task->task);

}

void
assign_nothing (data** d)
{
  *d = new_data();
  (*d)->type = Nothing;
  (*d)->data = NULL;
}


/* copy functions */

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
                                           copy_isregstr(e->is_regstr,
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

registry*
copy_registry(registry* r0)
{
  registry* r1;
  if (update_hash_size(r0->elements, r0->hash_size))
    r1 = new_registry(r0->up, 2*r0->hash_size, r0->task);
  else
    r1 = new_registry(r0->up, r0->hash_size, r0->task);
  
  data* d;

  for (int i = 0; i < r0->hash_size; i++)
    {
      content* cur = r0->objects[i];
      if (cur == NULL)
        continue;

      cur = tail(r0->objects[i]);
      while (cur != NULL)
        {
          d = copy_data(cur->value);
          set(r1, d, cur->name, 0);
          cur = cur->right;
        }
    }

  return r1;
}

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

arbel_task*
copy_arbel_task (arbel_task* task0)
{
  arbel_task* task1 = malloc(sizeof(arbel_task));

  task1->current_parse_registry =
    copy_registry(task0->current_parse_registry);

  task1->source_code = malloc(sizeof(char)*(strlen(task0->source_code)+1));
  strcpy(task1->source_code, task0->source_code);

  task1->arbel_ll = malloc(sizeof(void*)*task0->arbel_ll_cnt);
  for (int i=0; i < task0->arbel_ll_cnt; i++)
    {
      task1->arbel_ll[i] = task0->arbel_ll[i];
    }

  task1->arbel_ll_cnt = task0->arbel_ll_cnt;
}

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
      assign_str(&d, (const char*) d_in->data, 1);
      break;
    case Register:
      assign_regstr(&d, ((regstr*) d_in->data)->name,
                    ((regstr*) d_in->data)->key);
      break;
    case Registry:
      assign_registry(&d, (registry*) d_in->data, true,
                      ((registry*) d_in->data)->task);
      break;
    case Operation:
      assign_op(&d, ((op_wrapper*) d_in->data)->op,
                ((op_wrapper*) d_in->data)->instr,
                ((op_wrapper*) d_in->data)->args,
                ((op_wrapper*) d_in->data)->n_arg);
      break;
    case Instruction:
      assign_instr(&d, ((instruction*) d_in->data)->stmt,
                   ((instruction*) d_in->data)->code);
      break;
    case Active_Instruction:
      assign_active(&d, ((instruction*) d_in->data)->stmt);
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
