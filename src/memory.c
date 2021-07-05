/* 
   WOB is a REGISTER BASED ENVIRONMENT AND LANGUAGE
   Copyright 2019 Zach Flynn

   This file is part of WOB.

   WOB is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   WOB is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with WOB (in COPYING file).  If not, see <https://www.gnu.org/licenses/>.
   
*/

#include "wob.h"
#ifdef GARBAGE
#undef malloc
#undef realloc
#undef free
#undef free_statement
#undef free_instruction
#undef free_data
#undef free_registry
#undef free_arg_array_data
#undef free_arg
#define free(x) GC_FREE(x)
#endif




void
free_statement (statement* s)
{
  element* e;
  element* e_tmp;
  statement* s_tmp;
  int i;

  while (s != NULL)
    {
      e = s->head;
      while (e != NULL)
        {
          if (e->data != NULL)
            free_data(e->data);

          if (e->name != NULL)
            {
              for (i=0; i < e->levels; i++)
                {
                  free(e->name[i]);
                }
              free(e->name);
            }

          if (e->hash_name != NULL)
            {
              free(e->hash_name);
            }

          if (e->is_regstr != NULL)
            {
              free(e->is_regstr);
            }

          if (e->s != NULL)
            free_statement(e->s);

          e_tmp = e;
          e = e->right;
          free(e_tmp);
        }
      free_registry(s->arg_reg);
      free(s->hash_bins);
      free_arg(&s->arg);
      s_tmp = s;
      s = s->right;
      free(s_tmp);

    }
}

void
free_instruction (instruction* inst)
{
  if (inst->code != NULL)
    free(inst->code);

  if (inst->stmt != NULL)
    free_statement(inst->stmt);

  free(inst);
      
}

void
free_data (data* d)
{
  if (d==NULL)
    return;
  if (d->type == Registry)
    {
      free_registry((registry*) d->data);
      free(d);
    }
  else if (d->type == Register)
    {
      free(((regstr*) d->data)->name);
      free(d->data);
      free(d);
    }
  else if (d->type == Operation)
    {
      if (((op_wrapper*) d->data)->instr != NULL)
        free_data(((op_wrapper*) d->data)->instr);

      if (((op_wrapper*) d->data)->args != NULL)
        {
          for (int i=0; i < ((op_wrapper*) d->data)->n_arg; i++)
            {
              free_data(((op_wrapper*) d->data)->args[i]);
            }
          free(((op_wrapper*) d->data)->args);
        }

      free(d);
    }
  else if (d->type == File)
    {
      free(d);
    }
  else if (d->type == Instruction)
    {
      free_instruction((instruction*) d->data);
      free(d);
    }
  else if (d->type == Expression)
    {
      free_statement(((instruction*) d->data)->stmt);
      free(d->data);
      free(d);
    }
  else if (d->type == Integer)
    {
      mpz_clear(*((mpz_t*) d->data));
      free(d->data);
      free(d);
    }
  else if (d->type == Task)
    {
      free_task((task*) d->data);
      free(d);
    }
  else
    {
      if (d->data != NULL)
        free(d->data);
  
      free(d);
    }
}

void
free_registry (registry* reg)
{

  if (reg==NULL)
    return;

  for (int i = 0; i < reg->hash_size; i++)
    {
      content* c = reg->objects[i];
      if (c == NULL)
        continue;
      
      if (is_init_reg(c))
        {
          free(c);
          continue;
        }
      content* cur = tail(c);
      content* tmp;

      if (cur != NULL)
        free(cur->left);

      while (cur != NULL)
       {
         if (!cur->do_not_free_data)
           free_data(cur->value);

          free(cur->name);
          tmp = cur->right;
          free(cur);
          cur = tmp;
        }

    }

  free(reg->objects);

  free(reg);

}

void
free_arg_array_data (arg* a, int n)
{
  for (int i = 0; i < n; i++)
    {
      if (a->free_data[i] && a->arg_array[i] != NULL)
        {
          free_data(a->arg_array[i]);
          a->arg_array[i] = NULL;
        }
    }

}

void
free_arg (arg* a)
{
  free_arg_array_data(a, a->length);
  free(a->free_data);
  free(a->arg_array);
}
  
void
free_task (task* t)
{

  if (t->task != NULL)
    free_task_vars(t->task);

  if (t->state != NULL)
    free_registry(t->state);

  if (t->code != NULL)
    free_instruction(t->code);

  if (t->queued_instruction != NULL)
    free_registry(t->queued_instruction);

  if (t->thread != NULL)
    free(t->thread);
}

void
free_task_vars (task_vars* t)
{
  int rc = end_task(t);
}
