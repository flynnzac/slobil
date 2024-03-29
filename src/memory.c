/* 
   SLOBIL
   Copyright 2023 Zach Flynn

   Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

   1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

   3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS “AS IS” AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
   
*/

#include "slobil.h"
#ifdef GARBAGE
#undef malloc
#undef realloc
#undef free
#undef free_statement
#undef free_instruction
#undef free_data
#undef free_object
#undef free_arg_array_data
#undef free_arg
#define free(x) GC_FREE(x)
#endif


/**
 * @file memory.c
 * @brief Functions for manipulating memory
 */

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

          if (e->is_slot != NULL)
            {
              free(e->is_slot);
            }

          if (e->s != NULL)
            free_statement(e->s);

          e_tmp = e;
          e = e->right;
          free(e_tmp);
        }
      free_object(s->arg_reg);
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
  if (d->type == Object)
    {
      free_object((object*) d->data);
      free(d);
    }
  else if (d->type == Slot)
    {
      free(((slot*) d->data)->name);
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
free_object (object* reg)
{

  if (reg==NULL)
    return;

  for (int i = 0; i < reg->hash_size; i++)
    {
      content* c = reg->objects[i];
      if (c == NULL)
        continue;
      
      if (is_init_content(c))
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
    free_object(t->state);

  if (t->code != NULL)
    free_instruction(t->code);

  if (t->queued_instruction != NULL)
    free_object(t->queued_instruction);

  if (t->thread != NULL)
    free(t->thread);
}

void
free_task_vars (task_vars* t)
{
  int rc = end_task(t);
}
