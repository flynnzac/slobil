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
  if (d->type == REGISTRY)
    {
      free_registry((registry*) d->data);
      free(d);
    }
  else if (d->type == REFERENCE)
    {
      int i;
      for (i = 0; i < ((ref*) d->data)->levels; i++)
        {
          free(((ref*) d->data)->name[i]);
        }
      free(((ref*) d->data)->name);
      free(((ref*) d->data)->key);
      free(((ref*) d->data)->is_regstr);
      free(d->data);
      free(d);
    }
  else if (d->type == REGISTER)
    {
      free(((regstr*) d->data)->name);
      free(d->data);
      free(d);
    }
  else if (d->type == OPERATION)
    {
      free(d);
    }
  else if (d->type == ARBEL_FILE)
    {
      free(d);
    }
  else if (d->type == INSTRUCTION)
    {
      free_instruction((instruction*) d->data);
      free(d);
    }
  else if (d->type == ACTIVE_INSTRUCTION)
    {
      free_statement(((instruction*) d->data)->stmt);
      free(d->data);
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

  for (int i = 0; i < ARBEL_HASH_SIZE; i++)
    {
      content* c = reg->objects[i];
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

  free(reg);

}
