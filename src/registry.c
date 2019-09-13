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

registry*
new_registry (registry* parent)
{
  registry* r = malloc(sizeof(registry));
  r->right = NULL;
  r->left = NULL;
  r->up = parent;
  r->value = NULL;
  r->name = NULL;
  r->key = 0;
  r->do_not_free_data = 0;
  return r;
}

int
is_init_reg (registry* r)
{
  return (r->right == NULL) && (r->left==NULL) && (r->value == NULL) &&
    (r->key == 0) && (r->name == NULL);
}

void
set (registry* reg, data* d, const char* name)
{
  unsigned long hash_name = hash_str(name);
  registry* new_reg = del(reg,hash_name,-1);

  if (new_reg == NULL)
    {

      reg = head(reg);
      new_reg = malloc(sizeof(registry));
      new_reg->left = reg;
      new_reg->right = NULL;
      new_reg->up = reg->up;
      new_reg->value = d;
      reg->right = new_reg;
      new_reg->do_not_free_data = 0;
      new_reg->name = malloc(sizeof(char)*(strlen(name)+1));
      strcpy(new_reg->name, name);
      new_reg->key = hash_name;
    }
  else
    {
      new_reg->do_not_free_data = 0;
      new_reg->value = d;
    }

  if (d != NULL && d->type == REGISTRY)
    {
      ((registry*) d->data)->up = reg;
    }


}

data*
get (registry* reg, unsigned long hash_name, int recursive)
{
  if (reg == NULL)
    return NULL;

  if (is_init_reg(reg))
    {
      if (recursive)
        {
          return get(reg->up, hash_name, recursive);
        }
      else
        {
          return NULL;
        }
    }

  registry* cur = reg->right;

  while (cur != NULL)
    {
      if (cur->key == hash_name && cur->value != NULL)
        {
          return cur->value;
        }
      cur = cur->right;
    }

  if (reg == NULL) return NULL;
  if ((reg->up != NULL) && recursive)
    {
      return get(reg->up, hash_name, recursive);
    }
  else
    {
      return NULL;
    }
}

data*
lookup (registry* reg, unsigned long hash_name, int recursive)
{
  data* d = get(reg, hash_name, recursive);

  if (d == NULL)
    return NULL;

  if (d->type == ACTIVE_INSTRUCTION && (reg->up != NULL))
    {
      execute_code(((instruction*) d->data)->stmt, reg->up);
      d = get(reg, arbel_hash_ans, 0);

    }
  else if (d->type == REFERENCE)
    {
      
      data* d_ref;
      if (((ref*) d->data)->reg == NULL)
        {
          d_ref = get_by_levels(reg->up,
                                ((ref*) d->data)->key,
                                ((ref*) d->data)->levels,
                                ((ref*) d->data)->is_regstr,
                                ((ref*) d->data)->name);
        }
      else
        {
          d_ref = get_by_levels(((ref*) d->data)->reg,
                                ((ref*) d->data)->key,
                                ((ref*) d->data)->levels,
                                ((ref*) d->data)->is_regstr,
                                ((ref*) d->data)->name);
        }

      if (d_ref == NULL)
        {
          char* msg = malloc(sizeof(char)*
                             (strlen("Reference not found.") +
                              strlen(((ref*) d->data)->name[0]) +
                              5));
          sprintf(msg, "Reference `%s` not found.",
                  ((ref*) d->data)->name[0]);
          do_error(msg);
          free(msg);
          d = NULL;
        }
      else
        {
          if (d_ref->type == REGISTRY)
            {
              ((registry*) d_ref->data)->up = reg->up;
            }
          d = d_ref;
        }
    }

  return d;
}

registry*
mov (registry* reg, regstr* old, regstr* new)
{
  registry* cur = tail(reg);
  while (cur != NULL)
    {
      if (cur->key == old->key)
        {
          del(reg, new->key,1);
          if (cur->name != NULL)
            free(cur->name);
          
          cur->name = malloc(sizeof(char)*(strlen(new->name)+1));
          strcpy(cur->name, new->name);
          cur->key = new->key;
          return cur;
        }
      cur = cur->right;
    }
  return NULL;
}

registry*
del (registry* reg, unsigned long hash_name, int del_data)
{
  registry* cur;
  cur = tail(reg);

  while (cur != NULL)
    {
      if (cur->key == hash_name)
        {
          if (cur->right != NULL && del_data >= 0)
            {
              cur->right->left = cur->left;
            }
          if (cur->left != NULL && del_data >= 0)
            {
              cur->left->right = cur->right;
            }

          if (del_data && cur->value != NULL && !cur->do_not_free_data)
            {
              free_data(cur->value);
              cur->value = NULL;
            }

          if (cur->name != NULL && del_data >= 0)
            {
              free(cur->name);
              cur->name = NULL;
            }
                
          if (del_data < 0)
            {
              return cur;
            }
          else
            {
              free(cur);
              return NULL;
            }

        }
      cur = cur->right;
    }

  return NULL;
}

void
mark_do_not_free (registry* reg, unsigned long hash_name)
{
  if (reg==NULL || is_init_reg(reg))
    return;

  registry* cur = reg->right;
  while (cur != NULL)
    {
      if (cur->key == hash_name)
        {
          cur->do_not_free_data = 1;
          return;
        }
      cur = cur->right;
    }

}

data*
get_by_levels (registry* reg, unsigned long* hash_name, int levels, int* is_regstr, char** name)
{
  data* d = lookup(reg, hash_name[0], 1);
  if (d == NULL)
    {
      char* msg = malloc(sizeof(char)*
                         (strlen("Value `` not found.")
                          + strlen(name[0]) + 1));
      sprintf(msg, "Value `%s` not found.", name[0]);
      do_error(msg);
      free(msg);
    }
  else if (d->type != REGISTRY && levels > 1)
    {
      do_error("Cannot get registers in non-registry.");
    }
  else
    {
      for (int i=1; i < levels; i++)
        {
          if (d == NULL)
            {
              do_error("Register not found in registry.");
              return NULL;
            }

          if (d->type != REGISTRY)
            {
              do_error("Cannot get registers in non-registry.");
              return NULL;
            }

          if (is_regstr[i])
            {
              d = lookup((registry*) d->data, hash_name[i], 0);
            }
          else
            {
              data* d1 = lookup(reg, hash_name[i], 1);
              if (d1 == NULL || d1->type != REGISTER)
                {
                  do_error("Cannot use `:` with non-register.");
                  return NULL;
                }
              else
                {
                  d = lookup((registry*) d->data,
                             ((regstr*) d1->data)->key,
                             0);
                }
            }
        }

      if (d == NULL)
        {
          do_error("Register not found in registry.");
          return NULL;
        }
		  
    }

  return d;
}



registry*
head (registry* reg)
{
  if (reg == NULL)
    return NULL;

  while (reg->right != NULL)
    {
      reg = reg->right;
    }

  return reg;
}

registry*
tail (registry* reg)
{
  if (reg == NULL)
    return NULL;
  
  while (reg->left != NULL)
    {
      reg = reg->left;
    }

  return reg->right;
}
