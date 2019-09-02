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
assign_dec (data** d, const double num)
{
  *d = malloc(sizeof(data));
  (*d)->type = DECIMAL;
  (*d)->data = malloc(sizeof(double));
  *((double*) (*d)->data) = num;
}
          
void
assign_int (data** d, const int num)
{
  *d = malloc(sizeof(data));
  (*d)->type = INTEGER;
  (*d)->data = malloc(sizeof(int));
  *((int*) (*d)->data) = num;
}

char*
escape_str (char* str)
{
  char* dest = str;
  int escape = 0;
  int i;
  int j = 0;
  for (i=0; i < strlen(str); i++)
    {
      if (str[i] == '\\' && escape == 0)
        {
          escape = 1;
        }
      else if (escape == 1)
        {
          escape = 0;
          switch (str[i])
            {
            case '\\':
              dest[j] = '\\';
              j++;
              break;
            case '\'':
              dest[j] = '\'';
              j++;
              break;
            case 't':
              dest[j] = '\t';
              j++;
              break;
            case 'n':
              dest[j] = '\n';
              j++;
              break;
            case 'r':
              dest[j] = '\r';
              j++;
              break;
            default:
              dest[j] = '\\';
              j++;
              dest[j] = str[i];
              j++;
              break;
            }
        }
      else if (str[i] == '\'')
        {
          dest[j] = '"';
          j++;
        }
      else
        {
          dest[j] = str[i];
          j++;
        }
    }

  dest[j] = '\0';

  return dest;

}

void
assign_str (data** d, const char* str, int copy)
{
  *d = malloc(sizeof(data));
  (*d)->type = STRING;
  
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
              el = append_argument_element(el, e->name);
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

void
assign_instr (data** d, statement* s, const char* code)
{
  *d = malloc(sizeof(data));
  (*d)->type = INSTRUCTION;
  (*d)->data = malloc(sizeof(instruction));
  ((instruction*) (*d)->data)->stmt = copy_statement(s);
  ((instruction*) (*d)->data)->code = malloc(sizeof(char)*(strlen(code)+1));
  strcpy(((instruction*) (*d)->data)->code, code);
}

void
assign_active (data** d, statement* s)
{
  *d = malloc(sizeof(data));
  (*d)->type = ACTIVE_INSTRUCTION;
  (*d)->data = copy_statement(s);
}

void
assign_op (data** d, const operation op)
{
  *d = malloc(sizeof(data));
  (*d)->type = OPERATION;
  (*d)->data = op;
}

registry*
copy_registry(registry* r0)
{
  registry* r1 = new_registry(r0->up);
  data* d;
  registry* cur = tail(r0);
  while (cur != NULL)
    {
      d = copy_data(cur->value);
      set(r1, d, cur->name);
      cur = cur->right;
    }

  return r1;
}

void
assign_registry (data** d, registry* r)
{
  *d = malloc(sizeof(data));
  (*d)->type = REGISTRY;
  if (r == NULL)
    {
      (*d)->data = new_registry(NULL);
    }
  else
    {
      (*d)->data = copy_registry(r);
    }
}

void
assign_regstr (data** d, const char* name, unsigned long key)
{
  *d = malloc(sizeof(data));
  (*d)->type = REGISTER;
  (*d)->data = malloc(sizeof(regstr));
  ((regstr*) (*d)->data)->name = malloc(sizeof(char)*(strlen(name)+1));
  strcpy(((regstr*) (*d)->data)->name, name);
  ((regstr*) (*d)->data)->key = key;
  
}

void
assign_ref (data** d, registry* reg, const char* name)
{
  *d = malloc(sizeof(data));
  (*d)->type = REFERENCE;
  (*d)->data = malloc(sizeof(ref));
  ((ref*) (*d)->data)->name = malloc(sizeof(char)*(strlen(name)+1));
  strcpy(((ref*) (*d)->data)->name, name);
  ((ref*) (*d)->data)->reg = reg;
  ((ref*) (*d)->data)->key = hash_str(name);
  
}

void
assign_file (data** d, FILE* f)
{
  *d = malloc(sizeof(data));
  (*d)->type = ARBEL_FILE;
  (*d)->data = f;
  
}


void
assign_nothing (data** d)
{
  *d = malloc(sizeof(data));
  (*d)->type = NOTHING;
  (*d)->data = NULL;
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
      execute_code((statement*) d->data, reg->up);
      d = get(reg, arbel_hash_ans, 0);

    }
  else if (d->type == REFERENCE)
    {
      if (((ref*) d->data)->key == arbel_hash_data)
        {
          d = top_registry;
        }
      else if (((ref*) d->data)->key == arbel_hash_up)
        {
          d = up_registry;
          d->data = reg->up->up;
          if (d->data == NULL)
            {
              do_error("Reference `up` not found.");
              d = NULL;
            }
        }
      else
        {
          data* d_ref;
          if (((ref*) d->data)->reg == NULL)
            {
              d_ref = lookup(reg->up,
                             ((ref*) d->data)->key,
                             1);
            }
          else
            {
              d_ref = lookup(((ref*) d->data)->reg,
                             ((ref*) d->data)->key,
                             1);
            }

          if (d_ref == NULL)
            {
              char* msg = malloc(sizeof(char)*
                                 (strlen("Reference not found.") +
                                  strlen(((ref*) d->data)->name) +
                                  5));
              sprintf(msg, "Reference `%s` not found.",
                      ((ref*) d->data)->name);
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
    }

  return d;
}

void
mov (registry* reg, regstr* old, regstr* new)
{
  registry* cur = tail(reg);
  while (cur != NULL)
    {
      if (cur->key == old->key)
        {
          del(reg, new->key,1);
          free(cur->name);
          cur->name = malloc(sizeof(char)*(strlen(new->name)+1));
          strcpy(cur->name, new->name);
          cur->key = new->key;
          return;
        }
      cur = cur->right;
    }
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
ret (registry* reg, data* d, const char* name)
{
  registry* up = reg->up;
  if (up != NULL)
    {
      set(up, d, name);
    }
  else
    {
      do_error("Cannot return from top-level.");
    }
}

void
ret_ans (registry* reg, data* d)
{
  if (d == NULL)
    return;
  
  ret(reg, d, "ans");
  if (!is_error(-1) && reg->up != NULL)
    {
      if (reg->up->up == NULL)
        is_retval(1);
    }
}
  

data*
copy_data (data* d_in)
{
  data* d = NULL;

  switch (d_in->type)
    {
    case INTEGER:
      assign_int(&d, *((int*) d_in->data));
      break;
    case DECIMAL:
      assign_dec(&d, *((double*) d_in->data));
      break;
    case STRING:
      assign_str(&d, (const char*) d_in->data, 1);
      break;
    case REGISTER:
      assign_regstr(&d, ((regstr*) d_in->data)->name,
                    ((regstr*) d_in->data)->key);
      break;
    case REGISTRY:
      assign_registry(&d, (registry*) d_in->data);
      break;
    case OPERATION:
      assign_op(&d, (operation) d_in->data);
      break;
    case INSTRUCTION:
      assign_instr(&d, ((instruction*) d_in->data)->stmt,
                   ((instruction*) d_in->data)->code);
      break;
    case ACTIVE_INSTRUCTION:
      assign_active(&d, (statement*) d_in->data);
      break;
    case ARBEL_FILE:
      assign_file(&d, (FILE*) d_in->data);
      break;
    case NOTHING:
      assign_nothing(&d);
      break;
    case REFERENCE:
      assign_ref(&d,((ref*) d_in->data)->reg,
                 ((ref*) d_in->data)->name);
      break;
    }

  return d;
}

void
compute (registry* reg)
{
  data* arg = lookup(reg,arbel_hash_0,0);
  if (arg == NULL)
    {
      do_error("Cannot compute a registry without an instruction at #0.");
    }
  else if (arg->type != OPERATION && arg->type != INSTRUCTION)
    {

      do_error("Cannot compute a registry without an instruction at #0.");
    }

  if (is_error(-1))
    return;

  if (arg->type == INSTRUCTION)
    {
      execute_code(((instruction*) arg->data)->stmt, reg);
      data* d = get(reg, arbel_hash_ans, 0);
      if (d != NULL)
        {
          mark_do_not_free(reg, arbel_hash_ans);
          ret_ans(reg, d);
        }
      return;
    }

  if (is_error(-1))
    {
      return;
    }

  ((operation) arg->data)(reg);
  
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
