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

void
assign_str (data** d, const char* str)
{
  *d = malloc(sizeof(data));
  (*d)->type = STRING;
  (*d)->data = malloc(sizeof(char)*(strlen(str)+1));

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
              ((char*) ((*d)->data))[j] = '\\';
              j++;
              break;
            case '\'':
              ((char*) ((*d)->data))[j] = '\'';
              j++;
              break;
            case 't':
              ((char*) ((*d)->data))[j] = '\t';
              j++;
              break;
            case 'n':
              ((char*) ((*d)->data))[j] = '\n';
              j++;
              break;
            case 'r':
              ((char*) ((*d)->data))[j] = '\r';
              j++;
              break;
            default:
              ((char*) ((*d)->data))[j] = '\\';
              j++;
              ((char*) ((*d)->data))[j] = str[i];
              j++;
              break;
            }
        }
      else if (str[i] == '\'')
        {
          ((char*) ((*d)->data))[j] = '"';
          j++;
        }
      else
        {
          ((char*) ((*d)->data))[j] = str[i];
          j++;
        }
    }

  ((char*) ((*d)->data))[j] = '\0';
  
  
}

void
assign_instr (data** d, statement* s, const char* code)
{
  *d = malloc(sizeof(data));
  (*d)->type = INSTRUCTION;
  (*d)->data = malloc(sizeof(instruction));
  ((instruction*) (*d)->data)->stmt = s;
  ((instruction*) (*d)->data)->code = malloc(sizeof(char)*(strlen(code)+1));
  strcpy(((instruction*) (*d)->data)->code, code);
}

void
assign_active (data** d, statement* s)
{
  *d = malloc(sizeof(data));
  (*d)->type = ACTIVE_INSTRUCTION;
  (*d)->data = s;
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
      set(r1, d, cur->key);
      cur = cur->right;
    }

  return r1;
}
  

void
assign_registry (data** d, registry* r)
{
  *d = malloc(sizeof(data));
  (*d)->type = REGISTRY;
  (*d)->data = copy_registry(r);
}

void
assign_regstr (data** d, const char* name)
{
  *d = malloc(sizeof(data));
  (*d)->type = REGISTER;
  (*d)->data = malloc(sizeof(char)*(strlen(name)+1));
  strcpy((regstr) (*d)->data, name);
  
}

void
assign_ref (data** d, registry* reg, const char* name)
{
  *d = malloc(sizeof(data));
  (*d)->type = REFERENCE;
  (*d)->data = malloc(sizeof(ref));
  ((ref*) (*d)->data)->key = malloc(sizeof(char)*(strlen(name)+1));
  strcpy(((ref*) (*d)->data)->key, name);
  ((ref*) (*d)->data)->reg = reg;
  
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
  del(reg,name,1);
  reg = head(reg);
  registry* new_reg = malloc(sizeof(registry));
  new_reg->left = reg;
  new_reg->right = NULL;
  new_reg->up = reg->up;
  new_reg->value = d;
  reg->right = new_reg;

  new_reg->key = malloc(sizeof(char)*(strlen(name)+1));
  strcpy(new_reg->key, name);

}

data*
get (registry* reg, const char* name, int recursive)
{
  if (reg == NULL)
    return NULL;

  if (is_init_reg(reg))
    {
      if (recursive)
        {
          return get(reg->up, name, recursive);
        }
      else
        {
          return NULL;
        }
    }

  reg = tail(reg);
  registry* cur = reg;

  while (cur != NULL)
    {
      if (strcmp(cur->key, name)==0)
        {
          return cur->value;
        }
      cur = cur->right;
    }
  if ((reg->up != NULL) && recursive)
    {
      if (reg->up->up != NULL)
        {
          if (reg->up == reg->up->up)
            printf("HERE!\n");
        }
      return get(reg->up, name, recursive);
    }
  else
    {
      return NULL;
    }
}

data*
lookup (registry* reg, const char* name, int recursive)
{
  data* d = get(reg, name, recursive);

  if (d == NULL)
    {
      printf("Actually not found!\n");
      return NULL;
    }

  if (d->type == ACTIVE_INSTRUCTION && (reg->up != NULL))
    {
      execute_code((statement*) d->data, reg);
      d = get(reg, "ans", 0);

    }
  else if (d->type == REFERENCE)
    {
      if (strcmp(((ref*) d->data)->key, "data")==0)
        {
          d = top_registry;
        }
      else if (strcmp(((ref*) d->data)->key, "up")==0)
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
	      d_ref = get(reg,
			  ((ref*) d->data)->key,
			  1);
	    }
	  else
	    {
	      d_ref = get(((ref*) d->data)->reg,
			  ((ref*) d->data)->key,
			  1);
	    }

          if (d_ref == NULL)
            {
              char* msg = malloc(sizeof(char)*
                                 (strlen("Reference not found.") +
                                  strlen(((ref*) d->data)->key) +
                                  5));
              sprintf(msg, "Reference `%s` not found.",
                      ((ref*) d->data)->key);
              do_error(msg);
              free(msg);
              d = NULL;
            }
          else
            {
              d = d_ref;
            }
        }
    }

  return d;
}

void
relabel (registry* reg, const char* name, const char* new_name)
{
  registry* cur = tail(reg);

  while (cur != NULL)
    {
      if (strcmp(cur->key, name)==0)
        {
          free(cur->key);
          cur->key = malloc(sizeof(char)*(strlen(new_name)+1));
          strcpy(cur->key, new_name);
          return;
        }
      cur = cur->right;
    }
}

void
mov (registry* reg, const char* name, const char* new_name)
{
  registry* cur = tail(reg);
  while (cur != NULL)
    {
      if (strcmp(cur->key,name)==0)
        {
          del(reg,new_name,1);
          free(cur->key);
          cur->key = malloc(sizeof(char)*(strlen(new_name)+1));
          strcpy(cur->key, new_name);
          return;
        }
      cur = cur->right;
    }
}

                                 

void
del (registry* reg, const char* name, int del_data)
{
  registry* cur;

  cur = tail(reg);

  while (cur != NULL)
    {
      if (strcmp(name,cur->key)==0)
        {
          if (cur->right != NULL)
            {
              cur->right->left = cur->left;
            }
          if (cur->left != NULL)
            {
              cur->left->right = cur->right;
            }

          if (del_data)
            free_data(cur->value);
          
          free(cur->key);
          free(cur);
          return;
        }

      cur = cur->right;
    }
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
  data* d;

  switch (d_in->type)
    {
    case INTEGER:
      assign_int(&d, *((int*) d_in->data));
      break;
    case DECIMAL:
      assign_dec(&d, *((double*) d_in->data));
      break;
    case STRING:
      assign_str(&d, (const char*) d_in->data);
      break;
    case REGISTER:
      assign_regstr(&d, (regstr) d_in->data);
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
                 ((ref*) d_in->data)->key);
      break;
    }

  return d;
}

void
compute (registry* reg)
{
  data* arg = lookup(reg,"#0",0);
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
      printf("In compute! Before Statement!\n");
      execute_code(((instruction*) arg->data)->stmt, reg);
    }

  if (is_error(-1))
    {
      return;
    }

  ((operation) arg->data)(reg);
  
}
