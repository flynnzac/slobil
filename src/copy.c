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
  *d = malloc(sizeof(data));
  (*d)->type = REAL;
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
  (*d)->data = malloc(sizeof(instruction));
  ((instruction*) (*d)->data)->stmt = copy_statement(s);
}

void
assign_op (data** d, const operation op)
{
  *d = malloc(sizeof(data));
  (*d)->type = OPERATION;
  (*d)->data = op;
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
assign_ref (data** d, registry* reg,
            char** names,
            const unsigned long* keys,
            const int levels,
            const int* is_regstr)
{
  *d = malloc(sizeof(data));
  (*d)->type = REFERENCE;
  (*d)->data = malloc(sizeof(ref));
  ((ref*) (*d)->data)->reg = reg;
  ((ref*) (*d)->data)->name = malloc(sizeof(char*)*levels);
  ((ref*) (*d)->data)->key = malloc(sizeof(unsigned long)*levels);
  ((ref*) (*d)->data)->is_regstr = malloc(sizeof(int)*levels);
  ((ref*) (*d)->data)->levels = levels;

  int i;
  for (i=0; i < levels; i++)
    {
      ((ref*) (*d)->data)->name[i] = malloc(sizeof(char)*
                                            (strlen(names[i])+1));
      strcpy(((ref*) (*d)->data)->name[i], names[i]);
      ((ref*) (*d)->data)->key[i] = keys[i];
      ((ref*) (*d)->data)->is_regstr[i] = is_regstr[i];
    }
  
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
  registry* r1 = new_registry(r0->up);
  data* d;

  for (int i = 0; i < ARBEL_HASH_SIZE; i++)
    {
      content* cur = r0->objects[i];
      if (cur == NULL)
        continue;

      cur = tail(r0->objects[i]);
      while (cur != NULL)
        {
          d = copy_data(cur->value);
          set(r1, d, cur->name);
          cur = cur->right;
        }
    }

  return r1;
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
    case REAL:
      assign_real(&d, *((double*) d_in->data));
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
      assign_active(&d, ((instruction*) d_in->data)->stmt);
      break;
    case ARBEL_FILE:
      assign_file(&d, (FILE*) d_in->data);
      break;
    case NOTHING:
      assign_nothing(&d);
      break;
    case REFERENCE:
      assign_ref(&d,((ref*) d_in->data)->reg,
                 ((ref*) d_in->data)->name,
                 ((ref*) d_in->data)->key,
                 ((ref*) d_in->data)->levels,
                 ((ref*) d_in->data)->is_regstr);
      break;
    }

  return d;
}
