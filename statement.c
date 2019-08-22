#include "arbel.h"

element*
append_literal_element (element* current, data* d)
{
  element* e = malloc(sizeof(element));

  e->data = d;
  e->name = NULL;
  e->literal = 1;
  e->right = NULL;
  
  if (current != NULL)
    {
      current->right = e;
    }

  return e;
}

element*
append_argument_element (element* current, char* name)
{
  element* e = malloc(sizeof(element));
  e->data = NULL;
  e->name = name;
  e->literal = 0;
  e->right = NULL;

  if (current != NULL)
    {
      current->right = e;
    }
  return e;
}

statement*
append_statement (statement* current, element* head)
{
  statement* s = malloc(sizeof(statement));
  s->right = NULL;
  s->head = head;
  if (current != NULL)
    {
      current->right = s;
    }

  return s;
}

void
execute_statement (statement* s, registry* reg)
{
  registry* arg_reg = new_registry(reg);
  element* e = s->head;
  char* name = NULL;
  int arg_n = 0;
  data* d;
  while (e != NULL)
    {
      name = argument_name(arg_n);
      if (e->literal)
        {
          d = copy_data(e->data);
          set(arg_reg, d, name);
        }
      else
        {
          d = get(reg, e->name, 1);
          d = copy_data(d);
          set(arg_reg, d, name);
        }

      free(name);
      arg_n++;
      e = e->right;
    }
  compute(arg_reg);
  free_registry(arg_reg);
}

void
execute_code (statement* s, registry* reg)
{
  while (s != NULL)
    {
      execute_statement(s, reg);
      s = s->right;
    }
}
