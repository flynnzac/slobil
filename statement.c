#include "arbel.h"

element*
append_literal_element (element* current, data* d)
{
  element* e = malloc(sizeof(element));

  e->data = d;
  e->name = NULL;
  e->literal = 1;
  e->statement = 0;
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
  e->name = malloc(sizeof(char)*(strlen(name)+1));
  strcpy(e->name,name);
  e->literal = 0;
  e->statement = 0;
  e->right = NULL;

  if (current != NULL)
    {
      current->right = e;
    }
  return e;
}

element*
append_statement_element (element* current, statement* s)
{
  element* e = malloc(sizeof(element));
  e->data = NULL;
  e->name = NULL;
  e->s = s;
  e->literal = 0;
  e->statement = 1;
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
  registry* st_reg = NULL;
  element* e = s->head;
  char* name = NULL;
  int arg_n = 0;
  data* d;
  while (e != NULL)
    {
      name = argument_name(arg_n);
      printf("Statement: %d\n",e->statement);
      printf("Literal: %d\n",e->literal);
      if (e->literal)
        {
          d = copy_data(e->data);
        }
      else
        {
          if (e->statement)
            {
	      printf("Hello!\n");
              st_reg = new_registry(reg);
              execute_code(e->s, st_reg);
              d = get(st_reg, "ans", 0);
              /* need to check that the register is set */
              d = copy_data(d);
              free_registry(st_reg);
            }
          else
            {
              d = get(reg, e->name, 1);
              d = copy_data(d);
            }
        }

      set(arg_reg, d, name);

      free(name);
      arg_n++;
      e = e->right;
    }
  print_registry(arg_reg);
  compute(arg_reg);
  free_registry(arg_reg);
}

void
execute_code (statement* s, registry* reg)
{
  while (s != NULL)
    {
      execute_statement(s, reg);
      /* if (is_error(-1)) */
      /* 	break; */
      s = s->right;
    }
}
