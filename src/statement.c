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
  e->s = NULL;
  e->hash_name = NULL;
  e->levels = 0;
  e->is_regstr = NULL;
  
  if (current != NULL)
    {
      current->right = e;
    }

  return e;
}

element*
append_argument_element (element* current, char** name,
                         unsigned long* hash_name,
                         const int levels,
                         int* is_regstr)
{
  element* e = malloc(sizeof(element));
  e->data = NULL;
  e->name = name;
  e->literal = 0;
  e->statement = 0;
  e->right = NULL;
  e->s = NULL;
  e->hash_name = hash_name;
  e->levels = levels;
  e->is_regstr = is_regstr;
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
  e->hash_name = NULL;
  e->levels = 0;
  e->is_regstr = NULL;

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
  int arg_n = 0;
  data* d = NULL;
  char* arg_name = NULL;
  while (e != NULL)
    {
      arg_name = argument_name(arg_n);
      if (e->literal)
        {
          if (e->data == NULL)
            {
              do_error("Literal not found.  This is a bug, please report to http://github.com/flynnzac/arbel .");
            }
          else
            {
              d = e->data;
            }
        }
      else
        {
          if (e->statement)
            {
              execute_code(e->s, reg);
              d = get(reg, arbel_hash_ans, 0);
              if (d == NULL)
                {
                  do_error("Instruction in [] did not set $ans register.");
                }
              else
                {
                  mark_do_not_free(reg, arbel_hash_ans);
                }
            }
          else
            {
              d = get_by_levels(reg,
                                e->hash_name, e->levels, e->is_regstr,
                                e->name);
              if (arg_n > 0 && d != NULL)
                d = copy_data(d);
              
            }
        }

      if (!is_error(-1))
        {
          content* c = set(arg_reg, d, arg_name);

          if (e->literal || e->statement || arg_n == 0)
            {
              c->do_not_free_data = 1;
            }
          else
            {
              c->do_not_free_data = 0;
            }
        }

      arg_n++;
      e = e->right;
      free(arg_name);
      if (is_error(-1)) break;
    }

  if (!is_error(-1))
    compute(arg_reg);


  free_registry(arg_reg);

}

void
execute_code (statement* s, registry* reg)
{
  statement* stmt = s;
  while (stmt != NULL)
    {
      execute_statement(stmt, reg);
      if (is_error(-1))
        {
          break;
        }
      stmt = stmt->right;
    }
}
