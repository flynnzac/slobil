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

registry*
gen_arg_reg (element* e, unsigned long** hash_bin, size_t** location)
{
  int n = 0;
  char* name;
  unsigned long hash_name;
  element* e1 = e;

  while (e1 != NULL)
    {
      n++;
      e1 = e1->right;
    }
  *hash_bin = malloc(sizeof(unsigned long)*n);
  *location = malloc(sizeof(size_t)*n);
  n = 0;

  registry* arg_reg = new_registry(NULL, new_hash_size(n));
  registry* reg = arg_reg;


  while (e != NULL)
    {
      name = argument_name(n);
      hash_name = hash_str(name);
      (*hash_bin)[n] = hash_name % reg->hash_size;

      content* c;
      if (reg->objects[hash_name % reg->hash_size] == NULL)
        {
          reg->objects[hash_name % reg->hash_size] = new_content();
          c = reg->objects[hash_name % reg->hash_size];
        }
      else
        {
          c = head(reg->objects[hash_name % reg->hash_size]);
        }
      content* cprime = reg->objects[hash_name % reg->hash_size];
      (*location)[n] = 1;
      while (cprime->right != NULL)
        {
          (*location)[n]++;
          cprime = cprime->right;
        }
      
      c->right = new_content();
      c->right->left = c;
      c->right->name = name;
      c->right->key = hash_name;
      c->right->value = NULL;
      e = e->right;

      n++;
    }

  return arg_reg;
  
}


statement*
append_statement (statement* current, element* head)
{
  statement* s = malloc(sizeof(statement));
  s->right = NULL;
  s->head = head;
  s->arg_reg = NULL;
  s->location = NULL;
  s->hash_bins = NULL;
  if (current != NULL)
    {
      current->right = s;
    }
  
  element* e = s->head;
  size_t i = 0;
  while (e != NULL)
    {
      i++;
      e = e->right;
    }
  s->arg.length = i;
  s->arg.free_data = malloc(sizeof(int)*i);
  s->arg.arg_array = malloc(sizeof(data*)*i);

  for (int j = 0; j < i; j++)
    {
      s->arg.arg_array[j] = NULL;
      s->arg.free_data[j] = 0;
    }

  return s;
}

void
execute_statement (statement* s, registry* reg)
{

  element* e = s->head;
  int arg_n = 0;
  data* d = NULL;
  while (e != NULL)
    {
      
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
                  do_error("Instruction in [] did not set /ans register.");
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
            }
        }

      if (!is_error(-1))
        {
          if (d != NULL && d->type == INSTRUCTION &&
	      ((instruction*) d->data)->being_called)
            {
              s->arg.arg_array[arg_n] = copy_data(d);
              s->arg.free_data[arg_n] = 1;
            }
          else if (d != NULL && e->statement)
            {
              s->arg.arg_array[arg_n] = d;
              s->arg.free_data[arg_n] = 1;
            }
          else
            {
              s->arg.arg_array[arg_n] = d;
              s->arg.free_data[arg_n] = 0;
            }
        }

      if (is_error(-1)) break;      
      arg_n++;
      e = e->right;

    }

  if (!is_error(-1))
    {
      compute(s->arg.arg_array[0], reg, s->arg);

    }
  
  free_arg_array_data(&s->arg, arg_n);

}

void
execute_code (statement* s, registry* reg)
{
  statement* stmt = s;
  int error = 0;
  while (stmt != NULL)
    {
      execute_statement(stmt, reg);
      error = is_error(-1) > error ? is_error(-1) : error;
      if (error >= arbel_stop_error_threshold)
        {
          break;
        }


      stmt = stmt->right;
    }

  is_error(error);
}
