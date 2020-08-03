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
              do_error("Literal not found.  This is a bug, please report to http://github.com/flynnzac/arbel .",
                       reg->task->task);
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
                  do_error("Instruction in [] did not set /ans register.",
                           reg->task->task);
                }
              else 
                {
                  del(reg, arbel_hash_ans, 0, false);
                  /* mark_do_not_free(reg, arbel_hash_ans); */
                }
            }
          else
            {
              d = get_by_levels(reg,
                                e->hash_name, e->levels, e->is_regstr,
                                e->name);
            }
        }

      if (!is_error(-1, reg->task->task))
        {
          if (d != NULL && d->type == Instruction &&
              ((instruction*) d->data)->being_called && (arg_n == 0))
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
      else
	break;

      arg_n++;
      e = e->right;

    }

  if (!is_error(-1, reg->task->task))
    compute(s->arg.arg_array[0], reg, s->arg);
  
  free_arg_array_data(&s->arg, arg_n);

  if (is_error(-1, reg->task->task))
    {
      data* err;
      mpz_t err_z;
      mpz_init_set_si(err_z, is_error(-1, reg->task->task));
      assign_int(&err, err_z);
      set(reg, err, "error-code", 0);
    }
  else
    {
      data* err;
      mpz_t err_z;
      mpz_init_set_si(err_z, 0);
      assign_int(&err, err_z);
      set(reg, err, "error-code", 0);
    }
	

}

void
execute_code (statement* s, registry* reg)
{
  statement* stmt = s;
  int error = 0;
  while (stmt != NULL)
    {
      execute_statement(stmt, reg);
      error = is_error(-1, reg->task->task) > error ? is_error(-1, reg->task->task) : error;
      if (reg->task->task->arbel_stop_error_threshold > 0 &&
          (error >= reg->task->task->arbel_stop_error_threshold))
	{
	  printf("-> ");
	  print_statement(stmt);
	  printf("\n");
	  break;
	}
      else
        is_error(0, reg->task->task);

      stmt = stmt->right;
    }

  is_error(error, reg->task->task);

}
