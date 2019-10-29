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
  registry* arg_reg = new_registry(NULL);
  registry* reg = arg_reg;
  element* e1 = e;

  while (e1 != NULL)
    {
      n++;
      e1 = e1->right;
    }
  *hash_bin = malloc(sizeof(unsigned long)*n);
  *location = malloc(sizeof(size_t)*n);
  n = 0;

  while (e != NULL)
    {
      name = argument_name(n);
      hash_name = hash_str(name);
      (*hash_bin)[n] = hash_name % ARBEL_HASH_SIZE;

      content* c;
      if (reg->objects[hash_name % ARBEL_HASH_SIZE] == NULL)
        {
          reg->objects[hash_name % ARBEL_HASH_SIZE] = new_content();
          c = reg->objects[hash_name % ARBEL_HASH_SIZE];
        }
      else
        {
          c = head(reg->objects[hash_name % ARBEL_HASH_SIZE]);
        }
      content* cprime = reg->objects[hash_name % ARBEL_HASH_SIZE];
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
  s->arg_reg = gen_arg_reg(head, &s->hash_bins, &s->location);
  if (current != NULL)
    {
      current->right = s;
    }

  return s;
}

void
execute_statement (statement* s, registry* reg)
{
  registry* arg_reg = s->arg_reg;
  arg_reg->up = reg;

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
              if (arg_n > 0 && d != NULL)
                d = copy_data(d);
              
            }
        }

      if (!is_error(-1))
        {
          content* c = right_n(arg_reg->objects[s->hash_bins[arg_n]],
                               s->location[arg_n]);


          if (c->value != NULL && !c->do_not_free_data)
            {
              free_data(c->value);
              c->value = NULL;
            }
          
          c->value = d;
          
          if (e->literal || (arg_n == 0 && (d->type == INSTRUCTION ||
                                            d->type == OPERATION)))
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

      if (is_error(-1)) break;
    }

  if (!is_error(-1))
    compute(arg_reg);


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
