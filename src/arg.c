#include "arbel.h"

arg
gen_arg (int length, int def_free)
{
  arg a;
  a.length = length;
  a.free_data = malloc(length*sizeof(int));
  a.arg_array = malloc(length*sizeof(data*));

  for (int i=0; i < length; i++)
    a.free_data[i] = def_free;

  return a;
}


data*
resolve_reference (data* d, registry* reg)
{
  data* d_ref;
  if (((ref*) d->data)->reg == NULL)
    {
      d_ref = get_by_levels(reg,
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
    }
  else
    {
      if (d_ref->type == REGISTRY)
        {
          ((registry*) d_ref->data)->up = reg;
        }
    }

  return d_ref;
}


data*
resolve (data* arg, registry* reg)
{
  if (arg->type == ACTIVE_INSTRUCTION)
    {
      execute_code(((instruction*) arg->data)->stmt, reg);
      return get(reg, arbel_hash_ans, 0);
    }
  else
    {
      return arg;
    }
}

void
check_length (arg* a, int length)
{
  if (a->length < length)
    {
      char* error_msg = malloc
        (sizeof(char)*
         (strlen("Number of arguments is less than ") +
          floor(log10(length-1))+1 + strlen(".") + 1));

      sprintf(error_msg, "Number of arguments is less than %d.",
              length-1);
          
      do_error(error_msg);
      free(error_msg);
    }
}
