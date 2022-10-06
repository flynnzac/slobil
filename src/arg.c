/* 
   ARBEL is a Registry Based Environment and Language
   Copyright 2021 Zach Flynn <zlflynn@gmail.com>

   This file is part of ARBEL.

   ARBEL is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   ARBEL is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with ARBEL (in COPYING file).  If not, see <https://www.gnu.org/licenses/>.
   
*/

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

arg
shift_arg_right(arg a)
{
  arg b;

  b.arg_array = malloc(sizeof(data*)*(a.length-1));
  b.free_data = malloc(sizeof(int)*(a.length-1));
  for (size_t i=1; i < a.length; i++)
    b.arg_array[i-1] = a.arg_array[i];

  b.length = a.length - 1;
  b.free_data = a.free_data+1;
  return b;
}

data*
resolve (data* arg, registry* reg)
{
  if (arg->type == Expression)
    {
      ((instruction*) arg->data)->being_called = true;
      execute_code(((instruction*) arg->data)->stmt, reg);
      ((instruction*) arg->data)->being_called = false;
      return get(reg, reg->task->task->arbel_hash_ans, 0);
    }
  else
    {
      return arg;
    }
}

void
check_length (arg* a, int length, char* op, task_vars* t)
{
  if (a->length < length)
    {
      char* error_msg = malloc
        (sizeof(char)*
         (strlen("Number of arguments to < > is less than .") +
          floor(log10(length-1)+1) + 
          strlen(op) + 1));

      sprintf(error_msg, "Number of arguments to <%s> is less than %d.",
              op, length-1);
          
      do_error(error_msg, t);
      free(error_msg);
    }
}
