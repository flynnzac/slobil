/* 
   SLOBIL is a Object Based Environment and Language
   Copyright 2021 Zach Flynn <zlflynn@gmail.com>

   This file is part of SLOBIL.

   SLOBIL is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   SLOBIL is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with SLOBIL (in COPYING file).  If not, see <https://www.gnu.org/licenses/>.
   
*/

/**
 *  @file arg.c
 *  @brief Functions for argument lists
 *  @author Zach Flynn (flynnzac)
 */

#include "slobil.h"

/**
 * Generates an "arg", an argument object, of a given length.
 *
 * @param length the length of the arg object
 * @param def_free whether to free the data in the array when done
 * @return returns a new arg object of given length with given free options
 */
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


/**
 * Shifts an arg object arguments left, returning a new arg object
 *
 * @param a the arg object to shift left
 * @return an arg object without the first argument of \c a
 */
arg
shift_arg_left(arg a)
{
  arg b;

  b.arg_array = malloc(sizeof(data*)*(a.length-1));
  for (size_t i=1; i < a.length; i++)
    b.arg_array[i-1] = a.arg_array[i];

  b.length = a.length - 1;
  b.free_data = a.free_data+1;
  return b;
}


/**
 * Returns the data object unless the data is an Expression, in which case it is evaluated and the \c /ans slot is returned.
 *
 * @param arg a \c data object to resolve
 * @param obj the object to evaluate the data in if it is an Expression
 * @return Either the data object or what the Expression set the \c /ans slot to.
 */
 
data*
resolve (data* arg, object* obj)
{
  if (arg->type == Expression)
    {
      execute_0(arg, obj);
      return get(obj, &obj->task->task->slobil_slot_ans, 0);
    }
  else
    {
      return arg;
    }
}

/**
 * Checks argument length and sets error status if the argument does not have at least a given length
 * @param a a pointer to an argument object to check the length of
 * @param length the length to make sure the argument object has at least this length
 * @param op a null-terminated string giving the name of the operation being called (for printing informative errors)
 * @param t a pointer to a task_vars object to log the error to the correct task
 */
void
check_length (arg* a, int length, char* op, task_vars* t)
{
  if (a==NULL)
    {
      do_error("NULL argument object.", t);
      return;
    }
  
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

/**
 * Fills an object from an argument list that goes slot value slot value ...
 *
 * @param a the argument list
 * @param obj a pointer to the object
 * @param arg_start the location in the argument list to start reading slot value pairs
 * @param t a task vars object for assigning errors
 * @return false on success and true on failure (because the arg list does not follow slot value slot value ... pattern)
 */
bool
object_from_args (arg a, object* obj, int arg_start, task_vars* t)
{
  data* d = NULL;
  data* d_data = NULL;

  for (int i=arg_start; i < a.length; i=i+2)
    {
      d = a.arg_array[i];
      if (d->type != Slot)
        {
          do_error("Expected a slot", t);
          return true;
        }
      d_data = a.arg_array[i+1];
      content* c = set(obj, d_data, ((slot*) d->data)->name, 1);
      c->do_not_free_data = 1;
    }

  return false;
}
