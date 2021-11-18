/* 

   ONBU is a Basic Registry and Interactive Programming Language and Environment
   Copyright 2021 Zach Flynn <zlflynn@gmail.com>

   This file is part of ONBU.

   ONBU is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   ONBU is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with ONBU (in COPYING file).  If not, see <https://www.gnu.org/licenses/>.
   
*/

#include "onbu.h"

int
is_error (int e, task_vars* t)
{
  if (e >= 0)
    {
      t->onbu_error = e;
    }
  return t->onbu_error;
}


void
do_error (const char* msg, task_vars* t)
{
  data* d = get(t->onbu_options,
                hash_str("print-errors"),
                0);
  bool print_error = true;
  if (!(d==NULL || d->type != Boolean))
    {
      print_error = *((bool*) d->data);
    }
  if (print_error)
    fprintf(stderr, "Error: %s\n", msg);
  
  (void) is_error(1, t);
}

int
is_exit (int e, task_vars* t)
{
  if (e >= 0)
    {
      t->do_exit = e;
    }
  return t->do_exit;
}

