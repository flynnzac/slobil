/* 
   WOB is a REGISTER BASED ENVIRONMENT AND LANGUAGE
   Copyright 2019 Zach Flynn

   This file is part of WOB.

   WOB is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   WOB is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with WOB (in COPYING file).  If not, see <https://www.gnu.org/licenses/>.
   
*/

#include "wob.h"

int
is_error (int e, task_vars* t)
{
  if (e >= 0)
    {
      t->wob_error = e;
    }
  return t->wob_error;
}


void
do_error (const char* msg, task_vars* t)
{
  if (t->wob_print_error_messages)
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

