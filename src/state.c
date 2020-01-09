/* 
   ARBEL is a REGISTER BASED ENVIRONMENT AND LANGUAGE
   Copyright 2019 Zach Flynn

   This file is part of ARBEL.

   ARBEL is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   ARBLE is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with ARBEL (in COPYING file).  If not, see <https://www.gnu.org/licenses/>.
   
*/

#include "arbel.h"

int
is_error (int e)
{
  if (e >= 0)
    {
      arbel_error = e;
    }
  return arbel_error;
}


void
do_error (const char* msg)
{
  if (arbel_print_error_messages)
    fprintf(stderr, "Error: %s\n", msg);
  
  (void) is_error(1);
}

int
is_exit (int e)
{
  static int do_exit = 0;

  if (e >= 0)
    {
      do_exit = e;
    }
  return do_exit;
}

