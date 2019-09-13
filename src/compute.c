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

void
ret (registry* reg, data* d, const char* name)
{
  registry* up = reg->up;
  if (up != NULL)
    {
      set(up, d, name);
    }
  else
    {
      do_error("Cannot return from top-level.");
    }
}

void
ret_ans (registry* reg, data* d)
{
  if (d == NULL)
    return;
  
  ret(reg, d, "ans");
  if (!is_error(-1) && reg->up != NULL)
    {
      if (reg->up->up == NULL)
        is_retval(1);
    }
}
  
void
compute (registry* reg)
{
  data* arg = lookup(reg,arbel_hash_0,0);
  if (arg == NULL)
    {
      do_error("Cannot compute a registry without an instruction at #0.");
    }
  else if (arg->type != OPERATION && arg->type != INSTRUCTION)
    {

      do_error("Cannot compute a registry without an instruction at #0.");
    }

  if (is_error(-1))
    return;

  if (arg->type == INSTRUCTION)
    {
      execute_code(((instruction*) arg->data)->stmt, reg);
      data* d = get(reg, arbel_hash_ans, 0);
      if (d != NULL)
        {
          d = copy_data(d);
          /* mark_do_not_free(reg, arbel_hash_ans); */
          ret_ans(reg, d);
        }
      return;
    }

  if (is_error(-1))
    {
      return;
    }

  ((operation) arg->data)(reg);
  
}

