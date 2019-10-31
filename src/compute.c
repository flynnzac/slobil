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
}
  
void
compute (data* cmd, registry* reg)
{
  if (cmd == NULL)
    {
      do_error("Cannot compute without an operation or instruction at #0.");
      return;
    }
  
  switch (cmd->type)
    {
    case OPERATION:
      ((operation) cmd->data)(reg);
      break;
    case INSTRUCTION:
      execute_code(((instruction*) cmd->data)->stmt, reg);
      break;
    default:
      do_error("Tried to compute something that is not an operation or instruction.");
      break;
    }

}

