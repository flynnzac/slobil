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
str_shift_left (char* buffer)
{
  int i;
  for (i=0; i <= strlen(buffer); i++)
    {
      buffer[i] = buffer[i+1];
    }
}

char*
append_nl (char* str)
{
  size_t len = strlen(str);
  #ifdef GARBAGE
  #undef realloc
  #endif
  str = realloc(str, sizeof(char)*(len+2));
  #ifdef GARBAGE
  #define realloc(x,y) GC_REALLOC(x,y)
  #endif
  str[len] = '\n';
  str[len+1] = '\0';

  return str;
  
}


char*
escape_str (char* str)
{
  char* dest = str;
  int escape = 0;
  int i;
  int j = 0;
  for (i=0; i < strlen(str); i++)
    {
      if (str[i] == '\\' && escape == 0)
        {
          escape = 1;
        }
      else if (escape == 1)
        {
          escape = 0;
          switch (str[i])
            {
            case '\\':
              dest[j] = '\\';
              j++;
              break;
            case '\'':
              dest[j] = '\'';
              j++;
              break;
            case 't':
              dest[j] = '\t';
              j++;
              break;
            case 'n':
              dest[j] = '\n';
              j++;
              break;
            case 'r':
              dest[j] = '\r';
              j++;
              break;
            default:
              dest[j] = '\\';
              j++;
              dest[j] = str[i];
              j++;
              break;
            }
        }
      else if (str[i] == '\'')
        {
          dest[j] = '"';
          j++;
        }
      else
        {
          dest[j] = str[i];
          j++;
        }
    }

  dest[j] = '\0';

  return dest;

}

registry*
shift_list_down (registry* reg)
{
  registry* shifted_reg = copy_registry(reg);
  int num = 0;
  regstr old;
  regstr new;

  del(shifted_reg, arbel_hash_0, 1);

  old.name = NULL;
  new.name = NULL;
  
  do
    {
      num++;
      
      if (old.name != NULL)
        free(old.name);

      if (new.name != NULL)
        free(new.name);

      old.name = argument_name(num);
      new.name = argument_name(num-1);
      old.key = hash_str(old.name);
      new.key = hash_str(new.name);

    }
  while (mov(shifted_reg, &old, &new) != NULL);

  free(new.name);
  free(old.name);

  return shifted_reg;

}




