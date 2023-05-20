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

#include "slobil.h"


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

void
slobil_location (mpz_t loc, const int n)
{
  if (mpz_cmp_si(loc,0) <= 0)
    {
      mpz_add_ui(loc, loc, n);
    }
  if ((mpz_cmp_si(loc,0) > 0) && (mpz_cmp_si(loc,n) <= 0))
    {
    }
  else
    {
      mpz_set_si(loc, -1);
    }
}

int
digits (int n)
{
  if (n==0)
    return 1;
  else
    return floor(log10(n))+1;
}

slot
make_slot(char* name)
{
  slot sl;
  sl.name = name;
  sl.key = hash_str(name);
  return sl;
}
  
