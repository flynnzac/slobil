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

bool
is_integer (const char* str)
{
  int i;
  for (i=0; i < strlen(str); i++)
    {
      if (!(isdigit((int) str[i]) || ((i==0) && str[i]=='-')))
        return false;
    }
  return strcmp(str, "-") != 0;
}

bool
is_real (const char* str)
{
  int i;
  int decimals = 0;
  for (i=0; i < strlen(str); i++)
    {
      if (!(isdigit((int) str[i]) || ((i==0) && str[i]=='-') ||
            str[i] == '.'))
        return false;

      if (str[i] == '.' && decimals == 1)
        return false;

      if (str[i] == '.')
        decimals++;
    }
  return decimals==1;
}


bool
is_numeric (data* d)
{
  if (d->type == Real ||
      d->type == Integer)
    {
      return true;
    }
  else
    {
      return false;
    }
}

bool
is_symbol (const char* str)
{
  return (str[0] == '/');
}


bool
is_whitespace (const char c)
{
  if ((c == ' ') || (c == '\t') || (c == '\n') || (c == '\r'))
    return true;
  else
    return false;
}

bool
is_boolean (const char* str)
{
  return (strcmp(str,"True") == 0) || (strcmp(str, "False") == 0);
}

bool
is_nothing (const char* str)
{
  return strcmp(str, "Nothing") == 0;
}

const char*
str_type (data_type type)
{
  const char* s = "";

  switch (type)
    {
    case Integer:
      s = "Integer";
      break;
    case Real:
      s = "Real";
      break;
    case String:
      s = "String";
      break;
    case Symbol:
      s = "Symbol";
      break;
    case Object:
      s = "Object";
      break;
    case Instruction:
      s = "Instruction";
      break;
    case Expression:
      s = "Expression";
      break;
    case Operation:
      s = "Operation";
      break;
    case Nothing:
      s = "Nothing";
      break;
    case File:
      s = "File";
      break;
    case Boolean:
      s = "Boolean";
      break;
    }

  return s;
}
