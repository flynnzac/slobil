/*
  SLOBIL
  Copyright 2023 Zach Flynn

  Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

  1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

  2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

  3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS “AS IS” AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   
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
is_slot (const char* str)
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
    case Slot:
      s = "Slot";
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
