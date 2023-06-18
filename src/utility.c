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
  
