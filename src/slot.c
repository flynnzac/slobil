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

/**
 * @file register.c
 * @brief Functions for manipulating slots.
 */

char*
argument_name (int n)
{
  int n_digits;
  if (n==0)
    {
      n_digits = 1;
    }
  else if (n < 0)
    {
      n_digits = floor(log10(abs(n))) + 2;
    }
  else
    {
      n_digits = floor(log10(n)) + 1;
    }
  char* name = malloc(sizeof(char)*(n_digits+2));
  sprintf(name, "t%d", n);
  return name;
}

char*
custom_argument_name (int n, char* s)
{
  int n_digits;
  if (n==0)
    {
      n_digits = 1;
    }
  else if (n < 0)
    {
      n_digits = floor(log10(abs(n))) + 2;
    }
  else
    {
      n_digits = floor(log10(n)) + 1;
    }
  char* name = malloc(sizeof(char)*(n_digits+strlen(s)+1));
  sprintf(name, "%s%d", s, n);
  return name;
}


char*
vector_name (const char* lead, int n)
{
  int n_digits;
  if (n==0)
    {
      n_digits = 1;
    }
  else if (n < 0)
    {
      n_digits = floor(log10(n)) + 2;
    }
  else
    {
      n_digits = floor(log10(n)) + 1;
    }
  char* name = malloc(sizeof(char)*(strlen(lead)+n_digits+1));
  sprintf(name, "%s%d", lead, n);
  return name;
}

unsigned long
hash_str(const char *str)
{
  if (str==NULL) return 0;
  unsigned long hash = 5381;
  int c;

  while ((c = *str++))
    hash = ((hash << 5) + hash) + c; /* hash * 33 + c */

  return hash;
}

char**
split_by_colon (const char* name, int* cnt, int** is_symb)
{
  int i;
  *cnt = 1;
  int j = 0;

  for (i=0; i < strlen(name); i++)
    {
      if (name[i] == ':')
        {
          (*cnt) += 1;
        }
    }
  
  char** result = malloc((*cnt)*sizeof(char*));
  char* buffer = malloc(sizeof(char)*(strlen(name)+1));
  *is_symb = malloc(sizeof(int)*(*cnt));
  int k = 0;
  for (i=0; i < strlen(name); i++)
    {
      if (name[i] == ':')
        {
          buffer[j] = '\0';
          result[k] = malloc(sizeof(char)*(strlen(buffer)+1));
          if (is_slot(buffer))
            {
              (*is_symb)[k] = 1;
              buffer++;
              strcpy(result[k], buffer);
              buffer--;
            }
          else
            {
              (*is_symb)[k] = 0;
              strcpy(result[k], buffer);
            }
          k++;
          j = 0;
        }
      else
        {
          buffer[j] = name[i];
          j++;
        }
    }

  buffer[j] = '\0';

  if (strlen(buffer) != 0)
    {
      result[k] = malloc(sizeof(char)*(strlen(buffer)+1));
      if (is_slot(buffer))
        {
          (*is_symb)[k] = 1;
          buffer++;
          strcpy(result[k], buffer);
          buffer--;
        }
      else
        {
          (*is_symb)[k] = 0;
          strcpy(result[k], buffer);
        }

    }

  free(buffer);
  return result;
  
}

char**
copy_names (char** name, int levels)
{
  char** copy = malloc(sizeof(char*)*levels);
  int i;
  for (i=0; i < levels; i++)
    {
      copy[i] = malloc(sizeof(char)*(strlen(name[i])+1));
      strcpy(copy[i], name[i]);
    }
  return copy;
}
  

unsigned long*
copy_hashes (unsigned long* hashes, int levels)
{
  unsigned long* copy = malloc(sizeof(unsigned long)*levels);
  int i;
  for (i=0; i < levels; i++)
    {
      copy[i] = hashes[i];
    }
  return copy;
}

int*
copy_isslot (int* is_symb, int levels)
{
  int* copy = malloc(sizeof(int)*levels);
  int i;
  for (i=0; i < levels; i++)
    {
      copy[i] = is_symb[i];
    }
  return copy;
}

