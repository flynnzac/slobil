/* 
   ARBEL is a Registry Based Environment and Language
   Copyright 2021 Zach Flynn <zlflynn@gmail.com>

   This file is part of ARBEL.

   ARBEL is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   ARBEL is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with ARBEL (in COPYING file).  If not, see <https://www.gnu.org/licenses/>.
   
*/

#include "arbel.h"

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
split_by_colon (const char* name, int* cnt, int** is_regstr)
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
  *is_regstr = malloc(sizeof(int)*(*cnt));
  int k = 0;
  for (i=0; i < strlen(name); i++)
    {
      if (name[i] == ':')
        {
          buffer[j] = '\0';
          result[k] = malloc(sizeof(char)*(strlen(buffer)+1));
          if (is_register(buffer))
            {
              (*is_regstr)[k] = 1;
              buffer++;
              strcpy(result[k], buffer);
              buffer--;
            }
          else
            {
              (*is_regstr)[k] = 0;
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
      if (is_register(buffer))
        {
          (*is_regstr)[k] = 1;
          buffer++;
          strcpy(result[k], buffer);
          buffer--;
        }
      else
        {
          (*is_regstr)[k] = 0;
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
copy_isregstr (int* is_regstr, int levels)
{
  int* copy = malloc(sizeof(int)*levels);
  int i;
  for (i=0; i < levels; i++)
    {
      copy[i] = is_regstr[i];
    }
  return copy;
}

