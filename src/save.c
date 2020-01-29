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
save_registry (FILE* f, registry* reg)
{
  for (int i = 0; i < reg->hash_size; i++)
    {
      save_content(f, reg->objects[i]);
    }

  return 0;
}


int
save_content (FILE* f, content* reg)
{

  reg = tail(reg);
  int size;

  while (reg != NULL)
    {
      if (reg->value->type != Operation &&
          reg->value->type != Active_Instruction &&
          reg->value->type != Nothing)
        {
          fwrite(&reg->value->type, sizeof(data_type), 1, f);
      
          switch (reg->value->type)
            {
            case Integer:
              fwrite(reg->value->data, sizeof(int), 1, f);
              break;
            case Real:
              fwrite(reg->value->data, sizeof(double), 1, f);
              break;
            case String:
              size = strlen((char*) reg->value->data);
              fwrite(&size, sizeof(int), 1, f);
              fwrite(reg->value->data, sizeof(char),
                     strlen((char*) reg->value->data), f);
              break;
            case Register:
              size = strlen(((regstr*) reg->value->data)->name);
              fwrite(&size, sizeof(int), 1, f);
              fwrite(reg->value->data, sizeof(char),
                     strlen(((regstr*) reg->value->data)->name), f);
              break;
            case Registry:
              save_registry(f, (registry*) reg->value->data);
              size = Nothing;
              fwrite(&size, sizeof(data_type), 1, f);
              break;
            case Instruction:
              size = strlen(((instruction*) reg->value->data)->code);
              fwrite(&size, sizeof(int), 1, f);
              fwrite(((instruction*) reg->value->data)->code, sizeof(char),
                     strlen(((instruction*) reg->value->data)->code), f);
              break;
            default:
              break;
            }

          size = strlen(reg->name);
          fwrite(&size, sizeof(int), 1, f);
          fwrite(reg->name, sizeof(char), strlen(reg->name), f);

        }
      
      reg = reg->right;

    }

  return 0;
}

int
read_registry (FILE* f, registry* reg)
{
  data_type* type_cache = malloc(sizeof(data_type));
  void* cache;
  data* d;
  registry* r;
  int size;
  statement* stmt = NULL;
  parser_state state;
  FILE* f_sub;
  while (fread(type_cache, sizeof(data_type), 1, f)
         && (*type_cache != Nothing))
    {
      switch (*type_cache)
        {
        case Integer:
          cache = malloc(sizeof(int));
          fread(cache, sizeof(int), 1, f);
          assign_int(&d, *((int*) cache));
          free(cache);
          break;
        case Real:
          cache = malloc(sizeof(double));
          fread(cache, sizeof(double), 1, f);
          assign_real(&d, *((double*) cache));
          free(cache);
          break;
        case String:
          cache = malloc(sizeof(int));
          fread(cache, sizeof(int), 1, f);
          size = *((int*) cache);
          free(cache);
          cache = malloc(sizeof(char)*(size+1));
          fread(cache, sizeof(char), size, f);
          *((char*) (cache+size)) = '\0';
          assign_str(&d, (char*) cache, 0);
          break;
        case Register:
          cache = malloc(sizeof(int));
          fread(cache, sizeof(int), 1, f);
          size = *((int*) cache);
          free(cache);
          cache = malloc(sizeof(char)*(size+1));
          fread(cache, sizeof(char), size, f);
          *((char*) (cache+size)) = '\0';
          assign_regstr(&d, (char*) cache, hash_str((char*) cache));
          free(cache);          
          break;
        case Registry:
          r = new_registry(reg, ARBEL_HASH_SIZE);
          read_registry(f, r);
          assign_registry(&d, r, false);
          break;
        case Instruction:
          cache = malloc(sizeof(int));
          fread(cache, sizeof(int), 1, f);
          size = *((int*) cache);
          free(cache);
          
          cache = malloc(sizeof(char)*(size+1));
          fread(cache, sizeof(char), size, f);
          ((char*) cache)[size] = '\0';
          char* code = malloc(sizeof(char)*(size+1));
          strcpy(code, (char*) cache);
          
          f_sub = fmemopen(cache, sizeof(char)*size, "r");
          state = fresh_state(0);
          stmt = NULL;
          parse(f_sub, &state, &stmt);
          fclose(f_sub);

          d = new_data();
          d->type = Instruction;
          d->data = malloc(sizeof(instruction));
          ((instruction*) d->data)->stmt = stmt;
          ((instruction*) d->data)->code = code;
          stmt = NULL;
          free(cache);
          break;
        default:
          break;
        }
      cache = malloc(sizeof(int));
      fread(cache, sizeof(int), 1, f);
      size = *((int*) cache);
      free(cache);
      cache = malloc(sizeof(char)*(size+1));
      fread(cache, sizeof(char), size, f);
      *((char*) (cache+size)) = '\0';

      if (d != NULL)
        set(reg, d, (char*) cache, 1);
      
      free(cache);
    }

  free(type_cache);
      
  return 0;
  
}
