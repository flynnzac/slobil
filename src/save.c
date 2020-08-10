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
  op_wrapper* op;

  while (reg != NULL)
    {
      if ((reg->value->type != Active_Instruction) &&
          (reg->value->type != File) &&
          ((reg->value->type != Operation) || ((op_wrapper*) reg->value->data)->instr != NULL))
        {
          fwrite(&reg->value->type, sizeof(data_type), 1, f);
      
          switch (reg->value->type)
            {
            case Integer:
              {
                mpz_t* z = (mpz_t*) reg->value->data;
                size = mpz_sizeinbase(*z, 10)+2;
                char* s = malloc(size);
                mpz_get_str(s, 10, *z);
                size = strlen(s);
                fwrite(&size, sizeof(int), 1, f);
                fwrite(s, sizeof(char), strlen(s), f);
                free(s);
              }
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
              size = NotAType;
              fwrite(&size, sizeof(data_type), 1, f);
              break;
            case Instruction:
              size = strlen(((instruction*) reg->value->data)->code);
              fwrite(&size, sizeof(int), 1, f);
              fwrite(((instruction*) reg->value->data)->code, sizeof(char),
                     strlen(((instruction*) reg->value->data)->code), f);
              break;
            case Operation:
              op = ((op_wrapper*) reg->value->data);
              if (op->instr == NULL)
                break;
              size = strlen(((instruction*) op->instr->data)->code);

              fwrite(&size, sizeof(int), 1, f);
              fwrite(((instruction*) op->instr->data)->code, sizeof(char), size, f);
              fwrite(&(op->n_arg), sizeof(int), 1, f);

              for (int i=0; i < op->n_arg; i++)
                {
                  size = strlen(((regstr*) op->args[i]->data)->name);
                  fwrite(&size, sizeof(int), 1, f);
                  fwrite(((regstr*) op->args[i]->data)->name,
                         sizeof(char), size, f);
                }
              break;
            case Task:
              {
                task* t = (task*) reg->value->data;
                save_registry(f, t->state);
                size = NotAType;
                fwrite(&size, sizeof(data_type),1,f);
                size = strlen(t->code->code);
                fwrite(&size, sizeof(int), 1, f);
                fwrite(t->code->code, sizeof(char), size, f);
              }
              break;
            case Boolean:
              fwrite(reg->value->data, sizeof(bool), 1, f);
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
  char* code;
  while (fread(type_cache, sizeof(data_type), 1, f)
         && (*type_cache != NotAType))
    {
      switch (*type_cache)
        {
        case Integer:
          {
            cache = malloc(sizeof(int));
            fread(cache, sizeof(int), 1, f);
            size = *((int*) cache);
            free(cache);
            cache = malloc(sizeof(char)*(size+1));
            fread(cache, sizeof(char), size, f);
            *((char*) (cache+size)) = '\0';
            mpz_t z;
            mpz_init_set_str(z, cache, 10);
            assign_int(&d, z);
            mpz_clear(z);
            free(cache);
          }
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
          r = new_registry(reg, ARBEL_HASH_SIZE, reg->task);
          read_registry(f, r);
          assign_registry(&d, r, false, reg->task);
          break;
        case Instruction:
          cache = malloc(sizeof(int));
          fread(cache, sizeof(int), 1, f);
          size = *((int*) cache);
          free(cache);
          
          cache = malloc(sizeof(char)*(size+1));
          fread(cache, sizeof(char), size, f);
          ((char*) cache)[size] = '\0';
          code = malloc(sizeof(char)*(size+1));
          strcpy(code, (char*) cache);
          
          f_sub = fmemopen(cache, sizeof(char)*size, "r");
          state = fresh_state(0);
          stmt = NULL;
          parse(f_sub, &state, &stmt, reg->task);
          fclose(f_sub);

          d = new_data();
          d->type = Instruction;
          d->data = malloc(sizeof(instruction));
          ((instruction*) d->data)->stmt = stmt;
          ((instruction*) d->data)->code = code;
          stmt = NULL;
          free(cache);
          break;
        case Operation:
          cache = malloc(sizeof(int));
          fread(cache, sizeof(int), 1, f);
          size = *((int*) cache);
          free(cache);

          cache = malloc(sizeof(char)*(size+1));
          fread(cache, sizeof(char), size, f);
          ((char*) cache)[size] = '\0';
          code = malloc(sizeof(char)*(size+1));
          strcpy(code, (char*) cache);

          f_sub = fmemopen(cache, sizeof(char)*size, "r");
          state = fresh_state(0);
          stmt = NULL;
          parse(f_sub, &state, &stmt, reg->task);
          fclose(f_sub);

          op_wrapper* op = malloc(sizeof(op_wrapper));
          op->instr = new_data();
          op->instr->type = Instruction;
          op->instr->data = malloc(sizeof(instruction));
          ((instruction*) op->instr->data)->stmt = stmt;
          ((instruction*) op->instr->data)->code = code;
          stmt = NULL;
          free(cache);

          cache = malloc(sizeof(int));
          fread(cache, sizeof(int), 1, f);
          op->n_arg = *((int*) cache);
          free(cache);

          op->args = malloc(sizeof(data*)*(op->n_arg));

          for (int i = 0; i < op->n_arg; i++)
            {
              cache = malloc(sizeof(int));
              fread(cache, sizeof(int), 1, f);
              size = *((int*) cache);
              free(cache);

              char* name = malloc(sizeof(char)*(size+1));
              fread(name, sizeof(char), size, f);
              name[size] = '\0';

              unsigned long hash = hash_str(name);

              assign_regstr(&op->args[i], name, hash);
              free(name);
            }

          d = new_data();
          d->type = Operation;
          d->data = op;
          break;
        case Task:
          {
            task* t = malloc(sizeof(task));
            t->task = new_task(t);
            t->state = new_registry(t->task->current_parse_registry, ARBEL_HASH_SIZE, t);
            read_registry(f, t->state);
            cache = malloc(sizeof(int));
            fread(cache, sizeof(int), 1, f);
            size = *((int*) cache);
            free(cache);

            cache = malloc(sizeof(char)*(size+1));
            fread(cache, sizeof(char), size, f);
            ((char*) cache)[size] = '\0';
            code = malloc(sizeof(char)*(size+1));
            strcpy(code, (char*) cache);

            f_sub = fmemopen(cache, sizeof(char)*size, "r");
            state = fresh_state(0);
            stmt=NULL;
            parse(f_sub, &state, &stmt, t->task);
            fclose(f_sub);
            t->code = malloc(sizeof(instruction));
            t->code->stmt = stmt;
            t->code->code = code;
            stmt = NULL;
            free(cache);

            t->task->current_parse_registry = t->state;
            t->queued_instruction = new_registry(NULL, ARBEL_HASH_SIZE, t);
            t->pid = -1;
            t->thread = NULL;
            d = new_data();
            d->type = Task;
            d->data = t;
          }
          break;
        case Boolean:
          cache = malloc(sizeof(bool));
          fread(cache, sizeof(bool), 1, f);
          assign_boolean(&d, *(bool*) cache);
          free(cache);
          break;
        case Nothing:
          assign_nothing(&d);
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

void
save_outer (registry* reg, char* fname)
{
  FILE* f = fopen(fname, "wb");
  save_registry(f, reg);
  data_type end = NotAType;
  fwrite(&end, sizeof(data_type), 1, f);
  fclose(f);
}

  
