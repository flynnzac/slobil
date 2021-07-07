/* 
   WOB is a REGISTER BASED ENVIRONMENT AND LANGUAGE
   Copyright 2019 Zach Flynn

   This file is part of WOB.

   WOB is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   WOB is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with WOB (in COPYING file).  If not, see <https://www.gnu.org/licenses/>.
   
*/

#include "wob.h"
#include <inttypes.h>
#define BYT8 nbytes(8)
#define BYT(s) nbytes(8*s)

int
save_registry (gzFile f, registry* reg)
{
  for (int i = 0; i < reg->hash_size; i++)
    {
      save_content(f, reg->objects[i]);
    }

  return 0;
}

uint64_t
strlen8 (char* str, unsigned nbit)
{
  uint64_t res = (uint64_t) strlen(str);
  res = (uint64_t) ceil(res*(nbit / 8.0));
  return res;
}

size_t
nbytes (size_t nbits)
{
  return (size_t) ceil(((double) nbits) / CHAR_BIT);
}

int
save_content (gzFile f, content* reg)
{
  reg = tail(reg);
  uint64_t size;
  op_wrapper* op;

  while (reg != NULL)
    {
      if ((reg->value->type != Expression) &&
          (reg->value->type != File) &&
          ((reg->value->type != Operation) || ((op_wrapper*) reg->value->data)->instr != NULL))
        {

          uint32_t type = htobe32(reg->value->type);
          gzfwrite(&type, sizeof(uint32_t), 1, f);
      
          switch (reg->value->type)
            {
            case Integer:
              {
                mpz_t* z = (mpz_t*) reg->value->data;
                size = mpz_sizeinbase(*z, 10)+2;
                char* s = malloc(size);
                mpz_get_str(s, 10, *z);
                size = (uint64_t) strlen8(s, CHAR_BIT);
                size = htobe64(size);
                gzfwrite(&size, sizeof(uint64_t), 1, f);
                gzfwrite(s, sizeof(char), strlen(s), f);
                free(s);
              }
              break;
            case Real:
              {
                uint64_t* inp = (double*) reg->value->data;
                uint64_t u = htobe64(*inp);
                gzfwrite(&u, sizeof(uint64_t), 1, f);
              }
              break;
            case String:
              size = (uint64_t) strlen8((char*) reg->value->data,
                                        CHAR_BIT);
              size = htobe64(size);
              gzfwrite(&size, sizeof(uint64_t), 1, f);
              gzfwrite(reg->value->data, sizeof(char),
                     strlen((char*) reg->value->data), f);
              break;
            case Register:
              size = (uint64_t) strlen8(((regstr*) reg->value->data)->name,
                                        CHAR_BIT);
              size = htobe64(size);
              gzfwrite(&size, sizeof(uint64_t), 1, f);
              gzfwrite(reg->value->data, sizeof(char),
                     strlen(((regstr*) reg->value->data)->name), f);
              break;
            case Registry:
              save_registry(f, (registry*) reg->value->data);
              size = htobe32((uint32_t) NotAType);
              gzfwrite(&size, sizeof(uint32_t), 1, f);
              break;
            case Instruction:
              size = (uint64_t) strlen8(((instruction*) reg->value->data)->code,
                                        CHAR_BIT);
              size = htobe64(size);
              gzfwrite(&size, sizeof(uint64_t), 1, f);
              gzfwrite(((instruction*) reg->value->data)->code, sizeof(char),
                     strlen(((instruction*) reg->value->data)->code), f);
              break;
            case Operation:
              {
                op = ((op_wrapper*) reg->value->data);
                if (op->instr == NULL)
                  break;
                size = (uint64_t) strlen8(((instruction*) op->instr->data)->code,
                                          CHAR_BIT);
                uint64_t size_out = htobe64(size);

                gzfwrite(&size_out, sizeof(uint64_t), 1, f);
                gzfwrite(((instruction*) op->instr->data)->code, sizeof(char),
                         strlen(((instruction*) op->instr->data)->code), f);
                uint64_t narg = htobe64((uint64_t) op->n_arg);
                gzfwrite(&narg, sizeof(uint64_t), 1, f);

                for (int i=0; i < op->n_arg; i++)
                  {
                    size = (uint64_t) strlen8(((regstr*) op->args[i]->data)->name, CHAR_BIT);
                    size_out = htobe64(size);
                    gzfwrite(&size_out, sizeof(uint64_t), 1, f);
                    gzfwrite(((regstr*) op->args[i]->data)->name,
                             sizeof(char), strlen(((regstr*) op->args[i]->data)->name), f);
                  }
              }
              break;
            case Task:
              {
                task* t = (task*) reg->value->data;
                save_registry(f, t->state);
                size = htobe32((uint32_t) NotAType);
                gzfwrite(&size, sizeof(uint32_t),1,f);
                size = (uint64_t) strlen8(t->code->code, CHAR_BIT);
                uint64_t size_out = htobe64(size);
                gzfwrite(&size_out, sizeof(uint64_t), 1, f);
                gzfwrite(t->code->code, sizeof(char), strlen(t->code->code), f);
              }
              break;
            case Boolean:
              {
                uint8_t* u8 = reg->value->data;
                gzfwrite(u8, sizeof(uint8_t), 1, f);
              }
              break;
            default:
              break;
            }

          size = (uint64_t) strlen8(reg->name, CHAR_BIT);
          size = htobe64(size);
          gzfwrite(&size, sizeof(uint64_t), 1, f);
          gzfwrite(reg->name, sizeof(char), strlen(reg->name), f);

        }
      
      reg = reg->right;

    }

  return 0;
}

int
read_registry (gzFile f, registry* reg)
{
  uint32_t* type_cache = malloc(sizeof(uint32_t));
  void* cache;
  data* d;
  registry* r;
  uint64_t size;
  statement* stmt = NULL;
  parser_state state;
  FILE* f_sub;
  char* code;
  while (gzfread(type_cache, sizeof(uint32_t), 1, f))
    {
      uint32_t _type = be32toh(*type_cache);
      data_type type = (int) _type;

      if (type == NotAType)
        break;
      
      switch (type)
        {
        case Integer:
          {
            cache = malloc(sizeof(uint64_t));
            gzfread(cache, sizeof(uint64_t), 1, f);
            size = *((uint64_t*) cache);
            size = be64toh(size);
            free(cache);
            cache = malloc(sizeof(uint8_t)*(size+1));
            gzfread(cache, sizeof(uint8_t), size, f);
            *((char*) (cache+size)) = '\0';
            mpz_t z;
            mpz_init_set_str(z, cache, 10);
            assign_int(&d, z);
            mpz_clear(z);
            free(cache);
          }
          break;
        case Real:
          {

            cache = malloc(sizeof(uint64_t));
            gzfread(cache, sizeof(uint64_t), 1, f);
            uint64_t tmp = be64toh(*((uint64_t*) cache));
            double* to_assign = &tmp;

            assign_real(&d, *to_assign);
            free(cache);
          }
          break;
        case String:
          cache = malloc(sizeof(uint64_t));
          gzfread(cache, sizeof(uint64_t), 1, f);
          size = be64toh(*((uint64_t*) cache));
          free(cache);
          cache = malloc(BYT(size)+1);
          gzfread(cache, 1, BYT(size), f);
          *((char*) (cache+BYT(size))) = '\0';
          assign_str(&d, (char*) cache, 0);
          break;
        case Register:
          cache = malloc(sizeof(uint64_t));
          gzfread(cache, sizeof(uint64_t), 1, f);
          size = be64toh(*((uint64_t*) cache));
          free(cache);
          cache = malloc(BYT(size)+1);
          gzfread(cache, 1, BYT(size), f);
          *((char*) (cache+BYT(size))) = '\0';
          assign_regstr(&d, (char*) cache, hash_str((char*) cache));
          free(cache);
          break;
        case Registry:
          r = new_registry(reg, WOB_HASH_SIZE, reg->task);
          read_registry(f, r);
          assign_registry(&d, r, false, reg->task);
          break;
        case Instruction:
          cache = malloc(sizeof(uint64_t));
          gzfread(cache, sizeof(uint64_t), 1, f);
          size = be64toh(*((uint64_t*) cache));
          free(cache);
          
          cache = malloc(BYT(size)+1);
          gzfread(cache, 1, BYT(size), f);
          ((char*) cache)[BYT(size)] = '\0';
          code = malloc(BYT(size)+1);
          strcpy(code, (char*) cache);
          
          f_sub = fmemopen(cache, BYT(size), "r");
          state = fresh_state(0);
          stmt = NULL;
          parse(f_sub, &state, &stmt, reg->task->task);
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
          cache = malloc(sizeof(uint64_t));
          gzfread(cache, sizeof(uint64_t), 1, f);
          size = be64toh(*((uint64_t*) cache));
          free(cache);

          cache = malloc(BYT(size)+1);
          gzfread(cache, 1, BYT(size), f);
          ((char*) cache)[BYT(size)] = '\0';
          code = malloc(BYT(size)+1);
          strcpy(code, (char*) cache);

          f_sub = fmemopen(cache, BYT(size), "r");
          state = fresh_state(0);
          stmt = NULL;
          parse(f_sub, &state, &stmt, reg->task->task);
          fclose(f_sub);

          op_wrapper* op = malloc(sizeof(op_wrapper));
          op->instr = new_data();
          op->instr->type = Instruction;
          op->instr->data = malloc(sizeof(instruction));
          ((instruction*) op->instr->data)->stmt = stmt;
          ((instruction*) op->instr->data)->code = code;
          stmt = NULL;
          free(cache);

          cache = malloc(sizeof(uint64_t));
          gzfread(cache, sizeof(uint64_t), 1, f);
          op->n_arg = be64toh(*((uint64_t*) cache));
          free(cache);

          op->args = malloc(sizeof(data*)*(op->n_arg));

          for (int i = 0; i < op->n_arg; i++)
            {
              cache = malloc(sizeof(uint64_t));
              gzfread(cache, sizeof(uint64_t), 1, f);
              size = be64toh(*((uint64_t*) cache));
              free(cache);

              char* name = malloc(BYT(size)+1);
              gzfread(name, 1, BYT(size), f);
              name[BYT(size)] = '\0';

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
            t->state = new_registry(t->task->current_parse_registry, WOB_HASH_SIZE, t);
            read_registry(f, t->state);
            cache = malloc(sizeof(uint64_t));
            gzfread(cache, sizeof(uint64_t), 1, f);
            size = be64toh(*((uint64_t*) cache));
            free(cache);

            cache = malloc(BYT(size)+1);
            gzfread(cache, 1, BYT(size), f);
            ((char*) cache)[BYT(size)] = '\0';
            code = malloc(BYT(size)+1);
            strcpy(code, (char*) cache);

            f_sub = fmemopen(cache, BYT(size), "r");
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
            t->queued_instruction = new_registry(NULL, WOB_HASH_SIZE, t);
            t->pid = -1;
            t->thread = NULL;
            d = new_data();
            d->type = Task;
            d->data = t;
          }
          break;
        case Boolean:
          {
            cache = malloc(sizeof(uint8_t));
            gzfread(cache, sizeof(uint8_t), 1, f);
            assign_boolean(&d, (bool) *((uint8_t*) cache));
            free(cache);
          }
          break;
        case Nothing:
          assign_nothing(&d);
          break;
        default:
          break;
        }
      cache = malloc(sizeof(uint64_t));
      gzfread(cache, sizeof(uint64_t), 1, f);
      size = be64toh(*((uint64_t*) cache));
      free(cache);
      cache = malloc(BYT(size)+1);
      gzfread(cache, 1, BYT(size), f);
      *((char*) (cache+BYT(size))) = '\0';

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
  gzFile f = gzopen(fname, "w6");
  double save_version = 1.1;
  uint64_t* inp = &save_version;
  uint64_t u = htobe64(*inp);
  gzfwrite(&u, sizeof(uint64_t), 1, f);
  
  save_registry(f, reg);
  data_type end = NotAType;
  uint32_t end_u = htobe32((uint32_t) end);
  gzfwrite(&end_u, sizeof(uint32_t), 1, f);
  gzclose(f);
}

void
read_outer (gzFile f, registry* reg)
{
  double version;
  uint64_t tmp;
  gzfread(&tmp, sizeof(uint64_t), 1, f);
  tmp = be64toh(tmp);
  version = *((double*) &tmp);
  
  read_registry(f, reg);
}
