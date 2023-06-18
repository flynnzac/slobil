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
#define BYT8 nbytes(8)
#define BYT(s) nbytes(8*s)

int
save_object (gzFile f, object* reg)
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

uint8_t*
read_string(gzFile f)
{
  uint64_t size;
  uint8_t* string;
  gzfread(&size, sizeof(uint64_t), 1, f);

  size = le64toh(size);

  string = malloc(BYT(size)+1);
  gzfread(string, 1, BYT(size), f);
  string[BYT(size)] = '\0';

  return string;
}

uint32_t*
read_string_u32(gzFile f)
{
  uint64_t size;
  uint32_t* string;
  gzfread(&size, sizeof(uint64_t), 1, f);

  size = le64toh(size);

  string = malloc(sizeof(uint32_t)*(size+1));
  gzfread(string, sizeof(uint32_t), size, f);
  string[size] = (uint32_t) 0;
  uint32_t* u32 = u32_str_to_h((uint32_t*) string);
  free(string);

  return u32;
}


statement*
read_in_instruction(uint8_t* code, task_vars* tv)
{
  FILE* f_sub = fmemopen(code, BYT(strlen(code)), "r");
  parser_state state = fresh_state(0);
  statement* stmt = NULL;

  int complete = parse(f_sub, &state, &stmt, tv);
  fclose(f_sub);
  if (!complete)
    {
      char* ending = malloc(sizeof(char)*(strlen(" . ")+1));
      strcpy(ending, " . ");
      f_sub = fmemopen(ending,
                       sizeof(char)*strlen(ending),
                       "r");
      complete = parse(f_sub, &state, &stmt, tv);
      fclose(f_sub);
      free(ending);
    }

  return stmt;
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

          uint32_t type = htole32(reg->value->type);
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
                size = htole64(size);
                gzfwrite(&size, sizeof(uint64_t), 1, f);
                gzfwrite(s, sizeof(char), strlen(s), f);
                free(s);
              }
              break;
            case Real:
              {
                uint64_t* inp = (double*) reg->value->data;
                uint64_t u = htole64(*inp);
                gzfwrite(&u, sizeof(uint64_t), 1, f);
              }
              break;
            case String:
              {
                size = (uint64_t) u32_strlen((uint32_t*) reg->value->data);
                size = htole64(size);
                gzfwrite(&size, sizeof(uint64_t), 1, f);
                uint32_t* u32 = u32_str_to_le
                  ((uint32_t*) reg->value->data);
                
                gzfwrite(reg->value->data, sizeof(uint32_t),
                         u32_strlen(u32), f);

                free(u32);
              }
              break;
            case Slot:
              size = (uint64_t) strlen8(((slot*) reg->value->data)->name,
                                        CHAR_BIT);
              size = htole64(size);
              gzfwrite(&size, sizeof(uint64_t), 1, f);
              gzfwrite(((slot*) reg->value->data)->name, sizeof(char),
                       strlen(((slot*) reg->value->data)->name), f);
              break;
            case Object:
              save_object(f, (object*) reg->value->data);
              size = htole32((uint32_t) NotAType);
              gzfwrite(&size, sizeof(uint32_t), 1, f);
              break;
            case Instruction:
              size = (uint64_t) strlen8(((instruction*) reg->value->data)->code,
                                        CHAR_BIT);
              size = htole64(size);
              gzfwrite(&size, sizeof(uint64_t), 1, f);
              gzfwrite(((instruction*) reg->value->data)->code, sizeof(char),
                       strlen(((instruction*) reg->value->data)->code), f);
              size = (uint64_t) strlen8(((instruction*) reg->value->data)->help,
                                        CHAR_BIT);
              size = htole64(size);
              gzfwrite(&size, sizeof(uint64_t), 1, f);
              gzfwrite(((instruction*) reg->value->data)->help, sizeof(char),
                       strlen(((instruction*) reg->value->data)->help), f);
              break;
            case Operation:
              {
                op = ((op_wrapper*) reg->value->data);
                if (op->instr == NULL)
                  break;
                size = (uint64_t) strlen8(((instruction*) op->instr->data)->code,
                                          CHAR_BIT);
                uint64_t size_out = htole64(size);

                gzfwrite(&size_out, sizeof(uint64_t), 1, f);
                gzfwrite(((instruction*) op->instr->data)->code, sizeof(char),
                         strlen(((instruction*) op->instr->data)->code), f);
                uint64_t narg = htole64((uint64_t) op->n_arg);
                gzfwrite(&narg, sizeof(uint64_t), 1, f);

                for (int i=0; i < op->n_arg; i++)
                  {
                    size = (uint64_t) strlen8(((slot*) op->args[i]->data)->name, CHAR_BIT);
                    size_out = htole64(size);
                    gzfwrite(&size_out, sizeof(uint64_t), 1, f);
                    gzfwrite(((slot*) op->args[i]->data)->name,
                             sizeof(char), strlen(((slot*) op->args[i]->data)->name), f);
                  }
              }
              break;
            case Task:
              {
                task* t = (task*) reg->value->data;
                save_object(f, t->state);
                size = htole32((uint32_t) NotAType);
                gzfwrite(&size, sizeof(uint32_t),1,f);
                size = (uint64_t) strlen8(t->code->code, CHAR_BIT);
                uint64_t size_out = htole64(size);
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
          size = htole64(size);
          gzfwrite(&size, sizeof(uint64_t), 1, f);
          gzfwrite(reg->name, sizeof(char), strlen(reg->name), f);

        }
      
      reg = reg->right;

    }

  return 0;
}

int
read_object (gzFile f, object* reg)
{
  uint32_t* type_cache = malloc(sizeof(uint32_t));
  void* cache;
  data* d;
  object* r;
  uint64_t size;
  statement* stmt = NULL;
  parser_state state;
  FILE* f_sub;
  char* code;
  char* help;
  int complete;
  while (gzfread(type_cache, sizeof(uint32_t), 1, f))
    {
      uint32_t _type = le32toh(*type_cache);
      data_type type = (int) _type;

      if (type == NotAType)
        break;
      
      switch (type)
        {
        case Integer:
          {
            uint8_t* number = read_string(f);
            mpz_t z;
            mpz_init_set_str(z, number, 10);
            assign_int(&d, z);
            mpz_clear(z);
          }
          break;
        case Real:
          {
            uint64_t input;
            gzfread(&input, sizeof(uint64_t), 1, f);
            uint64_t tmp = le64toh(input);
            double* to_assign = &tmp;
            assign_real(&d, *to_assign);
          }
          break;
        case String:
          {
            uint32_t* u32_string = read_string_u32(f);
            assign_str(&d, u32_string, 0);
          }
          break;
        case Slot:
          {
            char* slot_name = read_string(f);
            assign_slot(&d, slot_name, hash_str(slot_name));
          }
          break;
        case Object:
          r = new_object(reg, SLOBIL_HASH_SIZE, reg->task);
          read_object(f, r);
          assign_object(&d, r, false, reg->task);
          break;
        case Instruction:
          code = read_string(f);
          stmt = read_in_instruction(code, reg->task->task);
          help = read_string(f);

          d = new_data();
          d->type = Instruction;
          d->data = malloc(sizeof(instruction));
          ((instruction*) d->data)->stmt = stmt;
          ((instruction*) d->data)->code = code;
          ((instruction*) d->data)->help = help;
          stmt = NULL;
          break;
        case Operation:
          code = read_string(f);
          stmt = read_in_instruction(code, reg->task->task);

          op_wrapper* op = malloc(sizeof(op_wrapper));
          op->instr = new_data();
          op->instr->type = Instruction;
          op->instr->data = malloc(sizeof(instruction));
          ((instruction*) op->instr->data)->stmt = stmt;
          ((instruction*) op->instr->data)->code = code;
          stmt = NULL;

          cache = malloc(sizeof(uint64_t));
          gzfread(cache, sizeof(uint64_t), 1, f);
          op->n_arg = le64toh(*((uint64_t*) cache));
          free(cache);

          op->args = malloc(sizeof(data*)*(op->n_arg));

          for (int i = 0; i < op->n_arg; i++)
            {
              char* name = read_string(f);
              unsigned long hash = hash_str(name);

              assign_slot(&op->args[i], name, hash);
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
            t->state = new_object(t->task->current_parse_object, SLOBIL_HASH_SIZE, t);
            read_object(f, t->state);

            code = read_string(f);
            stmt = read_in_instruction(f, t->task);
            
            t->code = malloc(sizeof(instruction));
            t->code->stmt = stmt;
            t->code->code = code;

            stmt = NULL;

            t->task->current_parse_object = t->state;
            t->queued_instruction = new_object(NULL, SLOBIL_HASH_SIZE, t);
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
      char* loc = read_string(f);
      if (d != NULL)
        set(reg, d, loc, 1);

    }

  free(type_cache);
      
  return 0;
  
}

void
save_outer (object* reg, char* fname)
{
  gzFile f = gzopen(fname, "w6");
  double save_version = 1.1;
  uint64_t* inp = &save_version;
  uint64_t u = htole64(*inp);
  gzfwrite(&u, sizeof(uint64_t), 1, f);
  
  save_object(f, reg);
  data_type end = NotAType;
  uint32_t end_u = htole32((uint32_t) end);
  gzfwrite(&end_u, sizeof(uint32_t), 1, f);
  gzclose(f);
}

void
read_outer (gzFile f, object* reg)
{
  double version;
  uint64_t tmp;
  gzfread(&tmp, sizeof(uint64_t), 1, f);
  tmp = le64toh(tmp);
  version = *((double*) &tmp);
  
  read_object(f, reg);
}
