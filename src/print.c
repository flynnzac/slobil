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
 * @file print.c
 * @brief Functions for printing data to screen.
 */

/**
 * Print data object out to standard output.
 *
 * @param d the data to print.
 * @param settings the print_settings for output
 */
void
print_data (data* d, print_settings settings)
{
  if (settings & PRINT_ANSWER)
    {
      printf("ans = ");
    }
  switch (d->type)
    {
    case Integer:
      gmp_printf("%Zd", *((mpz_t*) d->data));
      break;
    case String:
      if (settings & PRINT_QUOTES)
        ulc_fprintf(stdout, "\"%llU\"", (uint32_t*) d->data);
      else
        ulc_fprintf(stdout, "%llU", (uint32_t*) d->data);
      break;
    case Real:
      printf("%f", *((double*) d->data));
      break;
    case Object:
      print_object((object*) d->data, true);
      break;
    case Instruction:
      print_instruction(((instruction*) d->data));
      break;
    case Slot:
      printf("/%s", ((slot*) d->data)->name);
      break;
    case Operation:
      if (((op_wrapper*) d->data)->instr == NULL)
        printf("Built-in operation.");
      else
        {
          printf("Arguments: ");
          for (int i = 0; i < ((op_wrapper*) d->data)->n_arg; i++)
            {
              printf("/%s ",
                     ((slot*) ((op_wrapper*) d->data)->args[i]->data)->name);
            }
          printf("\n( %s )", ((instruction*) ((op_wrapper*) d->data)->instr->data)->code);
        }
      break;
    case File:
      printf("A file.");
      break;
    case Boolean:
      if (*((bool*) d->data))
        printf("True.");
      else
        printf("False.");
      break;
    case Nothing:
      printf("Nothing.");
      break;
    case Task:
      printf("A Task with Body:\n");
      printf("( %s )", ((task*) d->data)->code->code);
      printf("\n");
      pthread_mutex_lock(&((task*) d->data)->lock);
      if (((task*) d->data)->thread==NULL)
        {
          printf("Task Running.");
        }
      else
        {
          printf("Task Not Running.");
        }
      pthread_mutex_unlock(&((task*) d->data)->lock);
      break;
    default:
      break;
    }

  if ((settings & PRINT_NEWLINE) && (d->type != Object))
    printf("\n");
}

void
print_object (object* obj, bool initial)
{
  if (obj == NULL)
    return;

  if (obj->inherit != NULL)
    {
      printf("->Inherits:\n");
      print_object(obj->inherit, false);
    }

  if (initial)
    {
      if (obj->inherit != NULL)
        {
          printf("\n");
        }
      printf("an Object with:\n");
    }
  for (int i = 0; i < obj->hash_size; i++)
    {
      content* cur = obj->objects[i];
      if (cur == NULL || is_init_content(cur))
        continue;
      cur = tail(cur);
      const char* s = "";
      while (cur != NULL)
        {
          s = str_type(cur->value->type);
          printf("%s of type %s", (const char*) cur->name, s);
          printf(", value: ");
          print_data(cur->value,PRINT_NEWLINE | PRINT_QUOTES);
          cur = cur->right;
        }
    }
}

void
print_instruction (instruction* inst)
{
  statement* stmt = inst->stmt;
  while (stmt != NULL)
    {
      print_statement(stmt);
      printf("\n");
      stmt = stmt->right;
    }
}

void
print_statement (statement* s)
{
  element* e = s->head;
  while (e != NULL)
    {
      if (e->literal)
        {
          print_data(e->data, PRINT_QUOTES);
        }
      else if (e->statement)
        {
          printf(" [ ");
          print_statement(e->s);
          printf(" ] ");
        }
      else
        {
          for (int i = 0; i < e->levels; i++)
            {
              if (i < (e->levels-1))
                printf("%s:/", e->name[i]);
              else
                printf("%s", e->name[i]);
            }
        }
      if (e->right != NULL)
        printf(" ");
      
      e = e->right;

    }
  printf(" .");
}

