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
is_integer (const char* str)
{
  int i;
  for (i=0; i < strlen(str); i++)
    {
      if (!(isdigit(str[i]) || ((i==0) && str[i]=='-')))
        return 0;
    }
  return 1;
}

int
is_decimal (const char* str)
{
  int i;
  int decimals = 0;
  for (i=0; i < strlen(str); i++)
    {
      if (!(isdigit(str[i]) || ((i==0) && str[i]=='-') ||
            str[i] == '.'))
        return 0;

      if (str[i] == '.' && decimals == 1)
        return 0;

      if (str[i] == '.')
        decimals++;
    }
  return decimals==1;
}


int
is_numeric (data* d)
{
  if (d->type == DECIMAL ||
      d->type == INTEGER)
    {
      return 1;
    }
  else
    {
      return 0;
    }
}

int
is_register (const char* str)
{
  return (str[0] == '$');
}

int
is_reference (const char* str)
{
  return (str[0] == '\\');
}

int
is_whitespace (const char c)
{
  if ((c == ' ') || (c == '\t') || (c == '\n'))
    return 1;
  else
    return 0;
}


void
free_data (data* d)
{
  if (d->type == REGISTRY)
    {
      free_registry((registry*) d->data);
      free(d);
    }
  else if (d->type == REFERENCE)
    {
      free(((ref*) d->data)->key);
      free(d->data);
      free(d);
    }
  else if (d->type == OPERATION)
    {
      free(d);
    }
  else if (d->type == ARBEL_FILE)
    {
      free(d);
      if (d->data != NULL)
	fclose((FILE*) d->data);
      
    }
  else
    {
      if (d->data != NULL)
        free(d->data);
  
      free(d);
    }
}

void
free_registry (registry* reg)
{

  if (reg==NULL)
    return;

  if (is_init_reg(reg))
    {
      free(reg);
      return;
    }
  registry* cur = tail(reg);
  registry* tmp;

  if (cur != NULL)
    free(cur->left);

  while (cur != NULL)
    {
      free_data(cur->value);

      free(cur->key);
      tmp = cur->right;
      free(cur);
      cur = tmp;
    }

}

registry*
head (registry* reg)
{
  if (reg == NULL)
    return NULL;

  while (reg->right != NULL)
    {
      reg = reg->right;
    }

  return reg;
}

registry*
tail (registry* reg)
{
  if (reg == NULL)
    return NULL;
  
  while (reg->left != NULL)
    {
      reg = reg->left;
    }

  return reg->right;
}

int
is_error (int e)
{
  static int error = 0;

  if (e >= 0)
    error = e;
  else
    return error;
}


void
do_error (const char* msg)
{
  fprintf(stderr, "Error: %s\n", msg);
  (void) is_error(1);
}

void
str_shift_left (char* buffer)
{
  int i;
  for (i=0; i <= strlen(buffer); i++)
    {
      buffer[i] = buffer[i+1];
    }
}

void
print_data (data* d, int print_cmd)
{
  if (!print_cmd)
    {
      printf("ans = ");
    }
  switch (d->type)
    {
    case INTEGER:
      printf("%d\n", *((int*) d->data));
      break;
    case STRING:
      printf("%s\n", (const char*) d->data);
      break;
    case DECIMAL:
      printf("%f\n", *((double*) d->data));
      break;
    case REGISTRY:
      printf("a registry with:\n");
      print_registry((registry*) d->data);
      break;
    case INSTRUCTION:
      printf("( %s )\n", (const char*) d->data);
      break;
    case REGISTER:
      printf("$%s\n", (const char*) d->data);
      break;
    case OPERATION:
      printf("Built-in instruction.\n");
      break;
    case REFERENCE:
      printf("Reference to: $%s\n", ((ref*) d->data)->key);
      break;
    case ARBEL_FILE:
      printf("A file.\n");
      break;
    default:
      break;
    }
}

const char*
str_type (data_type type)
{
  const char* s;

  switch (type)
    {
    case INTEGER:
      s = "Integer";
      break;
    case DECIMAL:
      s = "Decimal";
      break;
    case STRING:
      s = "String";
      break;
    case REGISTER:
      s = "Register";
      break;
    case REGISTRY:
      s = "Registry";
      break;
    case INSTRUCTION:
      s = "Instruction";
      break;
    case ACTIVE_INSTRUCTION:
      s = "Active-Instruction";
      break;
    case OPERATION:
      s = "Instruction";
      break;
    case REFERENCE:
      s = "Reference";
      break;
    case NOTHING:
      s = "Nothing";
      break;
    case ARBEL_FILE:
      s = "File";
    }

  return s;
}
        
    

void
print_registry (registry* reg)
{
  registry* cur = tail(reg);
  const char* s;
  while (cur != NULL)
    {
      s = str_type(cur->value->type);
      printf("%s of type %s", (const char*) cur->key, s);
      printf(", value: ");
      print_data(cur->value,1);
      cur = cur->right;
    }
}

char*
append_nl (char* str)
{
  size_t len = strlen(str);
  str = realloc(str, sizeof(char)*(len+2));
  str[len] = '\n';
  str[len+1] = '\0';

  return str;
  
}


int
is_exit (int e)
{
  static int do_exit = 0;

  if (e >= 0)
    {
      do_exit = e;
    }
  else
    {
      return do_exit;
    }
}
    
registry*
new_registry (registry* parent)
{
  registry* r = malloc(sizeof(registry));
  r->right = NULL;
  r->left = NULL;
  r->up = parent;
  r->value = NULL;
  r->key = NULL;

  return r;
}

int
is_init_reg (registry* r)
{
  return (r->right == NULL) && (r->left==NULL) && (r->value == NULL) &&
    (r->key == NULL);
}

struct parser_state
fresh_state (int print) 
{
  struct parser_state state;
  state.buffer[0] = '\0';
  state.arg_n = 0;
  state.i = 0;
  state.in_instr = 0;
  state.after_instr = 0;
  state.in_quote = 0;
  state.after_quote = 0;
  state.open_paren = '\0';
  state.print_out = print;
  state.in_comment = 0;

  return state;
}

int
is_retval (const int r)
{
  static int retval = 0;

  if (r >= 0)
    retval = r;

  return retval;
}

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
      n_digits = floor(log10(n)) + 2;
    }
  else
    {
      n_digits = floor(log10(n)) + 1;
    }
  char* name = malloc(sizeof(char)*(n_digits+2));
  sprintf(name, "#%d", n);
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

