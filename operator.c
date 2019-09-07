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

void
op_list (registry* reg)
{
  registry* r_new = new_registry(reg->up);
  data* d = NULL;
  data* d_new;
  int i = 1;
  char* r = NULL;
  unsigned long hash_r = 0;

  r = argument_name(i);
  hash_r = hash_str(r);
  while ((d = lookup(reg, hash_r, 0)) != NULL)
    {
      d_new = copy_data(d);
      set(r_new, d_new, r);
      i++;
      free(r);
      r = argument_name(i);
      hash_r = hash_str(r);
    }

  if (r != NULL)
    free(r);

  d = malloc(sizeof(data));
  d->type = REGISTRY;
  d->data = r_new;
  ret_ans(reg, d);

}

void
op_reg (registry* reg)
{
  registry* r_new = new_registry(reg->up);
  data* d = NULL;
  data* d_data = NULL;
  data* d_new;
  int i = 1;
  char* r = NULL;
  unsigned long hash_r = 0;

  r = argument_name(i);
  hash_r = hash_str(r);

  while ((d = lookup(reg, hash_r, 0)) != NULL)
    {
      if (d->type != REGISTER)
        {
          do_error("Expected a register");
          free_registry(r_new);
          free(r);
          return;
        }
      free(r);
      r = argument_name(i+1);
      hash_r = hash_str(r);
      d_data = lookup(reg,hash_r,0);
      d_new = copy_data(d_data);
      set(r_new, d_new, ((regstr*) d->data)->name);
      i = i + 2;
      free(r);
      r = argument_name(i);
      hash_r = hash_str(r);
    }

  if (r != NULL)
    free(r);

  d = malloc(sizeof(data));
  d->type = REGISTRY;
  d->data = r_new;
  ret_ans(reg, d);
}

void
op_arithmetic (registry* reg, const int code)
{
  data* arg1 = lookup(reg, arbel_hash_1, 0);
  data* arg2 = lookup(reg, arbel_hash_2, 0);
  data* new_data;

  if (arg1 == NULL || arg2 == NULL)
    {
      do_error ("Requires two arguments.");
      return;
    }

  if (!is_numeric(arg1) || !is_numeric(arg2))
    {
      do_error("Not numeric arguments.");
      return;
    }

  if (arg1->type == DECIMAL || arg2->type == DECIMAL)
    {
      double term2;
      double result;
      if (arg1->type == INTEGER)
        result = *((int*) arg1->data);
      else
        result = *((double*) arg1->data);

      if (arg2->type == INTEGER)
        term2 = *((int*) arg2->data);
      else
        term2 = *((double*) arg2->data);

      switch (code)
        {
        case 1:
          result += term2;
          break;
        case 2:
          result *= term2;
          break;
        case 3:
          result -= term2;
          break;
        case 4:
          result /= term2;
          break;
        }
      assign_dec(&new_data, result);
    }
  else
    {
      
      int result_int = *((int*) arg1->data);
      int term2_int = *((int*) arg2->data);
      switch (code)
        {
        case 1:
          result_int += term2_int;
          break;
        case 2:
          result_int *= term2_int;
          break;
        case 3:
          result_int -= term2_int;
          break;
        case 4:
          result_int /= term2_int;
          break;
        }
      assign_int(&new_data, result_int);
    }
  
  ret_ans(reg, new_data);
}  

void
op_add (registry* reg)
{
  op_arithmetic(reg, 1);
}

void
op_multiply (registry* reg)
{
  op_arithmetic(reg, 2);
}

void
op_subtract (registry* reg)
{
  op_arithmetic(reg, 3);
}

void
op_divide (registry* reg)
{
  op_arithmetic(reg, 4);
}

void
op_set (registry* reg)
{
  data* arg1 = lookup(reg, arbel_hash_1,0);
  data* arg2 = lookup(reg, arbel_hash_2,0);
  data* arg3 = lookup(reg, arbel_hash_3,0);

  if (arg1 == NULL || arg2 == NULL)
    {
      do_error("`set` needs two arguments.");
      return;
    }

  if (arg1->type != REGISTER)
    {
      do_error("First argument to `set` must be a register.");
      return;
    }

  registry* to_set;
  if (arg3 != NULL)
    {
      if (arg3->type != REGISTRY)
        {
          do_error("Third argument to `set` must be a registry.");
          return;
        }
      
      to_set = (registry*) arg3->data;
    }
  else
    {
      to_set = reg->up;
    }
  

  char* name = ((regstr*) arg1->data)->name;
  data* d = copy_data(arg2);
  set(to_set, d, name);

  is_retval(0);

}

void
op_get (registry* reg)
{
  data* arg1 = lookup(reg,arbel_hash_1,0);
  data* arg2 = lookup(reg,arbel_hash_2,0);

  if (arg1 == NULL)
    do_error("Get needs an argument.");

  if (is_error(-1))
    return;

  if (arg1->type != REGISTER)
    do_error("First argument must be a register.");

  registry* to_look;
  if (arg2 == NULL)
    {
      to_look = reg->up;
    }
  else
    {
      if (arg2->type != REGISTRY)
        {
          do_error("Second argument must be a registry.");
          return;
        }

      to_look = (registry*) arg2->data;
    }

  data* ans = lookup(to_look, ((regstr*) arg1->data)->key, 0);
  if (ans != NULL)
    {
      ans = copy_data(ans);
      ret_ans(reg, ans);
    }
}

void
op_if (registry* reg)
{
  data* arg1 = lookup(reg, arbel_hash_1,0);

  if (arg1 == NULL)
    do_error("If needs an argument.");

  if (arg1->type != INTEGER)
    do_error("First argument to if must be an integer.");

  if (is_error(-1))
    return;

  if (*((int*) arg1->data))
    {
      data* arg2 = lookup(reg, arbel_hash_2,0);
      if (arg2 == NULL)
        {
          return;
        }
      data* d = copy_data(arg2);
      ret_ans(reg, d);
    }
  else
    {
      data* arg3 = lookup(reg,arbel_hash_3,0);
      data* d;
      if (arg3 != NULL)
        {
          d = copy_data(arg3);
          ret_ans(reg, d);
        }
    }
}

void
op_sit (registry* reg)
{
  return;
}


void
op_mov (registry* reg)
{
  data* arg1 = lookup(reg, arbel_hash_1,0);
  data* arg2 = lookup(reg, arbel_hash_2,0);

  if (arg1 == NULL || arg2 == NULL)
    do_error("mov requires two arguments.");

  if (arg1->type != REGISTER)
    do_error("First argument should be register.");

  if (arg2->type != REGISTER)
    do_error("Second argument should be register.");

  if (!is_error(-1) && reg->up != NULL)
    mov(reg->up,
        (regstr*) arg1->data,
        (regstr*) arg2->data);
  
}


void
op_del (registry* reg)
{
  data* arg1 = lookup(reg, arbel_hash_1,0);

  if (arg1 == NULL)
    {
      do_error("del requires an argument.");
      return;
    }

  if (arg1->type != REGISTER)
    do_error("Argument to del should be a register.");

  if (!is_error(-1) && reg->up != NULL)
    {
      del(reg->up, ((regstr*) arg1->data)->key, 1);
    }

}

void
op_exit (registry* reg)
{
  is_exit(1);
  is_error(1);
}

void
op_exist (registry* reg)
{
  data* arg1 = lookup(reg, arbel_hash_1, 0);

  if (arg1 == NULL)
    do_error("exist requires an argument.");

  if (is_error(-1))
    return;

  if (arg1->type != REGISTER)
    do_error("Argument should be a register.");

  if (is_error(-1))
    return;

  data* obj = get(reg->up, ((regstr*) arg1->data)->key, 0);
  data* d;
  if (obj == NULL)
    {
      assign_int(&d, 0);
    }
  else
    {
      assign_int(&d, 1);
    }

  ret_ans(reg,d);
}


      

void
op_answer (registry* reg)
{
  data* d = lookup(reg, arbel_hash_1, 0);
  if (d == NULL)
    do_error("`answer` requires an argument.");

  if (!is_error(-1))
    {
      d = copy_data(d);
      ret_ans(reg, d);
    }
}

int
op_comparison (registry* reg)
{

  data* arg1 = lookup(reg, arbel_hash_1, 0);
  data* arg2 = lookup(reg, arbel_hash_2, 0);

  if (arg1 == NULL || arg2 == NULL)
    {
      do_error("Comparisons require two arguments.");
      return -2;
    }

  if (!is_numeric(arg1) || !is_numeric(arg2))
    {
      do_error("Both arguments to comparison much be numeric.");
      return -2;
    }


  if (arg1->type == INTEGER && arg2->type == INTEGER)
    {
      if (*((int*) arg1->data) > *((int*) arg2->data))
        {
          return 1;
        }
      else if (*((int*) arg1->data) < *((int*) arg2->data))
        {
          return -1;
        }
      else
        {
          return 0;
        }
    }
  else
    {
      double dval1;
      double dval2;
      if (arg1->type == INTEGER)
        {
          dval1 = *((int*) arg1->data);
          dval2 = *((double*) arg2->data);
        }
      else if (arg2->type == INTEGER)
        {
          dval1 = *((double*) arg1->data);
          dval2 = *((int*) arg2->data);
        }
      else
        {
          dval1 = *((double*) arg1->data);
          dval2 = *((double*) arg2->data);
        }

      if (dval1 > dval2)
        {
          return 1;
        }
      else if (dval1 < dval2)
        {
          return -1;
        }
      else
        {
          return 0;
        }
            
    }
      
}

void
op_gt (registry* reg)
{
  data* d = NULL;
  int is_gt = op_comparison(reg);

  if (is_error(-1))
    return;

  if (is_gt==1)
    {
      assign_int(&d, 1);
      ret_ans(reg,d);
    }
  else
    {
      assign_int(&d, 0);
      ret_ans(reg,d);
    }
}

void
op_lt (registry* reg)
{
  data* d = NULL;
  int is_lt = op_comparison(reg);

  if (is_error(-1))
    return;

  if (is_lt==-1)
    {
      assign_int(&d, 1);
      ret_ans(reg,d);
    }
  else
    {
      assign_int(&d, 0);
      ret_ans(reg,d);
    }
}

void
op_eq (registry* reg)
{
  data* d = NULL;
  int is_eq = op_comparison(reg);

  if (is_error(-1))
    return;

  if (is_eq==0)
    {
      assign_int(&d, 1);
      ret_ans(reg,d);
    }
  else
    {
      assign_int(&d, 0);
      ret_ans(reg,d);
    }
}

void
op_gteq (registry* reg)
{
  data* d = NULL;
  int is_gteq = op_comparison(reg);

  if (is_error(-1))
    return;

  if (is_gteq==1 || is_gteq==0)
    {
      assign_int(&d, 1);
      ret_ans(reg,d);
    }
  else
    {
      assign_int(&d, 0);
      ret_ans(reg,d);
    }
}


void
op_lteq (registry* reg)
{
  data* d = NULL;
  int is_lteq = op_comparison(reg);

  if (is_error(-1))
    return;

  if (is_lteq==-1 || is_lteq==0)
    {
      assign_int(&d, 1);
      ret_ans(reg,d);
    }
  else
    {
      assign_int(&d, 0);
      ret_ans(reg,d);
    }
}



void
op_print (registry* reg)
{
  data* arg1 = lookup(reg, arbel_hash_1,0);
  if (arg1 != NULL)
    {
      print_data(arg1,1);
    }
}

void
op_character (registry* reg)
{
  data* arg1 = lookup(reg, arbel_hash_1, 0);
  data* arg2 = lookup(reg, arbel_hash_2, 0);

  if (arg1 == NULL || arg2 == NULL)
    {
      do_error("`character` requires two arguments.");
      return;
    }

  if (arg1->type != STRING)
    {
      do_error("First argument to `character` must be a string.");
      return;
    }

  if (arg2->type != INTEGER)
    {
      do_error("Second argument to `character` must be an integer.");
    }

  char* str = (char*) arg1->data;
  int loc = *((int*) arg2->data);

  if (loc <= 0)
    {
      int length = strlen(str);
      loc += length;
    }

  if ((loc > strlen(str)) || (loc <= 0))
    {
      do_error("Index out of range.");
      return;
    }

  char* res = malloc(sizeof(char)*2);
  res[0] = str[loc-1];
  res[1] = '\0';

  data* d;
  assign_str (&d, res, 0);

  ret_ans(reg, d);
  
}

void
op_count_characters (registry* reg)
{
  data* arg1 = lookup(reg, arbel_hash_1, 0);

  if (arg1 == NULL)
    {
      do_error("`count-characters` requires an argument.");
      return;
    }

  if (arg1->type != STRING)
    {
      do_error("`count-characters`'s argument must be a string.");
      return;
    }

  int len = strlen((char*) arg1->data);

  data* d;
  assign_int(&d, len);

  ret_ans(reg, d);
  
}

void
op_concat (registry* reg)
{
  data* arg1 = lookup(reg, arbel_hash_1, 0);
  data* arg2 = lookup(reg, arbel_hash_2, 0);

  if (arg1 == NULL || arg2 == NULL)
    {
      do_error("concat requires two arguments.");
      return;
    }

  if (arg1->type != STRING || arg2->type != STRING)
    {
      do_error("Both arguments to concat should be strings.");
      return;
    }

  char* result = malloc(sizeof(char)*(strlen(arg1->data) +
                                      strlen(arg2->data) + 1));
  strcpy(result, (char*) arg1->data);
  strcat(result, (char*) arg2->data);

  data* d = malloc(sizeof(data));
  d->type = STRING;
  d->data = result;

  ret_ans(reg, d);

}

void
op_compute (registry* reg)
{
  data* d;

  d = lookup(reg, arbel_hash_1, 0);

  if (d->type != REGISTRY)
    {
      do_error("First argument to `compute` should be a registry.");
      return;
    }

  registry* arg_reg = (registry*) d->data;
  arg_reg->up = reg->up;

  compute(arg_reg);

  
}

void
op_source (registry* reg)
{
  data* arg1 = lookup(reg, arbel_hash_1, 0);
  if (arg1 == NULL)
    {
      do_error("`source` requires an argument.");
      return;
    }

  if (arg1->type != STRING)
    {
      do_error("Argument to `source` must be a string.");
      return;
    }

  FILE* f = fopen((char*) arg1->data, "r");
  if (f == NULL)
    {
      char* msg = malloc(sizeof(char)*(strlen("File not found.")
                                       + strlen((char*) arg1->data) +
                                       6));
      sprintf(msg, "File `%s` not found.", (char*) arg1->data);
      do_error(msg);
      free(msg);
      return;
    }

  struct parser_state state = fresh_state(0);
  statement* s = NULL;
  parse(f, &state, &s);
  fclose(f);
  execute_code(s, reg->up);
  free_statement(s);

}

void
op_do_to_all (registry* reg)
{
  data* arg1 = lookup(reg, arbel_hash_1,0);
  data* arg2 = lookup(reg, arbel_hash_2,0);

  if (arg1 == NULL || arg2 == NULL)
    {
      do_error("`do-to-all` requires two arguments.");
      return;
    }

  if (arg1->type != INSTRUCTION && arg1->type != OPERATION)
    {
      do_error("First argument to `do-to-all` must be an instruction.");
      return;
    }

  if (arg2->type != REGISTRY)
    {
      do_error("Second argument to `do-to-all` must be a registry.");
      return;
    }

  registry* tmp_reg = new_registry(reg);
  set(tmp_reg, arg1, "#0");

  registry* arg_reg = (registry*) arg2->data;
  arg_reg = tail(arg_reg);

  registry* ret_reg = new_registry(reg->up);

  data* d;
  while (arg_reg != NULL)
    {
      d = arg_reg->value;
      set(tmp_reg, d, "#1");
      compute(tmp_reg);
      d = lookup(reg, arbel_hash_ans, 0);
      if (d == NULL)
        {
          do_error("Instruction in `do-to-all` did not set `ans` register.");
          break;
        }
        
      d = copy_data(d);
      set(ret_reg, d, arg_reg->name);
      del(tmp_reg, arbel_hash_1, 0);
      arg_reg = arg_reg->right;
    }

  del(tmp_reg, arbel_hash_0, 0);
  free_registry(tmp_reg);
  if (!is_error(-1))
    {
      d = malloc(sizeof(data));
      d->type = REGISTRY;
      d->data = ret_reg;

      ret_ans(reg, d);
    }
  
}

void
op_next (registry* reg)
{
  data* arg1 = lookup(reg, arbel_hash_1, 0);

  if (arg1 == NULL)
    {
      do_error("`next` needs an argument.");
      return;
    }

  if (arg1->type != REGISTER)
    {
      do_error("Argument to `next` must be a register.");
      return;
    }

  char* cur_name = malloc(sizeof(char)*
                          (strlen(((regstr*) arg1->data)->name)+1));
  strcpy(cur_name, ((regstr*) arg1->data)->name);

  int i = strlen(cur_name)-1;
  int j;
  while (i >= 0 && isdigit((int) cur_name[i]))
    i--;

  if (i == (strlen(cur_name)-1))
    {
      do_error("Register does not end in integer.");
      return;
    }
  
  i++;

  
  char* cur_num = malloc(sizeof(char)*(strlen(cur_name)-i + 1));
  for (j = i; j < strlen(cur_name); j++)
    {
      cur_num[j-i] = cur_name[j];
    }
  cur_num[strlen(cur_name)-i] = '\0';
  cur_name[i] = '\0';
  int num = atoi(cur_num)+1;
  free(cur_num);

  char* new_name = malloc(sizeof(char)*(strlen(cur_name)+2));
  sprintf(new_name, "%s%d", cur_name, num);

  data* d = malloc(sizeof(data));
  d->type = REGISTER;
  d->data = malloc(sizeof(regstr));
  ((regstr*) d->data)->name = new_name;
  ((regstr*) d->data)->key = hash_str(new_name);
  
  free(cur_name);
  ret_ans(reg,d);

  
}

void
op_last (registry* reg)
{
  data* arg1 = lookup(reg, arbel_hash_1, 0);
  data* arg2 = lookup(reg, arbel_hash_2, 0);
  if (arg1==NULL || arg2==NULL)
    {
      do_error("`last` requires two arguments.");
      return;
    }

  if (arg1->type != REGISTRY)
    {
      do_error("First argument to `last` must be a registry.");
      return;
    }

  if (arg2->type != STRING)
    {
      do_error("Second argument to `last` must be a string.");
      return;
    }

  registry* arg_reg = (registry*) arg1->data;
  char* name = NULL;
  int i = 1;
  data* d;
  name = vector_name((char*) arg2->data, i);
  unsigned long hash_name = hash_str(name);
  while ((d = get(arg_reg, hash_name, 0)) != NULL)
    {
      i++;
      free(name);
      name = vector_name((char*) arg2->data, i);
      hash_name = hash_str(name);
    }
  free(name);
  name = vector_name((char*) arg2->data, i-1);

  d = malloc(sizeof(data));
  d->type = REGISTER;
  d->data = malloc(sizeof(regstr));
  ((regstr*) d->data)->name = name;
  ((regstr*) d->data)->key = hash_str(name);

  ret_ans(reg, d);
  
}

void
op_in (registry* reg)
{
  data* arg1 = lookup(reg, arbel_hash_1, 0);
  data* arg2 = lookup(reg, arbel_hash_2, 0);

  if (arg1==NULL || arg2==NULL)
    {
      do_error("`in` requires two arguments.");
      return;
    }

  if (arg1->type != REGISTRY)
    {
      do_error("First argument to `in` should be a registry.");
      return;
    }

  if (arg2->type != INSTRUCTION)
    {
      do_error("Second argument to `in` should be a instruction.");
      return;
    }


  execute_code(((instruction*) arg2->data)->stmt,
               (registry*) arg1->data);

  data* ans = lookup((registry*) arg1->data, arbel_hash_ans, 0);
  if (ans != NULL)
    {
      ans = copy_data(ans);
      ret_ans(reg, ans);
    }
  
}

void
op_while (registry* reg)
{
  data* arg1 = lookup(reg, arbel_hash_1, 0);
  data* arg2 = lookup(reg, arbel_hash_2, 0);

  if (arg1 == NULL || arg2 == NULL)
    {
      do_error("`while` requires two arguments.");
      return;
    }

  if (arg1->type != INSTRUCTION || arg2->type != INSTRUCTION)
    {
      do_error("Both arguments to `while` should be instructions in ()'s.");
      return;
    }

  data* d;
  while (1)
    {
      execute_code(((instruction*) arg1->data)->stmt, reg->up);
      if (is_error(-1))
        break;
          
      d = get(reg->up, arbel_hash_ans, 0);

      if (d == NULL)
        {
          do_error("Instruction did not set `ans` to a value.");
          break;
        }

      if (d->type != INTEGER)
        {
          do_error("First instruction should set `ans` to an integer.");
          break;
        }

      if ((*(int*) d->data) == 0)
        {
          break;
        }
      
      execute_code(((instruction*) arg2->data)->stmt, reg->up);
      if (is_error(-1))
        break;

    }

}

void
op_repeat (registry* reg)
{
  data* arg1 = lookup(reg, arbel_hash_1,0);
  data* arg2 = lookup(reg, arbel_hash_2,0);

  if (arg1 == NULL || arg2 == NULL)
    {
      do_error("`repeat` requires two arguments.");
      return;
    }

  if (arg1->type != INTEGER)
    {
      do_error("The first argument to `repeat` must be an integer.");
      return;
    }

  if (arg2->type != INSTRUCTION)
    {
      do_error("The second argument to `repeat` must be an instruction.");
      return;
    }

  int i;

  for (i = 0; i < *((int*) arg1->data); i++)
    {
      execute_code(((instruction*) arg2->data)->stmt, reg->up);
      if (is_error(-1)) break;
    }
  is_retval(0);
}

void
op_to_register (registry* reg)
{
  data* arg1 = lookup(reg, arbel_hash_1, 0);
  if (arg1 == NULL)
    {
      do_error("`to-register` requires an argument.");
      return;
    }

  if (arg1->type != STRING)
    {
      do_error("Argument to `to-register` must be a string.");
    }

  data* d;
  assign_regstr(&d, (char*) arg1->data, hash_str((char*) arg1->data));
  ret_ans(reg,d);

}

void
op_collapse (registry* reg)
{
  data* arg1 = lookup(reg, arbel_hash_1, 0);
  data* arg2 = lookup(reg, arbel_hash_2, 0);
  data* arg3 = lookup(reg, arbel_hash_3, 0);
  if (arg1 == NULL || arg2 == NULL || arg3 == NULL)
    {
      do_error("`collapse` requires three arguments.");
      return;
    }

  if (arg1->type != REGISTRY)
    {
      do_error("The first argument to `collapse` must be a registry.");
      return;
    }

  if (arg2->type != INSTRUCTION && arg2->type != OPERATION)
    {
      do_error("The second argument to `collapse` must be an instruction.");
      return;
    }

  if (arg3->type != STRING)
    {
      do_error("The third argument to `collapse` must be a string.");
      return;
    }

  data* d;
  char* second_name;
  char* first_name;
  int i = 2;
  registry* to_walk = (registry*) arg1->data;

  first_name = vector_name((char*) arg3->data, 1);
  unsigned long first_hash = hash_str(first_name);
  second_name = vector_name((char*) arg3->data, i);

  unsigned long second_hash = hash_str(second_name);
  registry* r = new_registry(to_walk);
  arg2 = copy_data(arg2);
  set(r, arg2, "#0");

  data* ref1;
  data* ref2;
  
  data* ans = NULL;
  int is_regstr = 1;
  while ((d = lookup(to_walk, second_hash, 0)) != NULL)
    {
      assign_ref(&ref1, to_walk, &first_name, &first_hash, 1, &is_regstr);
      assign_ref(&ref2, to_walk, &second_name, &second_hash, 1, &is_regstr);
      set(r, ref1, "#1");
      set(r, ref2, "#2");
      compute(r);


      if (strlen(first_name) != strlen("ans"))
        {
          free(first_name);
          first_name = malloc(sizeof(char)*(strlen("ans")+1));
          strcpy(first_name, "ans");
          first_hash = arbel_hash_ans;
        }
      i++;
      free(second_name);
      second_name = vector_name((char*) arg3->data, i);
      second_hash = hash_str(second_name);
    }
  d = lookup(to_walk, arbel_hash_ans, 0);

  if (d != NULL)
    {
      ans = copy_data(d);
      ret_ans(reg, ans);
    }

  free_registry(r);
  free(second_name);
  free(first_name);


}

/* apply an instruction to all matching pairs of two registries return a new registry */
void
op_join (registry* reg)
{
  data* arg1 = lookup(reg, arbel_hash_1, 0);
  data* arg2 = lookup(reg, arbel_hash_2, 0);
  data* arg3 = lookup(reg, arbel_hash_3, 0);

  if (arg1 == NULL || arg2 == NULL || arg3 == NULL)
    {
      do_error("`join` requires three arguments.");
      return;
    }

  if (arg1->type != REGISTRY || arg2->type != REGISTRY)
    {
      do_error("The first two arguments of `join` must be registries.");
      return;
    }

  if (arg3->type != INSTRUCTION && arg3->type != OPERATION)
    {
      do_error("The third argument of `join` must be an instruction.");
      return;
    }

  registry* reg1 = (registry*) arg1->data;
  registry* reg2 = (registry*) arg2->data;
  registry* out_reg = new_registry(reg->up);
  registry* instr_reg = new_registry(reg1);

  registry* cur = tail(reg1);
  data* ref1 = NULL;
  data* ref2 = NULL;
  data* d;

  arg3 = copy_data(arg3);
  set(instr_reg, arg3, "#0");
  int is_regstr = 1;
  
  while (cur != NULL)
    {
      d = get(reg2, cur->key, 0);
      if (d != NULL)
        {
          assign_ref(&ref1, reg1, &cur->name, &cur->key, 1, &is_regstr);
          assign_ref(&ref2, reg2, &cur->name, &cur->key, 1, &is_regstr);
          set(instr_reg, ref1, "#1");
          set(instr_reg, ref2, "#2");
          compute(instr_reg);
          d = get(reg1, arbel_hash_ans, 0);
          if (d != NULL)
            {
              d = copy_data(d);
              set(out_reg, d, cur->name);
            }
        }
      cur = cur->right;
    }

  d = malloc(sizeof(data));
  d->type = REGISTRY;
  d->data = out_reg;
  
  free_registry(instr_reg);
  ret_ans(reg, d);

}

void
op_string_eq (registry* reg)
{
  data* arg1 = lookup(reg, arbel_hash_1, 0);
  data* arg2 = lookup(reg, arbel_hash_2, 0);

  if (arg1 == NULL || arg2 == NULL)
    {
      do_error("`string-eq` requires two arguments.");
      return;
    }

  if (arg1->type != STRING || arg2->type != STRING)
    {
      do_error("Both arguments to `string-eq` must be strings.");
      return;
    }

  char* str1 = arg1->data;
  char* str2 = arg2->data;
  data* d;
  if (strcmp(str1,str2) == 0)
    {
      assign_int(&d, 1);
    }
  else
    {
      assign_int(&d, 0);
    }

  ret_ans(reg, d);
}

void
op_string_lt (registry* reg)
{
  data* arg1 = lookup(reg, arbel_hash_1, 0);
  data* arg2 = lookup(reg, arbel_hash_2, 0);

  if (arg1 == NULL || arg2 == NULL)
    {
      do_error("`string-lt` requires two arguments.");
      return;
    }

  if (arg1->type != STRING || arg2->type != STRING)
    {
      do_error("Both arguments to `string-lt` must be strings.");
      return;
    }

  char* str1 = arg1->data;
  char* str2 = arg2->data;
  data* d;
  if (strcmp(str1,str2) < 0)
    {
      assign_int(&d, 1);
    }
  else
    {
      assign_int(&d, 0);
    }

  ret_ans(reg, d);
}

void
op_string_gt (registry* reg)
{
  data* arg1 = lookup(reg, arbel_hash_1, 0);
  data* arg2 = lookup(reg, arbel_hash_2, 0);

  if (arg1 == NULL || arg2 == NULL)
    {
      do_error("`string-gt` requires two arguments.");
      return;
    }

  if (arg1->type != STRING || arg2->type != STRING)
    {
      do_error("Both arguments to `string-gt` must be strings.");
      return;
    }

  char* str1 = arg1->data;
  char* str2 = arg2->data;
  data* d;
  if (strcmp(str1,str2) > 0)
    {
      assign_int(&d, 1);
    }
  else
    {
      assign_int(&d, 0);
    }

  ret_ans(reg, d);
}

void
op_exist_in (registry* reg)
{
  data* arg1 = lookup(reg, arbel_hash_1, 0);
  data* arg2 = lookup(reg, arbel_hash_2, 0);

  if (arg1 == NULL || arg2 == NULL)
    {
      do_error("`exist-in` requires two arguments.");
      return;
    }

  if (arg1->type != REGISTER)
    {
      do_error("First argument to `exist-in` must be a register.");
      return;
    }

  if (arg2->type != REGISTRY)
    {
      do_error("Second argument to `exist-in` must be a registry.");
      return;
    }

  data* chk = get((registry*) arg2->data, ((regstr*) arg1->data)->key, 0);
  
  if (chk != NULL)
    {
      assign_int(&chk, 1);
    }
  else
    {
      assign_int(&chk, 0);
    }

  ret_ans(reg, chk);
}

int
op_reg_cmp (registry* reg)
{
  data* arg1 = lookup(reg, arbel_hash_1, 0);
  data* arg2 = lookup(reg, arbel_hash_2, 0);

  if (arg1 == NULL || arg2 == NULL)
    {
      do_error("`reg-eq` requires two arguments.");
      return -2;
    }

  if (arg1->type != REGISTER || arg2->type != REGISTER)
    {
      do_error("Both arguments to `reg-eq` should be registers.");
      return -2;
    }

  return strcmp(((regstr*) arg1->data)->name, ((regstr*) arg2->data)->name);
}

void
op_reg_eq (registry* reg)
{
  int cmp = op_reg_cmp(reg);
  if (is_error(-1))
    return;
      
  data* d;
  if (cmp == 0)
    {
      assign_int(&d,1);
    }
  else
    {
      assign_int(&d,0);
    }

  ret_ans(reg,d);
}

void
op_reg_gt (registry* reg)
{
  int cmp = op_reg_cmp(reg);
  if (is_error(-1))
    return;
  
  data* d;
  if (cmp > 0)
    {
      assign_int(&d,1);
    }
  else
    {
      assign_int(&d,0);
    }

  ret_ans(reg,d);
}

void
op_reg_lt (registry* reg)
{
  int cmp = op_reg_cmp(reg);
  if (is_error(-1))
    return;
  
  data* d;
  if (cmp < 0)
    {
      assign_int(&d,1);
    }
  else
    {
      assign_int(&d,0);
    }

  ret_ans(reg,d);
}


void
op_go_in (registry* reg)
{
  data* arg1 = get(reg, arbel_hash_1, 0);

  if (arg1 == NULL)
    {
      do_error("`go-in` requires an argument.");
      return;
    }

  if (arg1->type != REFERENCE)
    {
      do_error("Argument to `go-in` must be a reference.");
      return;
    }

  arg1 = lookup(reg, arbel_hash_1, 0);

  if (is_error(-1))
    return;

  if (arg1->type != REGISTRY)
    {
      do_error("Argument to `go-in` must be a reference to a registry.");
      return;
    }

  current_parse_registry = (registry*) arg1->data;
  
}

void
op_go_out (registry* reg)
{
  if (current_parse_registry->up == NULL)
    {
      do_error("Already at top level registry.");
      return;
    }

  current_parse_registry = current_parse_registry->up;
}

void
op_save (registry* reg)
{
  data* arg1 = lookup(reg, arbel_hash_1, 0);

  if (arg1 == NULL)
    {
      do_error("`save` requires an argument.");
      return;
    }

  if (arg1->type != STRING)
    {
      do_error("The argument to `save` must be a string.");
      return;
    }

  char* fname = (char*) arg1->data;
  FILE* f = fopen(fname, "wb");
  save_registry(f, (registry*) top_registry->data);
  data_type end = NOTHING;
  fwrite(&end, sizeof(data_type), 1, f);
  fclose(f);

}

void
op_load (registry* reg)
{
  data* arg1 = lookup(reg, arbel_hash_1, 0);

  if (arg1 == NULL)
    {
      do_error("`load` requires an argument.");
      return;
    }

  if (arg1->type != STRING)
    {
      do_error("The argument to `load` must be a string.");
      return;
    }

  char* fname = (char*) arg1->data;
  FILE* f = fopen(fname, "rb");
  read_registry(f, (registry*) top_registry->data);
  fclose(f);
  is_retval(0);

}

void
op_to_string (registry* reg)
{
  data* arg1 = lookup(reg, arbel_hash_1, 0);
  if (arg1 == NULL)
    {
      do_error("`to-string` requires an argment.");
      return;
    }

  if (arg1->type != INTEGER && arg1->type != DECIMAL)
    {
      do_error("The argument to `to-string` should be numeric.");
      return;
    }


  char* result;
  if (arg1->type == INTEGER)
    {
      int n_digits;
      if ((*(int*) arg1->data) == 0)
        {
          n_digits = 1;
        }
      else
        {
          n_digits = floor(log10(*((int*) arg1->data))) + 1;
        }
      result = malloc(sizeof(char)*(n_digits+1));
      sprintf(result, "%d", *((int*) arg1->data));
    }
  else
    {
      data* arg2 = lookup(reg, arbel_hash_2, 0);
      int prec = 6;
      if (arg2 != NULL && arg2->type != INTEGER)
        {
          do_error("The second argument to `to-string` should be an integer.");
          return;
        }
      if (arg2 != NULL)
        {
          prec = *((int*) arg2->data);
          if (prec < 0)
            {
              do_error("The second argument to `to-string` must be nonnegative.");
              return;
            }
        }
      int n_digits;
      if (prec == 0)
        {
          n_digits = 1;
        }
      else
        {
          n_digits = floor(log10(prec)) + 1;
        }
      
      char* fmt = malloc(sizeof(char)*(strlen("%.f") + n_digits + 1));
      sprintf(fmt, "%%.%df", prec);

      int int_part = floor(*((double*) arg1->data));

      int int_size;
      if (int_part == 0)
        {
          int_size = 1;
        }
      else
        {
          int_size = floor(log10(int_part)) + 1;
        }
      result = malloc(sizeof(char)*(int_size+1+prec+1));
      sprintf(result, fmt, *((double*) arg1->data));
      free(fmt);
    }

  data* d;
  assign_str(&d, result, 0);
  ret_ans(reg, d);
  
}

void
op_to_number (registry* reg)
{
  data* arg1 = lookup(reg, arbel_hash_1, 0);
  if (arg1 == NULL)
    {
      do_error("`to-number` requires an argument.");
      return;
    }

  if (arg1->type != STRING)
    {
      do_error("The argument to `to-number` must be a string.");
      return;
    }

  char* value = (char*) arg1->data;
  data* d;
  if (is_integer(value))
    {
      int result = atoi(value);
      assign_int(&d, result);
      ret_ans(reg, d);
    }
  else if (is_decimal(value) && (strcmp(value, ".") != 0))
    {
      double result = atof(value);
      assign_dec(&d, result);
      ret_ans(reg, d);
    }
  else
    {
      do_error("String not a number.");
      return;
    }
  
}

void
op_ref (registry* reg)
{
  data* arg1 = lookup(reg, arbel_hash_1, 0);
  data* arg2 = lookup(reg, arbel_hash_2, 0);

  if (arg1 == NULL || arg2 == NULL)
    {
      do_error("`ref` requires two arguments.");
      return;
    }

  if (arg1->type != REGISTER)
    {
      do_error("First argument to `ref` must be a register.");
      return;
    }

  if (arg2->type != REGISTRY)
    {
      do_error("Second argument to `ref` must be a registry.");
      return;
    }

  data* d;
  int is_regstr = 1;
  assign_ref(&d, (registry*) arg2->data,
             &((regstr*) arg1->data)->name,
             &((regstr*) arg1->data)->key,
             1,
             &is_regstr);
  ret_ans(reg, d);
}

void
op_output_code (registry* reg)
{
  data* arg1 = lookup(reg, arbel_hash_1, 0);

  if (arg1 == NULL)
    {
      do_error("`output-code` requires an argument.");
      return;
    }

  if (arg1->type != STRING)
    {
      do_error("Argument to `output-code` must be a string.");
      return;
    }

  if (source_code != NULL)
    {
      FILE* f = fopen((char*) arg1->data, "w");
      fwrite(source_code, sizeof(char), strlen(source_code), f);
      fclose(f);
    }
  else
    {
      do_error("No source saved yet.");
      return;
    }
}

void
op_clear_code (registry* reg)
{
  if (source_code != NULL)
    free(source_code);

  source_code = NULL;
}

void
op_error (registry* reg)
{
  data* arg1 = lookup(reg, arbel_hash_1, 0);

  if (arg1 == NULL)
    {
      do_error("`error` requires an argument.");
      return;
    }

  if (arg1->type != STRING)
    {
      do_error("`error` requires a string argument.");
      return;
    }

  do_error((char*) arg1->data);
}

void
op_is_type (registry* reg, const data_type type)
{
  data* arg1 = lookup(reg, arbel_hash_1, 0);
  if (arg1 == NULL)
    {
      do_error("Assert instruction requires an argument.");
      return;
    }

  data* d;
  if (arg1->type == type || ((type==INSTRUCTION) &&
                             (arg1->type==OPERATION)))
    {
      assign_int(&d, 1);
    }
  else
    {
      assign_int(&d, 0);
    }

  ret_ans(reg,d);
}

void
op_is_integer (registry* reg)
{
  op_is_type(reg, INTEGER);
}

void
op_is_decimal (registry* reg)
{
  op_is_type(reg, DECIMAL);
}

void
op_is_string (registry* reg)
{
  op_is_type(reg, STRING);
}

void
op_is_register (registry* reg)
{
  op_is_type(reg, REGISTER);
}

void
op_is_registry (registry* reg)
{
  op_is_type(reg, REGISTRY);
}

void
op_is_instruction (registry* reg)
{
  op_is_type(reg, INSTRUCTION);
}

void
op_is_file (registry* reg)
{
  op_is_type(reg, ARBEL_FILE);
}

void
op_is_nothing (registry* reg)
{
  op_is_type(reg, NOTHING);
}


void
op_open_text_file (registry* reg)
{
  data* arg1 = lookup(reg, arbel_hash_1, 0);

  if (arg1 == NULL)
    {
      do_error("`open-text-file` requires an argument.");
      return;
    }

  if (arg1->type != STRING)
    {
      do_error("Argument to `open-text-file` must be a string.");
      return;
    }
  
  FILE* f = fopen((char*) arg1->data, "r+");
  if (f == NULL)
    {
      do_error("File did not open.  Possibly, it does not exist.");
      return;
    }

  data* d;
  assign_file(&d,f);
  ret_ans(reg,d);

}

void
op_read (registry* reg)
{
  data* arg1 = lookup(reg, arbel_hash_1, 0);

  if (arg1 == NULL)
    {
      do_error("`read` requires an argument.");
      return;
    }

  if (arg1->type != ARBEL_FILE)
    {
      do_error("Argument to `read` must be a file.");
      return;
    }

  char c = fgetc((FILE*) arg1->data);
  char* ret;
  data* d;
  if (c == EOF || c == '\0')
    {
      assign_nothing(&d);
    }
  else
    {
      ret = malloc(sizeof(char)*2);
      ret[0] = c;
      ret[1] = '\0';
      assign_str(&d, ret, 0);
    }
  ret_ans(reg,d);
}

void
op_close (registry* reg)
{
  data* arg1 = lookup(reg, arbel_hash_1, 0);

  if (arg1 == NULL)
    {
      do_error("`close` requires an argument.");
      return;
    }

  if (arg1->type != REGISTER)
    {
      do_error("Argument to `close` must be a register.");
      return;
    }

  data* f = lookup(reg->up, ((regstr*) arg1->data)->key, 0);

  if (f == NULL)
    {
      do_error("Register does not exist.");
      return;
    }

  if (f->type != ARBEL_FILE)
    {
      do_error("Register does not contain a file.");
      return;
    }

  if (f->data != NULL)
    {
      fclose((FILE*) f->data);
    }

  del(reg->up, ((regstr*) arg1->data)->key, 1);
  
}

void
op_or (registry* reg)
{
  data* arg1 = lookup(reg, arbel_hash_1, 0);
  data* arg2 = lookup(reg, arbel_hash_2, 0);

  if (arg1 == NULL || arg2 == NULL)
    {
      do_error("`or` requires two arguments.");
      return;
    }

  if (arg1->type != INTEGER || arg2->type != INTEGER)
    {
      do_error("Both arguments to `or` should be integers.");
      return;
    }

  data* d;
  if (*((int*) arg1->data) || *((int*) arg2->data))
    {
      assign_int(&d, 1);
    }
  else
    {
      assign_int(&d,0);
    }

  ret_ans(reg,d);
}

void
op_and (registry* reg)
{
  data* arg1 = lookup(reg, arbel_hash_1, 0);
  data* arg2 = lookup(reg, arbel_hash_2, 0);

  if (arg1 == NULL || arg2 == NULL)
    {
      do_error("`and` requires two arguments.");
      return;
    }

  if (arg1->type != INTEGER || arg2->type != INTEGER)
    {
      do_error("Both arguments to `and` should be integers.");
      return;
    }

  data* d;
  if (*((int*) arg1->data) && *((int*) arg2->data))
    {
      assign_int(&d, 1);
    }
  else
    {
      assign_int(&d,0);
    }

  ret_ans(reg,d);
}

void
op_not (registry* reg)
{
  data* arg1 = lookup(reg, arbel_hash_1, 0);

  if (arg1 == NULL)
    {
      do_error("`not` requires an argument.");
      return;
    }

  if (arg1->type != INTEGER)
    {
      do_error("The argument to `not` should be an integer.");
      return;
    }

  data* d;
  if (*((int*) arg1->data))
    {
      assign_int(&d, 0);
    }
  else
    {
      assign_int(&d, 1);
    }

  ret_ans(reg,d);
}

void
op_read_line (registry* reg)
{
  data* arg1 = lookup(reg, arbel_hash_1, 0);

  if (arg1 == NULL)
    {
      do_error("`read-line` requires an argument.");
      return;
    }

  if (arg1->type != ARBEL_FILE)
    {
      do_error("`read-line`'s argument must be a File.");
      return;
    }


  char* line  = NULL;
  size_t len = 0;
  ssize_t ret = getline(&line, &len, (FILE*) arg1->data);
  line[strlen(line)-1] = '\0';
  data* d;
  if (ret >= 0)
    {
      assign_str(&d, line, 0);
      ret_ans(reg,d);
    }
  else
    {
      assign_nothing(&d);
      ret_ans(reg,d);
      free(line);
    }
}

void
op_write (registry* reg)
{
  data* arg1 = lookup(reg, arbel_hash_1, 0);
  data* arg2 = lookup(reg, arbel_hash_2, 0);

  if (arg1 == NULL || arg2 == NULL)
    {
      do_error("`write` requires two arguments.");
      return;
    }

  if (arg1->type != STRING)
    {
      do_error("The first argument to `write` must be a string.");
      return;
    }

  if (arg2->type != ARBEL_FILE)
    {
      do_error("The second argument to `write` must be a file.");
      return;
    }

  fwrite((char*) arg1->data, sizeof(char), strlen((char*) arg1->data), (FILE*) arg2->data);
  data* d;
  assign_nothing(&d);
  ret_ans(reg,d);
}

void
op_input (registry* reg)
{
  data* arg1 = lookup(reg, arbel_hash_1, 0);
  if (arg1 == NULL)
    {
      do_error("`input` requires an argument.");
      return;
    }

  if (arg1->type != REGISTER)
    {
      do_error("Argument to `input` should be a register.");
      return;
    }

  char* input = readline("");
  data* d;
  assign_str(&d, input, 0);
  
  set(reg->up, d, ((regstr*) arg1->data)->name);
  
}

void
op_shell (registry* reg)
{
  data* arg1 = lookup(reg, arbel_hash_1, 0);
  if (arg1 == NULL)
    {
      do_error("`shell` requires an argument.");
      return;
    }

  if (arg1->type != STRING)
    {
      do_error("Argument to `shell` must be a string.");
      return;
    }

  char* cmd = malloc(sizeof(char)*(strlen("sh -c ")+strlen((char*) arg1->data)+1));
  strcpy(cmd, "sh -c ");
  strcat(cmd, (char*) arg1->data);
  FILE* f = popen(cmd, "r");
  free(cmd);
  
  if (f == NULL)
    {
      do_error("Command failed.");
      return;
    }

  char buffer[1024];
  data* d = NULL;
  while (fgets(buffer, sizeof(buffer)-1, f) != NULL)
    {
      if (d == NULL)
        {
          assign_str(&d, buffer, 1);
        }
      else
        {
          d->data = realloc(d->data, sizeof(char)*(strlen((char*) d->data)+strlen(buffer)+1));
          strcat((char*) d->data, buffer);
        }
    }

  pclose(f);

  ret_ans(reg,d);
      
  
}

void
op_link (registry* reg)
{
  data* arg1 = lookup(reg, arbel_hash_1, 0);
  data* arg2 = lookup(reg, arbel_hash_2, 0);
  data* arg3 = lookup(reg, arbel_hash_3, 0);
  if (arg1 == NULL || arg2 == NULL || arg3 == NULL)
    {
      do_error("`link` requires three arguments.");
      return;
    }

  if (arg1->type != STRING || arg2->type != STRING ||
      arg3->type != STRING)
    {
      do_error("All arguments to `link` must be strings.");
      return;
    }

  void* lib = dlopen((char*) arg1->data, RTLD_LAZY);

  if (lib == NULL)
    {
      printf("%s\n", dlerror());
      do_error("Library failed to open.");
      return;
    }

  operation new_op = dlsym(lib, (char*) arg2->data);
  if (new_op == NULL)
    {
      do_error("Error loading function.");
      dlclose(lib);
      return;
    }

  data* d;
  assign_op(&d, new_op);
  set((registry*) top_registry->data, d, (char*) arg3->data);

  if (arbel_ll == NULL)
    {
      
      arbel_ll = malloc(sizeof(void*));
    }
  else
    {
      arbel_ll = realloc(arbel_ll,
                         sizeof(void*)*(arbel_ll_cnt+1));
    }
  arbel_ll[arbel_ll_cnt] = lib;
  arbel_ll_cnt++;

}

void
op_match (registry* reg)
{
  data* arg1 = lookup(reg, arbel_hash_1, 0);
  data* arg2 = lookup(reg, arbel_hash_2, 0);
  data* arg3 = lookup(reg, arbel_hash_3, 0);

  if (arg1 == NULL || arg2 == NULL)
    {
      do_error("`match` requires two arguments.");
      return;
    }

  if (arg1->type != STRING || arg2->type != STRING)
    {
      do_error("The first two arguments to `match` should be strings.");
      return;
    }

  int max_matches = 0;
  if (arg3 != NULL && arg3->type != INTEGER)
    {
      do_error("The third argument to `match` must be an integer.");
      return;
    }
  else if (arg3 != NULL)
    {
      max_matches = *((int*) arg3->data);
    }

  regex_t regex;
  int error = regcomp(&regex, (char*) arg1->data, REG_EXTENDED);
  if (error)
    {
      do_error("Error compiling regular expression.");
      return;
    }

  size_t n_groups = 10*(regex.re_nsub+1);
  regmatch_t* matches;


  data* d;
  assign_registry(&d, NULL);
  int i;
  int j;
  char* to_add;
  char* name;
  data* d_str;
  data* d_reg;
  int n_matches = 0;
  char* cursor = (char*) arg2->data;
  size_t offset;
  while (max_matches <= 0 || n_matches < max_matches)
    {
      matches = malloc(sizeof(regmatch_t)*n_groups);
      error = regexec(&regex, cursor, n_groups, matches, 0);
      if (!error)
        {
          assign_registry(&d_reg, NULL);
          offset = 0;
          for (i=0; i < n_groups; i++)
            {
              if (matches[i].rm_so < 0)
                {
                  break;
                }
              else
                {
                  if (i==0) offset = matches[0].rm_eo;

                  to_add = malloc(sizeof(char)*(matches[i].rm_eo-
                                                matches[i].rm_so+2));
                  for (j=matches[i].rm_so; j < matches[i].rm_eo; j++)
                    {
                      to_add[j-matches[i].rm_so] = cursor[j];
                    }
                  to_add[j-matches[i].rm_so] = '\0';
                  assign_str(&d_str, to_add, 0);
                  name = argument_name(i);
                  set((registry*) d_reg->data, d_str, name);
                  free(name);
                }
            }
          n_matches++;
          name = argument_name(n_matches);
          set((registry*) d->data, d_reg, name);
          free(name);

          free(matches);
          cursor += offset;
        }
      else
        {
          free(matches);
          break;
        }
    }

  ret_ans(reg, d);
  
}

void
op_replace (registry* reg)
{
  data* arg1 = lookup(reg, arbel_hash_1, 0);
  data* arg2 = lookup(reg, arbel_hash_2, 0);
  data* arg3 = lookup(reg, arbel_hash_3, 0);
  data* arg4 = lookup(reg, arbel_hash_4, 0);

  if (arg1 == NULL || arg2 == NULL || arg3 == NULL)
    {
      do_error("`replace` requires three arguments.");
      return;
    }

  if (arg1->type != STRING || arg2->type != STRING
      || arg3->type != STRING)
    {
      do_error("All arguments to `replace` must be strings.");
      return;
    }

  int max_replace = 0;
  if (arg4 != NULL && arg4->type != INTEGER)
    {
      do_error("The fourth argument to `replace` must be an integer.");
      return;
    }
  else if (arg4 != NULL)
    {
      max_replace = *((int*) arg4->data);
    }

  regex_t regex;
  int error = regcomp(&regex, (char*) arg1->data, REG_EXTENDED);
  if (error)
    {
      do_error("Error compiling regular expression.");
      return;
    }

  regmatch_t* matches = malloc(sizeof(regmatch_t));

  data* d;
  assign_registry(&d, NULL);


  char* cursor = (char*) arg3->data;

  /* contains pairs of integers denoting ranges to copy from arg3->data.  After each pair insert string. */
  size_t offset;
  size_t n_ranges = 1;
  while (max_replace <= 0 || n_ranges <= max_replace)
    {
      error = regexec(&regex, cursor, 1, &matches[n_ranges-1], 0);
      if (!error)
        {
          offset = 0;
          if (matches[n_ranges].rm_so < 0)
            {
              break;
            }
          else
            {
              offset = matches[n_ranges-1].rm_eo;
              n_ranges++;
              matches = realloc(matches, n_ranges*sizeof(regmatch_t));
              cursor += offset;
            }
        }
      else
        {
          break;
        }
    }
  n_ranges--;

  size_t last = 0;
  int i;
  int j;
  char* final;
  cursor = (char*) arg3->data;
  /* calculate size of final string */
  size_t final_sz = 0;
  size_t last_range = 0;
  for (i = 0; i < n_ranges; i++)
    {
      final_sz += matches[i].rm_so;
      final_sz += strlen((char*) arg2->data);
      last_range += matches[i].rm_eo;
    }
  final_sz += strlen((char*) arg3->data) - last_range;

  final = malloc(sizeof(char)*(final_sz+1));
  for (i = 0; i < n_ranges; i++)
    {
      for (j = 0; j < matches[i].rm_so; j++)
        {
          final[last+j] = cursor[j];
        }

      last += matches[i].rm_so;

      for (j = 0; j < strlen((char*)arg2->data); j++)
        {
          final[last+j] = ((char*) arg2->data)[j];
        }

      last += strlen((char*)arg2->data);
      
      cursor += matches[i].rm_eo;
    }

  for (j = 0; j < (final_sz-last); j++)
    {
      final[last+j] = cursor[j];
    }
  final[final_sz] = '\0';

  assign_str(&d, final, 0);
  ret_ans(reg,d);

  free(matches);

}

void
op_log (registry* reg)
{
  data* arg1 = lookup(reg, arbel_hash_1, 0);

  if (arg1 == NULL)
    {
      do_error("`log` requires an argument.");
      return;
    }

  if (arg1->type != INTEGER && arg1->type != DECIMAL)
    {
      do_error("`log` requires a numeric argument.");
      return;
    }

  data* d;
  if (arg1->type == INTEGER)
    {
      assign_dec(&d, log((double) (*((int*) arg1->data))));
    }
  else
    {
      assign_dec(&d, log(*((double*) arg1->data)));
    }

  ret_ans(reg,d);
}


void
op_exp (registry* reg)
{
  data* arg1 = lookup(reg, arbel_hash_1, 0);

  if (arg1 == NULL)
    {
      do_error("`exp` requires an argument.");
      return;
    }

  if (arg1->type != INTEGER && arg1->type != DECIMAL)
    {
      do_error("`exp` requires a numeric argument.");
      return;
    }

  data* d;
  if (arg1->type == INTEGER)
    {
      assign_dec(&d, exp((double) (*((int*) arg1->data))));
    }
  else
    {
      assign_dec(&d, exp(*((double*) arg1->data)));
    }

  ret_ans(reg,d);
}

void
op_to_power (registry* reg)
{
  data* arg1 = lookup(reg, arbel_hash_1, 0);
  data* arg2 = lookup(reg, arbel_hash_2, 0);
  
  if (arg1 == NULL || arg2 == NULL)
    {
      do_error("`to-power` requires two arguments.");
      return;
    }

  if (arg1->type != INTEGER && arg1->type != DECIMAL
      && arg2->type != INTEGER && arg2->type != DECIMAL)
    {
      do_error("`to-power` requires two numeric argument.");
      return;
    }

  double base;
  double power;
  data* d;
  if (arg1->type == INTEGER)
    {
      base = (double) (*((int*) arg1->data));
    }
  else
    {
      base = *((double*) arg1->data);
    }

  if (arg2->type == INTEGER)
    {
      power = (double) (*((int*) arg2->data));
    }
  else
    {
      power = *((double*) arg2->data);
    }

  assign_dec(&d, pow(base,power));

  ret_ans(reg,d);
}

void
op_chdir (registry* reg)
{
  data* arg1 = lookup(reg, arbel_hash_1, 0);
  if (arg1 == NULL)
    {
      do_error("`chdir` requires an argument.");
      return;
    }

  if (arg1->type != STRING)
    {
      do_error("The argument to `chdir` must be a string.");
      return;
    }

  int error = chdir((char*) arg1->data);
  if (error)
    {
      do_error("Could not change directory.");
      return;
    }

}

void
op_copy_file (registry* reg)
{
  data* arg1 = lookup(reg, arbel_hash_1, 0);
  data* arg2 = lookup(reg, arbel_hash_2, 0);
  if (arg1 == NULL || arg2 == NULL)
    {
      do_error("`copy-file` requires an argument.");
      return;
    }

  if (arg1->type != STRING || arg2->type != STRING)
    {
      do_error("Both arguments to `copy-file` must be a string.");
      return;
    }

  FILE* f_in = fopen((char*) arg1->data, "rb");
  FILE* f_out = fopen((char*) arg2->data, "wb");
  int c;
  while ((c = fgetc(f_in)) != EOF)
    {
      c = fputc(c, f_out);
      if (c == EOF)
        {
          do_error("Error writing to file.");
          break;
        }
    }

  fclose(f_in);
  fclose(f_out);

}

void
op_import (registry* reg)
{
  data* arg1 = lookup(reg, arbel_hash_1, 0);

  if (arg1 == NULL)
    {
      do_error("`import` requires an argument.");
      return;
    }

  if (arg1->type != REGISTRY)
    {
      do_error("The argument to `import` must be a registry.");
      return;
    }

  registry* r1 = (registry*) arg1->data;
  r1 = tail(r1);
  
  data* d = NULL;
  while (r1 != NULL)
    {
      d = r1->value;
      if (d != NULL)
        {
          d = copy_data(d);
          set(reg->up, d, (char*) r1->name);
        }
      r1 = r1->right;
    }
}


void
op_curdir (registry* reg)
{
  char* dir = get_current_dir_name();
  if (dir == NULL)
    {
      do_error("Error getting current directory.");
      return;
    }
  data* d;
  assign_str(&d, dir, 0);
  ret_ans(reg,d);
}
  

void
add_basic_ops (registry* reg)
{
  data* d;

  reg->left = NULL;
  reg->right = NULL;
  reg->up = NULL;

  assign_op(&d, op_set);
  set(reg, d, "set");
  
  assign_op(&d, op_add);
  set(reg,d,"add");

  assign_op(&d, op_multiply);
  set(reg,d,"multiply");

  assign_op(&d, op_subtract);
  set(reg,d,"subtract");

  assign_op(&d, op_divide);
  set(reg,d,"divide");
  
  assign_op(&d, op_if);
  set(reg,d,"if");

  assign_op(&d, op_compute);
  set(reg,d,"compute");

  assign_op(&d, op_reg);
  set(reg,d,"reg");

  assign_op(&d, op_get);
  set(reg,d,"get");

  assign_op(&d, op_mov);
  set(reg,d,"mov");

  assign_op(&d, op_del);
  set(reg,d,"del");

  assign_op(&d, op_exit);
  set(reg,d,"exit");

  assign_op(&d, op_answer);
  set(reg,d,"answer");

  assign_op(&d, op_sit);
  set(reg,d,"sit");

  assign_op(&d, op_exist);
  set(reg,d,"exist");

  assign_op(&d, op_gt);
  set(reg,d,"gt");

  assign_op(&d, op_lt);
  set(reg,d,"lt");

  assign_op(&d, op_eq);
  set(reg,d,"eq");

  assign_op(&d, op_lteq);
  set(reg,d,"lteq");

  assign_op(&d, op_gteq);
  set(reg,d,"gteq");
  
  assign_op(&d, op_print);
  set(reg,d,"print");

  assign_op(&d, op_character);
  set(reg,d,"character");
  
  assign_op(&d, op_count_characters);
  set(reg,d,"count-characters");

  assign_op(&d, op_concat);
  set(reg,d,"concat");

  assign_op(&d, op_source);
  set(reg,d,"source");

  assign_op(&d, op_do_to_all);
  set(reg,d,"do-to-all");

  assign_op(&d, op_next);
  set(reg,d,"next");

  assign_op(&d, op_last);
  set(reg,d,"last");

  assign_op(&d, op_in);
  set(reg,d,"in");

  assign_op(&d, op_while);
  set(reg,d,"while");
  
  assign_op(&d, op_list);
  set(reg,d,"list");

  assign_op(&d, op_to_register);
  set(reg,d,"to-register");

  assign_op(&d, op_collapse);
  set(reg,d,"collapse");

  assign_op(&d, op_join);
  set(reg,d,"join");

  assign_op(&d, op_string_eq);
  set(reg,d,"string-eq");

  assign_op(&d, op_string_gt);
  set(reg,d,"string-gt");
  
  assign_op(&d, op_string_lt);
  set(reg,d,"string-lt");

  assign_op(&d, op_exist_in);
  set(reg,d,"exist-in");

  assign_op(&d, op_reg_eq);
  set(reg,d,"reg-eq");

  assign_op(&d, op_reg_lt);
  set(reg,d,"reg-lt");

  assign_op(&d, op_reg_gt);
  set(reg,d,"reg-gt");

  assign_op(&d, op_go_in);
  set(reg,d,"go-in");

  assign_op(&d, op_go_out);
  set(reg,d,"go-out");

  assign_op(&d, op_save);
  set(reg,d,"save");

  assign_op(&d, op_load);
  set(reg,d,"load");

  assign_op(&d, op_to_string);
  set(reg,d,"to-string");
  
  assign_op(&d, op_to_number);
  set(reg,d,"to-number");

  assign_op(&d, op_ref);
  set(reg,d,"ref");

  assign_op(&d, op_output_code);
  set(reg,d,"output-code");

  assign_op(&d, op_clear_code);
  set(reg,d,"clear-code");

  assign_op(&d, op_error);
  set(reg,d,"error");

  assign_op(&d, op_is_integer);
  set(reg,d,"is-integer");

  assign_op(&d, op_is_decimal);
  set(reg,d,"is-decimal");

  assign_op(&d, op_is_string);
  set(reg,d,"is-string");
  
  assign_op(&d, op_is_register);
  set(reg,d,"is-register");

  assign_op(&d, op_is_registry);
  set(reg,d,"is-registry");

  assign_op(&d, op_is_instruction);
  set(reg,d,"is-instruction");

  assign_op(&d, op_is_file);
  set(reg,d,"is-file");

  assign_op(&d, op_is_nothing);
  set(reg,d,"is-nothing");

  assign_op(&d, op_open_text_file);
  set(reg,d,"open-text-file");

  assign_op(&d, op_read);
  set(reg,d,"read");

  assign_op(&d, op_close);
  set(reg,d,"close");

  assign_op(&d, op_and);
  set(reg,d,"and");

  assign_op(&d, op_or);
  set(reg,d,"or");

  assign_op(&d, op_not);
  set(reg,d,"not");

  assign_op(&d, op_read_line);
  set(reg,d,"read-line");
  
  assign_op(&d, op_write);
  set(reg,d,"write");
  
  assign_op(&d, op_input);
  set(reg,d,"input");

  assign_op(&d, op_shell);
  set(reg,d,"shell");

  assign_op(&d, op_link);
  set(reg,d,"link");

  assign_op(&d, op_match);
  set(reg,d,"match");

  assign_op(&d, op_replace);
  set(reg,d,"replace");

  assign_op(&d, op_log);
  set(reg,d,"log");

  assign_op(&d, op_exp);
  set(reg,d,"exp");

  assign_op(&d, op_to_power);
  set(reg,d,"to-power");

  assign_op(&d, op_chdir);
  set(reg,d,"chdir");

  assign_op(&d, op_curdir);
  set(reg,d,"curdir");

  assign_op(&d, op_copy_file);
  set(reg,d,"copy-file");

  assign_op(&d, op_import);
  set(reg,d,"import");
  
  assign_op(&d, op_repeat);
  set(reg,d,"repeat");
  
}
  
