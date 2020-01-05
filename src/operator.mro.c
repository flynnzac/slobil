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

`#include "arbel.h"'

#CHECK_ARGS=`check_length(&#a~, #length~+1);
if (is_error(-1)) return;'@


#GETARG=`
data* arg#num~ = resolve(a.arg_array[#num~], reg);

if (#requireans~)
  {
    if (arg#num~ == NULL)
      {
        do_error("#op~ requires at least #num~ arguments.");
        return #retfail~;
      }
  }
if (arg#num~ != NULL && #checktype~ && (!(arg#num~->type & #type~)))
  {
    do_error("Argument #num~ of #op~ should be of type #type~.");
    return #retfail~;
  }
'
@;

#requireans=true@
#checktype=true@


void
op_range (arg a, registry* reg)
{
  #a=a@
  #length=2@
  ##CHECK_ARGS~$;

  #op=range@
  #num=1@
  #type=INTEGER@
  ##GETARG~$;

  #num=2@
  ##GETARG~$;

  int by = 1;
  if (a.length >= 4)
    {
      #num=3@
      ##GETARG~$;
      by = *((int*) arg3->data);
    }

  int lb = *((int*) arg1->data);
  int ub = *((int*) arg2->data);


  registry* r_new = new_registry(reg, new_hash_size(ceil((ub-lb)/by)));

  int cur = lb;
  int i = 1;
  char* r = NULL;
  data* d_tmp;
  while (cur <= ub)
    {
      r = argument_name(i);
      assign_int(&d_tmp, cur);
      set(r_new, d_tmp, r, 0);
      free(r);
      i++;
      cur += by;
    }

  d_tmp = malloc(sizeof(data));
  d_tmp->type = REGISTRY;
  d_tmp->data = r_new;

  ret_ans(reg,d_tmp);
  
}

void
op_list (arg a, registry* reg)
{
  registry* r_new = new_registry(reg, new_hash_size(a.length));
  data* d;
  data* d_new;
  int i = 1;
  char* r = NULL;

  for (i=1; i < a.length; i++)
    {
      r = argument_name(i);
      d_new = copy_data(resolve(a.arg_array[i], reg));
      set(r_new, d_new, r, 0);
      free(r);
    }

  d = malloc(sizeof(data));
  d->type = REGISTRY;
  d->data = r_new;
  ret_ans(reg, d);

}

void
op_registry (arg a, registry* reg)
{
  if (a.length != 1)
    {
      #length=2@
      ##CHECK_ARGS~$;
    }
  
  registry* r_new = new_registry(reg, new_hash_size(a.length / 2 + 1));
  data* d = NULL;
  data* d_data = NULL;
  data* d_new;

  for (int i = 1; i < a.length; i = i + 2)
    {
      d = a.arg_array[i];
      
      if (d->type != REGISTER)
        {
          do_error("Expected a register");
          free_registry(r_new);
          return;
        }

      d_data = resolve(a.arg_array[i+1], reg);
      d_new = copy_data(d_data);
      set(r_new, d_new, ((regstr*) d->data)->name, 0);
    }


  d = malloc(sizeof(data));
  d->type = REGISTRY;
  d->data = r_new;
  ret_ans(reg, d);
}

void
op_arithmetic (arg a, registry* reg, const int code)
{
  data* d;
  data_type result_type = INTEGER;
  int int_value = 0;
  double dbl_value = 0.0;
  for (int i = 1; i < a.length; i++)
    {
      data* cur = resolve(a.arg_array[i], reg);
      if (cur == NULL || !is_numeric(cur))
        {
          do_error("Arithmetic requires at least two numeric arguments.");
          return;
        }

      if (cur->type == REAL && result_type == INTEGER)
        {
          dbl_value = (double) int_value;
          result_type = REAL;
        }

      if (result_type == INTEGER)
        {
          if (i == 1)
            int_value = *((int*) cur->data);
          else
            switch (code)
              {
              case 1:
                int_value += *((int*) cur->data);
                break;
              case 2:
                int_value *= *((int*) cur->data);
                break;
              case 3:
                int_value -= *((int*) cur->data);
                break;
              case 4:
                int_value /= *((int*) cur->data);
                break;
              }
        }
      else
        {
          double val;
          if (cur->type == INTEGER)
            val = (double) (*((int*) cur->data));
          else
            val = *((double*) cur->data);
	    
          if (i == 1)
            dbl_value = val;
          else
            switch (code)
              {
              case 1:
                dbl_value += val;
                break;
              case 2:
                dbl_value *= val;
                break;
              case 3:
                dbl_value -= val;
                break;
              case 4:
                dbl_value /= val;
              }
        }
    }

  if (result_type == INTEGER)
    assign_int(&d, int_value);
  else
    assign_real(&d, dbl_value);
	      
  ret_ans(reg, d);
}  

void
op_add (arg a, registry* reg)
{
  #length=2@
  ##CHECK_ARGS~$;

  op_arithmetic(a, reg, 1);
}

void
op_mul (arg a, registry* reg)
{
  #length=2@
  ##CHECK_ARGS~$;

  op_arithmetic(a, reg, 2);
}

void
op_sub (arg a, registry* reg)
{
  #length=2@
  ##CHECK_ARGS~$;

  op_arithmetic(a, reg, 3);
}

void
op_div (arg a, registry* reg)
{
  #length=2@
  ##CHECK_ARGS~$;

  op_arithmetic(a, reg, 4);
}

void
op_set (arg a, registry* reg)
{
  #length=2@
  ##CHECK_ARGS~$;

  #op=set@
  #num=1@
  #type=REGISTER@
  ##GETARG~$;

  #num=2@
  #checktype=false@
  ##GETARG~$;
  #checktype=true@

  registry* to_set;
  if (a.length >= 4)
    {
      #num=3@
      #type=REGISTRY@
      ##GETARG~$;
      
      to_set = (registry*) arg3->data;
    }
  else
    {
      to_set = reg;
    }
  
  char* name = ((regstr*) arg1->data)->name;
  data* d = copy_data(arg2);
  set(to_set, d, name, 1);


}

void
op_get (arg a, registry* reg)
{
  #length=1@
  ##CHECK_ARGS~$;

  #op=get@
  #num=1@
  #type=REGISTER@
  ##GETARG~$;
  
  registry* to_look;
  if (a.length == 2)
    {
      to_look = reg;
    }
  else
    {
      #num=2@
      #type=REGISTRY@
      ##GETARG~$;

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
op_if (arg a, registry* reg)
{
  #length=1@
  ##CHECK_ARGS~$;

  #op=if@
  #num=1@
  #type=BOOLEAN@
  ##GETARG~$;

  #requireans=false@
  if (*((bool*) arg1->data))
    {
      if (a.length >= 3)
        {
          #num=2@
          #checktype=false@
          ##GETARG~$;
          #checktype=true@

          if (arg2 != NULL)
            {
              data* d = copy_data(arg2);
              ret_ans(reg, d);
            }
        }
    }
  else
    {
      if (a.length >= 4)
        {
          #num=3@
          #checktype=false@
          ##GETARG~$;
          #checktype=true@

          if (arg3 != NULL)
            {
              data* d = copy_data(arg3);
              ret_ans(reg,d);
            }
        }
    }
  #requireans=true@
}

void
op_sit (arg a, registry* reg)
{
  return;
}


void
op_move (arg a, registry* reg)
{
  #length=2@
  ##CHECK_ARGS~$;

  #op=move@
  #num=1@
  #type=REGISTER@
  ##GETARG~$;

  #num=2@
  ##GETARG~$;

  mov(reg, (regstr*) arg1->data,
      (regstr*) arg2->data);
  
}

void
op_delete (arg a, registry* reg)
{
  #length=1@
  ##CHECK_ARGS~$;

  #op=delete@
  #num=1@
  #type=REGISTER@
  ##GETARG~$;
  
  del(reg, ((regstr*) arg1->data)->key, 1);
}

void
op_exit (arg a, registry* reg)
{
  is_exit(1);
  is_error(1);
}

void
op_exist (arg a, registry* reg)
{
  #length=1@
  ##CHECK_ARGS~$;

  #op=exist@
  #num=1@
  #type=REGISTER@
  ##GETARG~$;

  registry* to_use = reg;
  if (a.length >= 3)
    {
      #num=2@
      #type=REGISTRY@
      ##GETARG~$;

      to_use = (registry*) arg2->data;
    }

  data* obj = get(to_use, ((regstr*) arg1->data)->key, 0);
  data* d;
  if (obj == NULL)
    {
      assign_boolean(&d, false);
    }
  else
    {
      assign_boolean(&d, true);
    }

  ret_ans(reg,d);
}

void
op_answer (arg a, registry* reg)
{
  #length=1@
  ##CHECK_ARGS~$;

  #op=answer@
  #num=1@
  #checktype=false@
  ##GETARG~$;
  #checktype=true@

  data* d = copy_data(arg1);
  ret_ans(reg, d);
}

int
op_comparison (arg a, registry* reg)
{
  #length=2@
  ##CHECK_ARGS~$;

  #op=comparison@
  #num=1@
  #type=`(INTEGER|REAL)'@
  #retfail=-2@
  ##GETARG~$;

  #num=2@
  ##GETARG~$;

  #retfail=@

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
op_gt (arg a, registry* reg)
{
  data* d = NULL;
  int is_gt = op_comparison(a, reg);

  if (is_error(-1))
    return;

  if (is_gt==1)
    assign_boolean(&d, true);
  else
    assign_boolean(&d, false);

  ret_ans(reg,d);
}

void
op_lt (arg a, registry* reg)
{
  data* d = NULL;
  int is_lt = op_comparison(a, reg);

  if (is_error(-1))
    return;

  if (is_lt==-1)
    assign_boolean(&d, true);
  else
    assign_boolean(&d, false);

  ret_ans(reg,d);
}

void
op_eq (arg a, registry* reg)
{
  data* d = NULL;
  int is_eq = op_comparison(a, reg);

  if (is_error(-1))
    return;

  if (is_eq==0)
    assign_boolean(&d, true);
  else
    assign_boolean(&d, false);

  ret_ans(reg,d);
}

void
op_gteq (arg a, registry* reg)
{
  data* d = NULL;
  int is_gteq = op_comparison(a, reg);

  if (is_error(-1))
    return;

  if (is_gteq==1 || is_gteq==0)
    assign_boolean(&d, true);
  else
    assign_boolean(&d, false);

  ret_ans(reg,d);
}


void
op_lteq (arg a, registry* reg)
{
  data* d = NULL;
  int is_lteq = op_comparison(a, reg);

  if (is_error(-1))
    return;

  if (is_lteq==-1 || is_lteq==0)
    assign_boolean(&d, true);
  else
    assign_boolean(&d, false);

  ret_ans(reg,d);
}

void
op_print (arg a, registry* reg)
{
  #length=1@
  ##CHECK_ARGS~$;

  #op=print@
  #num=1@
  #checktype=false@
  ##GETARG~$;
  #checktype=true@

  print_data(arg1,1);
}

void
op_character (arg a, registry* reg)
{
  #length=2@
  ##CHECK_ARGS~$;

  #op=character@
  #num=1@
  #type=STRING@
  ##GETARG~$;

  #num=2@
  #type=INTEGER@
  ##GETARG~$;

  char* str = (char*) arg1->data;
  int loc = *((int*) arg2->data);

  if (loc <= 0)
    {
      loc += strlen(str);
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
op_count_characters (arg a, registry* reg)
{
  #length=1@
  ##CHECK_ARGS~$;

  data* arg1 = resolve(a.arg_array[1], reg);

  if (arg1 == NULL)
    {
      do_error("*count-characters* requires an argument.");
      return;
    }

  if (arg1->type != STRING)
    {
      do_error("*count-characters*'s argument must be a string.");
      return;
    }

  int len = strlen((char*) arg1->data);

  data* d;
  assign_int(&d, len);

  ret_ans(reg, d);
  
}

void
op_concat (arg a, registry* reg)
{
  #length=2@
  ##CHECK_ARGS~$;

  #op=concat@
  #num=1@
  #type=STRING@
  ##GETARG~$;

  #num=2@
  ##GETARG~$;

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
op_source (arg a, registry* reg)
{
  #length=1@
  ##CHECK_ARGS~$;

  #op=source@
  #num=1@
  #type=STRING@
  ##GETARG~$;

  FILE* f = fopen((char*) arg1->data, "r");
  if (f == NULL)
    {
      char* msg = malloc(sizeof(char)*(strlen("File not found.")
                                       + strlen((char*) arg1->data) +
                                       6));
      sprintf(msg, "File *%s* not found.", (char*) arg1->data);
      do_error(msg);
      free(msg);
      return;
    }

  struct parser_state state = fresh_state(0);
  interact(f, &state, reg);
  fclose(f);

}

void
op_do_to_all (arg a, registry* reg)
{
  CHECK_ARGS(a, 2);
  data* arg1 = resolve(a.arg_array[1], reg);
  data** arg_registries = malloc(sizeof(data*)*
                                 (a.length-2));
  for (int i = 0; i < (a.length - 2); i++)
    {
      arg_registries[i] = resolve(a.arg_array[i+2], reg);
    }

  if (arg1 == NULL)
    {
      do_error("*do-to-all* requires an argument.");
      free(arg_registries);
      return;
    }

  for (int i = 0; i < (a.length-2); i++)
    {
      if (arg_registries[i] == NULL)
        {
          do_error("*do-to-all* requires more than one argument.");
          free(arg_registries);
          return;
        }
    }
  

  if (arg1->type != INSTRUCTION)
    {
      do_error("First argument to *do-to-all* must be an instruction.");
      return;
    }

  for (int i = 0; i < (a.length - 2); i++)
    {
      if (arg_registries[i]->type != REGISTRY)
        {
          do_error("Arguments to *do-to-all* must be registries.");
          free(arg_registries);
          return;
        }
    }

  registry** arg_reg = malloc(sizeof(registry*)*(a.length-2));
  for (int i = 0; i < (a.length - 2); i++)
    {
      arg_reg[i] = (registry*) arg_registries[i]->data;
    }
  registry* ret_reg = new_registry(reg, ARBEL_HASH_SIZE);
  data* d;
  arg a1;

  a1.length = 1 + 2*(a.length-2);

  a1.free_data = malloc(sizeof(int)*a1.length);
  for (int i = 0; i < a1.length; i++)
    a1.free_data[i] = 0;

  a1.arg_array = malloc(sizeof(data*)*a1.length);
  a1.arg_array[0] = arg1;
  for (int i = 1; i < a1.length; i++)
    a1.arg_array[i] = NULL;


  for  (int i = 0; i < arg_reg[0]->hash_size; i++)
    {
      content* c = arg_reg[0]->objects[i];
      if (c == NULL)
        continue;
      
      if (is_init_reg(c))
        continue;

      c = tail(c);
      while (c != NULL)
        {
          bool in_all = true;
          for (int j = 0; j < (a.length-2); j++)
            {
              d = get(arg_reg[j], c->key, 0);
              if (d==NULL)
                {
                  c = c->right;
                  in_all = false;
                  break;
                }
              else
                {
                  char* tname = malloc(sizeof(char)*
                                       (strlen("t")+
                                        floor(log10(j+1))
                                        + 1+1));
                  sprintf(tname, "t%d", j+1);
                  if (a1.arg_array[1+2*j] != NULL)
                    free_data(a1.arg_array[1+2*j]);
                  
                  assign_regstr(&a1.arg_array[1+2*j], tname,
                                hash_str(tname));
                  free(tname);
                  a1.arg_array[2+2*j] = d;
                }
            }

          if (!in_all)
            continue;

          compute(arg1, reg, a1);
          d = lookup(reg, arbel_hash_ans, 0);
          if (d == NULL)
            {
              do_error("Instruction in *do-to-all* did not set *ans* register.");
              break;
            }
          d = copy_data(d);
          set(ret_reg, d, c->name, 1);
          
          c = c->right;
        }

      if (d == NULL)
        break;
    }

  for (int j = 0; j < (a.length-2); j++)
    {
      if (a1.arg_array[2*j+1] == NULL)
        free_data(a1.arg_array[2*j+1]);

    }

  if (!is_error(-1))
    {
      d = malloc(sizeof(data));
      d->type = REGISTRY;
      d->data = ret_reg;
      ret_ans(reg, d);
    }
  
}


void
op_next (arg a, registry* reg)
{
  CHECK_ARGS(a, 1);
  #op=next@
  #num=1@
  #type=REGISTER@
  ##GETARG~$;

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
op_last (arg a, registry* reg)
{
  CHECK_ARGS(a, 2);
  #op=last@
  #num=1@
  #type=REGISTRY@
  ##GETARG~$;

  #num=2@
  #type=STRING@
  ##GETARG~$;

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
  if (i == 1)
    {
      do_error("No such register in given registry.");
      return;
    }
  name = vector_name((char*) arg2->data, i-1);

  d = malloc(sizeof(data));
  d->type = REGISTER;
  d->data = malloc(sizeof(regstr));
  ((regstr*) d->data)->name = name;
  ((regstr*) d->data)->key = hash_str(name);

  ret_ans(reg, d);
  
}

void
op_in (arg a, registry* reg)
{
  CHECK_ARGS(a,2);

  #op=in@
  #num=1@
  #type=REGISTRY@
  ##GETARG~$;

  #num=2@
  #type=INSTRUCTION@
  ##GETARG~$;

  ((registry*) arg1->data)->up = reg;

  execute_0(arg2, (registry*) arg1->data);
  
  data* ans = lookup((registry*) arg1->data, arbel_hash_ans, 0);
  if (ans != NULL)
    {
      ans = copy_data(ans);
      ret_ans(reg, ans);
    }
}

void
op_while (arg a, registry* reg)
{
  CHECK_ARGS(a, 2);

  #op=while@
  #num=1@
  #type=INSTRUCTION@
  ##GETARG~$;

  #num=2@
  ##GETARG~$;

  data* d;
  while (1)
    {
      execute_0(arg1, reg);
      
      if (is_error(-1))
        break;
          
      d = get(reg, arbel_hash_ans, 0);

      if (d == NULL)
        {
          do_error("Instruction did not set *ans* to a value.");
          break;
        }

      if (d->type != BOOLEAN)
        {
          do_error("First instruction should set *ans* to a Boolean.");
          break;
        }

      if (!(*(bool*) d->data))
        {
          break;
        }

      execute_0(arg2, reg);
      if (is_error(-1))
        break;

    }

}

void
op_repeat (arg a, registry* reg)
{
  CHECK_ARGS(a, 2);

  #op=repeat@
  #num=1@
  #type=INTEGER@
  ##GETARG~$;

  #num=2@
  #type=INSTRUCTION@
  ##GETARG~$;

  for (int i = 0; i < *((int*) arg1->data); i++)
    {
      execute_0(arg2, reg);
      if (is_error(-1)) break;
    }
}

void
op_to_register (arg a, registry* reg)
{
  CHECK_ARGS(a, 1);

  #op=to-register@
  #num=1@
  #type=`(STRING|INTEGER)'@
  ##GETARG~$;

  data* d;

  if (arg1->type == STRING)
    {
      assign_regstr(&d, (char*) arg1->data,
                    hash_str((char*) arg1->data));
    }
  else
    {
      char* name = argument_name(*((int*) arg1->data));
      unsigned long hash_name = hash_str(name);
      assign_regstr(&d, name, hash_name);
      free(name);
    }

  ret_ans(reg,d);

}


void
op_collapse (arg a, registry* reg)
{
  CHECK_ARGS(a, 2);
  #op=collapse@
  #num=1@
  #type=INSTRUCTION@
  ##GETARG~$;

  #num=2@
  #type=REGISTRY@
  ##GETARG~$;

  const char* prefix = "`#'";
  if (a.length >= 4)
    {
      #num=3@
      #type=STRING@
      ##GETARG~$;

      prefix = (char*) arg3->data;
    }

  data* d;
  char* second_name;
  char* first_name;
  int i = 2;
  registry* to_walk = (registry*) arg2->data;
  to_walk->up = reg;

  first_name = vector_name(prefix, 1);
  unsigned long first_hash = hash_str(first_name);
  second_name = vector_name(prefix, i);

  unsigned long second_hash = hash_str(second_name);
  registry* r = new_registry(to_walk, ARBEL_HASH_SIZE);

  arg a1;
  a1.length = 5;
  a1.free_data = malloc(sizeof(int)*a1.length);
  a1.arg_array = malloc(sizeof(data*)*a1.length);
  for (int j = 0; j < a1.length; j++)
    {
      if (j==1 || j == 3)
        {
          a1.free_data[j] = 1;
        }
      else
        {
          a1.free_data[j] = 0;
        }
    }

  a1.arg_array[0] = arg1;
  assign_regstr(&a1.arg_array[1], "t1", hash_str("t1"));
  assign_regstr(&a1.arg_array[3], "t2", hash_str("t2"));
  
  data* d1;
  data* d2;

  int is_first = 1;
  
  while ((d = lookup(to_walk, second_hash, 0)) != NULL)
    {

      if (is_first)
        d1 = lookup(to_walk, first_hash, 0);
      
      d2 = lookup(to_walk, second_hash, 0);

      a1.arg_array[2] = d1;
      a1.arg_array[4] = d2;
        
      compute(arg1, r, a1);
      d1 = lookup(r, arbel_hash_ans, 0);

      if (d1 == NULL)
        {
          do_error("Instruction did not set /ans register.");
          break;
        }
      
      if (is_first)
        {
          free(first_name);
          first_hash = arbel_hash_ans;
          is_first = 0;
        }
      i++;
      free(second_name);
      second_name = vector_name(prefix, i);
      second_hash = hash_str(second_name);
    }
  d = lookup(r, arbel_hash_ans, 0);
  if (d != NULL)
    {
      ret_ans(reg, copy_data(d));
    }

  del(r, arbel_hash_0, 0);

  free_registry(r);
  free(second_name);
  free_arg(&a1);


}

void
op_string_eq (arg a, registry* reg)
{
  CHECK_ARGS(a, 2);

  #op=string-eq@
  #num=1@
  #type=STRING@
  ##GETARG~$;

  #num=2@
  #type=STRING@
  ##GETARG~$;

  char* str1 = arg1->data;
  char* str2 = arg2->data;
  data* d;
  if (strcmp(str1,str2) == 0)
    {
      assign_boolean(&d, true);
    }
  else
    {
      assign_boolean(&d, false);
    }

  ret_ans(reg, d);
}

void
op_string_lt (arg a, registry* reg)
{
  CHECK_ARGS(a, 2);

  #op=string-lt@
  #num=1@
  #type=STRING@
  ##GETARG~$;

  #num=2@
  #type=STRING@
  ##GETARG~$;

  char* str1 = arg1->data;
  char* str2 = arg2->data;
  data* d;
  if (strcmp(str1,str2) < 0)
    {
      assign_boolean(&d, true);
    }
  else
    {
      assign_boolean(&d, false);
    }

  ret_ans(reg, d);
}

void
op_string_gt (arg a, registry* reg)
{
  CHECK_ARGS(a, 2);

  #op=string-gt@
  #num=1@
  #type=STRING@
  ##GETARG~$;

  #num=2@
  #type=STRING@
  ##GETARG~$;

  char* str1 = arg1->data;
  char* str2 = arg2->data;
  data* d;
  if (strcmp(str1,str2) > 0)
    {
      assign_boolean(&d, true);
    }
  else
    {
      assign_boolean(&d, false);
    }

  ret_ans(reg, d);
}


void
op_register_eq (arg a, registry* reg)
{
  CHECK_ARGS(a, 2);

  #op=register-eq@
  #num=1@
  #type=REGISTER@
  ##GETARG~$;

  #num=2@
  #type=REGISTER@
  ##GETARG~$;
      
  data* d;

  if (((regstr*) arg1->data)->key == ((regstr*) arg2->data)->key)
    assign_boolean(&d,true);
  else
    assign_boolean(&d,false);

  ret_ans(reg,d);
}



void
op_go_in (arg a, registry* reg)
{
  CHECK_ARGS(a, 1);

  #op=go-in@
  #num=1@
  #type=REGISTRY@
  ##GETARG~$;

  ((registry*) arg1->data)->up = reg;

  current_parse_registry = (registry*) arg1->data;
  
}

void
op_go_out (arg a, registry* reg)
{
  if (current_parse_registry->up == NULL)
    {
      do_error("Already at top level registry.");
      return;
    }

  current_parse_registry = current_parse_registry->up;
}

void
op_save (arg a, registry* reg)
{
  CHECK_ARGS(a, 1);

  #op=save@
  #num=1@
  #type=STRING@
  ##GETARG~$;

  char* fname = (char*) arg1->data;
  FILE* f = fopen(fname, "wb");
  save_registry(f, reg);
  data_type end = NOTHING;
  fwrite(&end, sizeof(data_type), 1, f);
  fclose(f);

}

void
op_load (arg a, registry* reg)
{
  CHECK_ARGS(a, 1);
  #op=load@
  #num=1@
  #type=STRING@
  ##GETARG~$;

  char* fname = (char*) arg1->data;
  FILE* f = fopen(fname, "rb");
  read_registry(f, reg);
  fclose(f);

}

void
op_to_string (arg a, registry* reg)
{
  CHECK_ARGS(a,1);
  #op=to-string@
  #num=1@
  #type=`(INTEGER | REAL | REGISTER)'@
  ##GETARG~$;

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
  else if (arg1->type == REAL)
    {
      data* arg2 = NULL;
      if (a.length >= 3)
        {
          arg2 = resolve(a.arg_array[2], reg);
        }
      
      int prec = 6;
      if (arg2 != NULL && arg2->type != INTEGER)
        {
          do_error("The second argument to *to-string* should be an integer.");
          return;
        }
      if (arg2 != NULL)
        {
          prec = *((int*) arg2->data);
          if (prec < 0)
            {
              do_error("The second argument to *to-string* must be non-negative.");
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
  else
    {
      char* regstr_name = ((regstr*) arg1->data)->name;
      result = malloc(sizeof(char)*(strlen(regstr_name)+1));
      strcpy(result, regstr_name);
    }

  data* d;
  assign_str(&d, result, 0);
  ret_ans(reg, d);
  
}

void
op_to_number (arg a, registry* reg)
{
  CHECK_ARGS(a, 1);

  #op=to-number@
  #num=1@
  #type=`(STRING|REGISTER)'@
  ##GETARG~$;

  if (arg1->type == STRING)
    {
      char* value = (char*) arg1->data;
      data* d;
      if (is_integer(value))
        {
          int result = atoi(value);
          assign_int(&d, result);
          ret_ans(reg, d);
        }
      else if (is_real(value) && (strcmp(value, ".") != 0))
        {
          double result = atof(value);
          assign_real(&d, result);
          ret_ans(reg, d);
        }
      else
        {
          do_error("String not a number.");
          return;
        }
    }
  else
    {
      char* name = ((regstr*) arg1->data)->name;
      int i = strlen(name);
      for (i = (strlen(name)-1); i >= 0; i--)
        {
          if (!isdigit(name[i]))
            break;
        }

      if (i >= (strlen(name)-1))
        {
          do_error("Register does not end in a number.");
          return;
        }

      name += i + 1;

      data* d;
      assign_int(&d, atoi(name));
      ret_ans(reg,d);
    }
  
}

void
op_to_real (arg a, registry* reg)
{
  CHECK_ARGS(a, 1);
  #op=to-real@
  #num=1@
  #type=`(INTEGER|REAL)'@
  ##GETARG~$;

  data* d;
  if (arg1->type == INTEGER)
    assign_real(&d, *((int*) arg1->data));
  else
    d = copy_data(arg1);
  
  ret_ans(reg,d);
  
}

void
op_output_code (arg a, registry* reg)
{
  CHECK_ARGS(a, 1);

  #op=output-code@
  #num=1@
  #type=STRING@
  ##GETARG~$;

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
op_clear_code (arg a, registry* reg)
{
  if (source_code != NULL)
    free(source_code);

  source_code = NULL;
}

void
op_error (arg a, registry* reg)
{
  CHECK_ARGS(a, 1);
  #op=error@
  #num=1@
  #type=STRING@
  ##GETARG~$;

  do_error((char*) arg1->data);

  if (a.length >= 3)
    {
      #num=2@
      #type=INTEGER@
      #requireans=false@
      ##GETARG~$;
      #requireans=true@
      if (arg2 != NULL)
        {
          is_error(*((int*) arg2->data));
        }
    }
}

void
op_is_type (arg a, registry* reg, const data_type type)
{
  CHECK_ARGS(a, 1);
  #op=is-Type@
  #num=1@
  #checktype=false@
  ##GETARG~$;
  #checktype=true@
  data* d;
  if (arg1->type == type || ((type==INSTRUCTION) &&
                             (arg1->type==OPERATION)))
    {
      assign_boolean(&d, true);
    }
  else
    {
      assign_boolean(&d, false);
    }

  ret_ans(reg,d);
}

void
op_is_integer (arg a, registry* reg)
{
  op_is_type(a, reg, INTEGER);
}

void
op_is_real (arg a, registry* reg)
{
  op_is_type(a, reg, REAL);
}

void
op_is_string (arg a, registry* reg)
{
  op_is_type(a, reg, STRING);
}

void
op_is_register (arg a, registry* reg)
{
  op_is_type(a, reg, REGISTER);
}

void
op_is_registry (arg a, registry* reg)
{
  op_is_type(a, reg, REGISTRY);
}

void
op_is_instruction (arg a, registry* reg)
{
  op_is_type(a, reg, INSTRUCTION);
}

void
op_is_file (arg a, registry* reg)
{
  op_is_type(a, reg, ARBEL_FILE);
}

void
op_is_nothing (arg a, registry* reg)
{
  op_is_type(a, reg, NOTHING);
}

void
op_is_boolean (arg a, registry* reg)
{
  op_is_type(a, reg, BOOLEAN);
}


void
op_open_text_file (arg a, registry* reg)
{
  CHECK_ARGS(a, 1);
  #op=open-text-file@
  #num=1@
  #type=STRING@
  ##GETARG~$;
  
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
op_read (arg a, registry* reg)
{
  CHECK_ARGS(a, 1);
  #op=read@
  #num=1@
  #type=ARBEL_FILE@
  ##GETARG~$;

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
op_close (arg a, registry* reg)
{
  CHECK_ARGS(a, 1);

  #op=close@
  #num=1@
  #type=REGISTER@
  ##GETARG~$;

  data* f = lookup(reg, ((regstr*) arg1->data)->key, 0);

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

  del(reg, ((regstr*) arg1->data)->key, 1);
  
}

void
op_or (arg a, registry* reg)
{
  CHECK_ARGS(a, 2);

  #op=or@
  #num=1@
  #type=BOOLEAN@
  ##GETARG~$;

  #num=2@
  ##GETARG~$;

  data* d;
  if (*((bool*) arg1->data) || *((bool*) arg2->data))
    {
      assign_boolean(&d, true);
    }
  else
    {
      assign_boolean(&d, false);
    }

  ret_ans(reg,d);
}

void
op_and (arg a, registry* reg)
{
  CHECK_ARGS(a, 2);

  #op=and@
  #num=1@
  #type=BOOLEAN@
  ##GETARG~$;

  #num=2@
  ##GETARG~$;

  data* d;
  if (*((bool*) arg1->data) && *((bool*) arg2->data))
    {
      assign_boolean(&d, true);
    }
  else
    {
      assign_boolean(&d, false);
    }

  ret_ans(reg,d);
}

void
op_not (arg a, registry* reg)
{
  CHECK_ARGS(a, 1);

  #op=not@
  #num=1@
  #type=BOOLEAN@
  ##GETARG~$;

  data* d;
  if (*((bool*) arg1->data))
    {
      assign_boolean(&d, false);
    }
  else
    {
      assign_boolean(&d, true);
    }

  ret_ans(reg,d);
}

void
op_read_line (arg a, registry* reg)
{
  CHECK_ARGS(a, 1);

  #op=read-line@
  #num=1@
  #type=ARBEL_FILE@
  ##GETARG~$;

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
op_write (arg a, registry* reg)
{
  CHECK_ARGS(a, 2);

  #op=write@
  #num=1@
  #type=STRING@
  ##GETARG~$;

  #num=2@
  #type=ARBEL_FILE@
  ##GETARG~$;

  fwrite((char*) arg1->data, sizeof(char), strlen((char*) arg1->data), (FILE*) arg2->data);
  data* d;
  assign_nothing(&d);
  ret_ans(reg,d);
}

void
op_input (arg a, registry* reg)
{
  CHECK_ARGS(a, 1);

  #op=input@
  #num=1@
  #type=REGISTER@
  ##GETARG~$;

  char* input = readline("");
  data* d;
  assign_str(&d, input, 0);
  
  set(reg, d, ((regstr*) arg1->data)->name, 1);
  
}

void
op_shell (arg a, registry* reg)
{
  CHECK_ARGS(a, 1);

  #op=shell@
  #num=1@
  #type=STRING@
  ##GETARG~$;

  FILE* f = popen((char*) arg1->data, "r");
  
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
op_link (arg a, registry* reg)
{
  CHECK_ARGS(a, 3);

  #op=link@
  #num=1@
  #type=STRING@
  ##GETARG~$;

  #num=2@
  ##GETARG~$;

  #num=3@
  ##GETARG~$;

  void* lib = dlopen((char*) arg1->data, RTLD_LAZY);

  if (lib == NULL)
    {
      printf("`%'s\n", dlerror());
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
  set(reg, d, (char*) arg3->data, 1);

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
op_match (arg a, registry* reg)
{
  CHECK_ARGS(a, 2);

  #op=match@
  #num=1@
  #type=STRING@
  ##GETARG~$;

  #num=2@
  ##GETARG~$;
  

  int max_matches = 0;
  if (a.length >= 4)
    {
      #num=3@
      #type=INTEGER@
      ##GETARG~$;
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
                  set((registry*) d_reg->data, d_str, name, 1);
                  free(name);
                }
            }
          n_matches++;
          name = argument_name(n_matches);
          set((registry*) d->data, d_reg, name, 1);
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
op_replace (arg a, registry* reg)
{
  CHECK_ARGS(a, 3);

  #op=replace@
  #type=STRING@
  #num=1@
  ##GETARG~$;

  #num=2@
  ##GETARG~$;

  #num=3@
  ##GETARG~$;

  int max_replace = 0;
  if (a.length >= 5)
    {
      #num=4@
      #type=INTEGER@
      ##GETARG~$;
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
op_log (arg a, registry* reg)
{
  CHECK_ARGS(a, 1);
  #op=log@
  #num=1@
  #type=`(INTEGER|REAL)'@
  ##GETARG~$;
  
  data* d;
  if (arg1->type == INTEGER)
    {
      assign_real(&d, log((double) (*((int*) arg1->data))));
    }
  else
    {
      assign_real(&d, log(*((double*) arg1->data)));
    }

  ret_ans(reg,d);
}


void
op_exp (arg a, registry* reg)
{
  CHECK_ARGS(a, 1);
  #op=exp@
  #num=1@
  #type=`(INTEGER|REAL)'@
  ##GETARG~$;

  data* d;
  if (arg1->type == INTEGER)
    {
      assign_real(&d, exp((double) (*((int*) arg1->data))));
    }
  else
    {
      assign_real(&d, exp(*((double*) arg1->data)));
    }

  ret_ans(reg,d);
}

void
op_power (arg a, registry* reg)
{
  CHECK_ARGS(a, 2);

  #op=power@
  #num=1@
  #type=`(INTEGER|REAL)'@
  ##GETARG~$;

  #num=2@
  ##GETARG~$;

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

  assign_real(&d, pow(base,power));

  ret_ans(reg,d);
}

void
op_change_dir (arg a, registry* reg)
{
  CHECK_ARGS(a, 1);
  #op=change-dir@
  #num=1@
  #type=STRING@
  ##GETARG~$;

  int error = chdir((char*) arg1->data);
  if (error)
    {
      do_error("Could not change directory.");
      return;
    }

}


void
op_import (arg a, registry* reg)
{
  CHECK_ARGS(a, 1);

  #op=import@
  #num=1@
  #type=REGISTRY@
  ##GETARG~$;

  registry* r1 = (registry*) arg1->data;

  for (int i = 0; i < r1->hash_size; i++)
    {
      content* c = r1->objects[i];

      if (c == NULL)
        continue;

      if (is_init_reg(c))
        continue;
      
      c = tail(c);
  
      data* d = NULL;
      while (c != NULL)
        {
          d = c->value;
          if (d != NULL)
            {
              d = copy_data(d);
              set(reg, d, (char*) c->name, 1);
            }
          c = c->right;
        }
    }
}


void
op_current_dir (arg a, registry* reg)
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
op_substring (arg a, registry* reg)
{
  CHECK_ARGS(a, 3);
  #op=substring@
  #num=1@
  #type=STRING@
  ##GETARG~$;

  #num=2@
  #type=INTEGER@
  ##GETARG~$;

  #num=3@
  ##GETARG~$;

  char* str = (char*) arg1->data;
  int start = *((int*) arg2->data);
  int end = *((int*) arg3->data);

  if (start <= 0)
    {
      start += strlen(str);
    }

  if (end <= 0)
    {
      end += strlen(str);
    }

  if ((start > strlen(str)) || (start <= 0))
    {
      do_error("Index out of range.");
      return;
    }


  if ((end > strlen(str)) || (end <= 0))
    {
      do_error("Index out of range.");
      return;
    }

  if (start > end)
    {
      do_error("The starting position of the substring is greater than the ending position.");
      return;
    }

  char* result = malloc(sizeof(char)*(end-start + 1 + 1));
  int i;

  for (i=(start-1); i < end; i++)
    {
      result[i+1-start] = str[i];
    }

  result[end-start+1] = '\0';

  data* d;
  assign_str(&d, result, 0);
  ret_ans(reg, d);
  
}

void
op_up (arg a, registry* reg)
{
  CHECK_ARGS(a,1);
  if (reg->up == NULL)
    {
      do_error("Cannot use *up* instruction at top-level registry.");
      return;
    }

  arg a1 = gen_arg(a.length-1,0);
  for (int i=1; i < a.length; i++)
    {
      a1.arg_array[i-1] = resolve(a.arg_array[i], reg);
    }

  compute(a1.arg_array[0], reg->up, a1);

  data* d = lookup(reg->up, arbel_hash_ans, 0);
  if (d != NULL)
    {
      d = copy_data(d);
      ret_ans(reg, d);
    }

  free_arg(&a1);


}

void
op_of (arg a, registry* reg)
{
  CHECK_ARGS(a, 2);

  #op=of@
  #num=1@
  #type=STRING@
  ##GETARG~$;

  #num=2@
  #type=REGISTRY@
  ##GETARG~$;

  data* d;
  assign_str(&d, (char*) arg1->data, 1);
  set((registry*) arg2->data, d, "--of", 1);
}

void
op_is_of (arg a, registry* reg)
{
  CHECK_ARGS(a, 2);
  #op=is-of@
  #num=1@
  #type=STRING@
  ##GETARG~$;

  #num=2@
  #type=REGISTRY@
  ##GETARG~$;

  data* d = lookup((registry*) arg2->data, arbel_hash_class, 0);

  if (d == NULL || d->type != STRING)
    {
      do_error("*class* register not found.");
      return;
    }

  if (strcmp((char*) d->data, (char*) arg1->data)==0)
    {
      assign_boolean(&d, true);
    }
  else
    {
      assign_boolean(&d, false);
    }

  ret_ans(reg, d);
  
}

void
op_dispatch (arg a, registry* reg)
{
  CHECK_ARGS(a, 2);

  #op=dispatch@
  #type=STRING@
  #num=1@
  ##GETARG~$;

  #num=2@
  #checktype=false@
  ##GETARG~$;
  #checktype=true@

  const char* class;
  data* d = NULL;
  switch (arg2->type)
    {
    case INTEGER:
      class = "Integer";
      break;
    case REAL:
      class = "Real";
      break;
    case STRING:
      class = "String";
      break;
    case REGISTER:
      class = "Register";
      break;
    case REGISTRY:
      d = lookup((registry*) arg2->data, arbel_hash_class, 0);
      if (d != NULL)
        {
          class = (char*) d->data;
        }
      else
        {
          class = "Registry";
        }
      break;
    case INSTRUCTION:
      class = "Instruction";
      break;
    case OPERATION:
      class = "Instruction";
      break;
    case ARBEL_FILE:
      class = "File";
      break;
    case NOTHING:
      class = "Nothing";
      break;
    default:
      class = "";
      break;
    }

  char* grab = malloc(sizeof(char)*(strlen((char*) arg1->data)+
                                    strlen(class)+strlen("+")+1));
  strcpy(grab, (char*) arg1->data);
  strcat(grab, "+");
  strcat(grab, class);

  unsigned long hash_grab = hash_str(grab);

  d = lookup(reg, hash_grab, 1);
  if (d != NULL)
    {
      d = copy_data(d);
      ret_ans(reg,d);
    }

  free(grab);
}

void
op_code (arg a, registry* reg)
{
  CHECK_ARGS(a, 1);

  #op=code@
  #num=1@
  #type=INSTRUCTION@
  ##GETARG~$;

  data* d;
  assign_str(&d, ((instruction*) arg1->data)->code, 1);

  ret_ans(reg, d);
      
}

void
op_is_error (arg a, registry* reg)
{
  CHECK_ARGS(a, 1);

  #op=is-error@
  #num=1@
  #type=INSTRUCTION@
  ##GETARG~$;

  execute_0(arg1, reg);
  data* d;
  assign_int(&d, is_error(-1));

  ret_ans(reg, d);
  is_error(0);

}


void
op_call (arg a, registry* reg)
{
  _op_call(a,reg,1);
}


void
add_basic_ops (registry* reg)
{
  data* d;

  assign_op(&d, op_set);
  set(reg, d, "set",1);
  
  assign_op(&d, op_add);
  set(reg,d,"add",1);

  assign_op(&d, op_mul);
  set(reg,d,"mul",1);

  assign_op(&d, op_sub);
  set(reg,d,"sub",1);

  assign_op(&d, op_div);
  set(reg,d,"div",1);

  assign_op(&d, op_if);
  set(reg,d,"if",1);

  assign_op(&d, op_registry);
  set(reg,d,"registry",1);

  assign_op(&d, op_get);
  set(reg,d,"get",1);

  assign_op(&d, op_move);
  set(reg,d,"move",1);

  assign_op(&d, op_delete);
  set(reg,d,"delete",1);

  assign_op(&d, op_exit);
  set(reg,d,"exit",1);

  assign_op(&d, op_answer);
  set(reg,d,"answer",1);

  assign_op(&d, op_sit);
  set(reg,d,"sit",1);

  assign_op(&d, op_exist);
  set(reg,d,"exist",1);

  assign_op(&d, op_gt);
  set(reg,d,"gt",1);

  assign_op(&d, op_lt);
  set(reg,d,"lt",1);

  assign_op(&d, op_eq);
  set(reg,d,"eq",1);

  assign_op(&d, op_lteq);
  set(reg,d,"lteq",1);

  assign_op(&d, op_gteq);
  set(reg,d,"gteq",1);
  
  assign_op(&d, op_print);
  set(reg,d,"print",1);

  assign_op(&d, op_character);
  set(reg,d,"character",1);
  
  assign_op(&d, op_count_characters);
  set(reg,d,"count-characters",1);

  assign_op(&d, op_concat);
  set(reg,d,"concat",1);

  assign_op(&d, op_source);
  set(reg,d,"source",1);

  assign_op(&d, op_do_to_all);
  set(reg,d,"do-to-all",1);

  assign_op(&d, op_next);
  set(reg,d,"next",1);

  assign_op(&d, op_last);
  set(reg,d,"last",1);

  assign_op(&d, op_in);
  set(reg,d,"in",1);

  assign_op(&d, op_while);
  set(reg,d,"while",1);
  
  assign_op(&d, op_list);
  set(reg,d,"list",1);

  assign_op(&d, op_to_register);
  set(reg,d,"to-register",1);

  assign_op(&d, op_collapse);
  set(reg,d,"collapse",1);

  assign_op(&d, op_string_eq);
  set(reg,d,"string-eq",1);

  assign_op(&d, op_string_gt);
  set(reg,d,"string-gt",1);
  
  assign_op(&d, op_string_lt);
  set(reg,d,"string-lt",1);

  assign_op(&d, op_register_eq);
  set(reg,d,"register-eq",1);

  assign_op(&d, op_go_in);
  set(reg,d,"go-in",1);

  assign_op(&d, op_go_out);
  set(reg,d,"go-out",1);

  assign_op(&d, op_save);
  set(reg,d,"save",1);

  assign_op(&d, op_load);
  set(reg,d,"load",1);

  assign_op(&d, op_to_string);
  set(reg,d,"to-string",1);
  
  assign_op(&d, op_to_number);
  set(reg,d,"to-number",1);

  assign_op(&d, op_output_code);
  set(reg,d,"output-code",1);

  assign_op(&d, op_clear_code);
  set(reg,d,"clear-code",1);

  assign_op(&d, op_error);
  set(reg,d,"error",1);

  assign_op(&d, op_is_integer);
  set(reg,d,"is-integer",1);

  assign_op(&d, op_is_real);
  set(reg,d,"is-real",1);

  assign_op(&d, op_is_string);
  set(reg,d,"is-string",1);
  
  assign_op(&d, op_is_register);
  set(reg,d,"is-register",1);

  assign_op(&d, op_is_registry);
  set(reg,d,"is-registry",1);

  assign_op(&d, op_is_instruction);
  set(reg,d,"is-instruction",1);

  assign_op(&d, op_is_file);
  set(reg,d,"is-file",1);

  assign_op(&d, op_is_nothing);
  set(reg,d,"is-nothing",1);

  assign_op(&d, op_is_boolean);
  set(reg,d,"is-boolean",1);
  
  assign_op(&d, op_open_text_file);
  set(reg,d,"open-text-file",1);

  assign_op(&d, op_read);
  set(reg,d,"read",1);

  assign_op(&d, op_close);
  set(reg,d,"close",1);

  assign_op(&d, op_and);
  set(reg,d,"and",1);

  assign_op(&d, op_or);
  set(reg,d,"or",1);

  assign_op(&d, op_not);
  set(reg,d,"not",1);

  assign_op(&d, op_read_line);
  set(reg,d,"read-line",1);
  
  assign_op(&d, op_write);
  set(reg,d,"write",1);
  
  assign_op(&d, op_input);
  set(reg,d,"input",1);

  assign_op(&d, op_shell);
  set(reg,d,"shell",1);

  assign_op(&d, op_link);
  set(reg,d,"link",1);

  assign_op(&d, op_match);
  set(reg,d,"match",1);

  assign_op(&d, op_replace);
  set(reg,d,"replace",1);

  assign_op(&d, op_log);
  set(reg,d,"log",1);

  assign_op(&d, op_exp);
  set(reg,d,"exp",1);

  assign_op(&d, op_power);
  set(reg,d,"power",1);

  assign_op(&d, op_change_dir);
  set(reg,d,"change-dir",1);

  assign_op(&d, op_current_dir);
  set(reg,d,"current-dir",1);

  assign_op(&d, op_import);
  set(reg,d,"import",1);
  
  assign_op(&d, op_repeat);
  set(reg,d,"repeat",1);

  assign_op(&d, op_substring);
  set(reg,d,"substring",1);

  assign_op(&d, op_up);
  set(reg,d,"up",1);
  
  assign_op(&d, op_of);
  set(reg,d,"of",1);

  assign_op(&d, op_is_of);
  set(reg,d,"is-of",1);

  assign_op(&d, op_dispatch);
  set(reg,d,"dispatch",1);

  assign_op(&d, op_to_real);
  set(reg,d,"to-real",1);

  assign_op(&d, op_call);
  set(reg,d,"call",1);

  assign_op(&d, op_code);
  set(reg,d,"code",1);

  assign_op(&d, op_is_error);
  set(reg,d,"is-error",1);

  assign_op(&d, op_range);
  set(reg,d,"range",1);

}
  