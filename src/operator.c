/* 
   ARBEL is a Register BASED ENVIRONMENT AND LANGUAGE
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

#include <errno.h>



;





void
op_range (arg a, registry* reg)
{
  

  
  
  check_length(&a, 2+1, "range");
if (is_error(-1)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<range> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Integer)))
  {
    do_error("Argument 1 of <range> should be of type Integer.");
    return ;
  }

;

  
  
data* arg2 = resolve(a.arg_array[2], reg);

if (true)
  {
    if (arg2 == NULL)
      {
        do_error("<range> requires at least 2 arguments.");
        return ;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & Integer)))
  {
    do_error("Argument 2 of <range> should be of type Integer.");
    return ;
  }

;

  mpz_t by;
  mpz_init(by);
  mpz_set_ui(by, 1);
  
  if (a.length >= 4)
    {
      
      
data* arg3 = resolve(a.arg_array[3], reg);

if (true)
  {
    if (arg3 == NULL)
      {
        do_error("<range> requires at least 3 arguments.");
        return ;
      }
  }
if (arg3 != NULL && true && (!(arg3->type & Integer)))
  {
    do_error("Argument 3 of <range> should be of type Integer.");
    return ;
  }

;
      mpz_set(by, *((mpz_t*) arg3->data));
    }

  mpz_t* lb = (mpz_t*) arg1->data;
  mpz_t* ub = (mpz_t*) arg2->data;

  mpz_t elements;
  mpz_init(elements);
  mpz_sub(elements, *ub, *lb);
  mpz_cdiv_q(elements, elements, by);
  
  registry* r_new = new_registry
    (reg,new_hash_size(mpz_get_ui(elements)));

  mpz_t cur;
  mpz_init_set(cur, *lb);
  
  int i = 1;
  char* r = NULL;
  data* d_tmp;
  while (mpz_cmp(cur,*ub) <= 0)
    {
      r = argument_name(i);
      assign_int(&d_tmp, cur);
      set(r_new, d_tmp, r, 0);
      free(r);
      i++;
      mpz_add(cur, cur, by);
    }

  d_tmp = new_data();
  d_tmp->type = Registry;
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

  d = new_data();
  d->type = Registry;
  d->data = r_new;
  ret_ans(reg, d);

}

void
op_registry (arg a, registry* reg)
{
  if (a.length != 1)
    {
      
      
      check_length(&a, 2+1, "registry");
if (is_error(-1)) return ;;
    }
  
  registry* r_new = new_registry(reg, new_hash_size(a.length / 2 + 1));
  data* d = NULL;
  data* d_data = NULL;
  data* d_new;

  for (int i = 1; i < a.length; i = i + 2)
    {
      d = a.arg_array[i];
      
      if (d->type != Register)
        {
          do_error("Expected a register");
          free_registry(r_new);
          return;
        }

      d_data = resolve(a.arg_array[i+1], reg);
      d_new = copy_data(d_data);
      set(r_new, d_new, ((regstr*) d->data)->name, 0);
    }


  d = new_data();
  d->type = Registry;
  d->data = r_new;
  ret_ans(reg, d);
}

void
op_arithmetic (arg a, registry* reg, const int code)
{
  data* d;
  data_type result_type = Integer;
  mpz_t int_value;
  mpz_init_set_si(int_value, 0);
  double dbl_value = 0.0;

  
  for (int i = 1; i < a.length; i++)
    {
      
      
      
data* argi = resolve(a.arg_array[i], reg);

if (true)
  {
    if (argi == NULL)
      {
        do_error("<arithmetic> requires at least i arguments.");
        return ;
      }
  }
if (argi != NULL && true && (!(argi->type & (Real|Integer))))
  {
    do_error("Argument i of <arithmetic> should be of type (Real|Integer).");
    return ;
  }

;

      if (argi->type == Real && result_type == Integer)
        {
          dbl_value = mpz_get_d(int_value);
          result_type = Real;
        }

      if (result_type == Integer)
        {
          if (i == 1)
            mpz_set(int_value, *((mpz_t*) argi->data));
          else
            switch (code)
              {
              case 1:
                mpz_add(int_value, int_value, *((mpz_t*) argi->data));
                break;
              case 2:
                mpz_mul(int_value, int_value, *((mpz_t*) argi->data));
                break;
              case 3:
                mpz_sub(int_value, int_value, *((mpz_t*) argi->data));
                break;
              case 4:
                mpz_fdiv_q(int_value,
                           int_value, *((mpz_t*) argi->data));
                break;
              }
        }
      else
        {
          double val;
          if (argi->type == Integer)
            val = mpz_get_d(*((mpz_t*) argi->data));
          else
            val = *((double*) argi->data);
	    
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

  if (result_type == Integer)
    assign_int(&d, int_value);
  else
    assign_real(&d, dbl_value);
	      
  ret_ans(reg, d);
}  

void
op_add (arg a, registry* reg)
{
  
  
  check_length(&a, 2+1, "add");
if (is_error(-1)) return ;;

  op_arithmetic(a, reg, 1);
}

void
op_mul (arg a, registry* reg)
{
  
  
  check_length(&a, 2+1, "mul");
if (is_error(-1)) return ;;

  op_arithmetic(a, reg, 2);
}

void
op_sub (arg a, registry* reg)
{
  
  
  check_length(&a, 2+1, "sub");
if (is_error(-1)) return ;;

  op_arithmetic(a, reg, 3);
}

void
op_div (arg a, registry* reg)
{
  
  
  check_length(&a, 2+1, "div");
if (is_error(-1)) return ;;

  op_arithmetic(a, reg, 4);
}

void
op_set (arg a, registry* reg)
{
  

  
  check_length(&a, 2+1, "set");
if (is_error(-1)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<set> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Register)))
  {
    do_error("Argument 1 of <set> should be of type Register.");
    return ;
  }

;

  
  
  
data* arg2 = resolve(a.arg_array[2], reg);

if (true)
  {
    if (arg2 == NULL)
      {
        do_error("<set> requires at least 2 arguments.");
        return ;
      }
  }
if (arg2 != NULL && false && (!(arg2->type & Register)))
  {
    do_error("Argument 2 of <set> should be of type Register.");
    return ;
  }

;
  

  registry* to_set;
  if (a.length >= 4)
    {
      
      
      
data* arg3 = resolve(a.arg_array[3], reg);

if (true)
  {
    if (arg3 == NULL)
      {
        do_error("<set> requires at least 3 arguments.");
        return ;
      }
  }
if (arg3 != NULL && true && (!(arg3->type & Registry)))
  {
    do_error("Argument 3 of <set> should be of type Registry.");
    return ;
  }

;
      
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
  
  
  check_length(&a, 1+1, "get");
if (is_error(-1)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<get> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Register)))
  {
    do_error("Argument 1 of <get> should be of type Register.");
    return ;
  }

;
  
  registry* to_look;
  if (a.length == 2)
    {
      to_look = reg;
    }
  else
    {
      
      
      
data* arg2 = resolve(a.arg_array[2], reg);

if (true)
  {
    if (arg2 == NULL)
      {
        do_error("<get> requires at least 2 arguments.");
        return ;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & Registry)))
  {
    do_error("Argument 2 of <get> should be of type Registry.");
    return ;
  }

;

      to_look = (registry*) arg2->data;
    }

  data* ans = lookup(to_look, ((regstr*) arg1->data)->key,
		     a.length==2);
  if (ans != NULL)
    {
      ans = copy_data(ans);
      ret_ans(reg, ans);
    }
}

void
op_if (arg a, registry* reg)
{
  
  
  check_length(&a, 1+1, "if");
if (is_error(-1)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<if> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Boolean)))
  {
    do_error("Argument 1 of <if> should be of type Boolean.");
    return ;
  }

;

  
  if (*((bool*) arg1->data))
    {
      if (a.length >= 3)
        {
          
          
          
data* arg2 = resolve(a.arg_array[2], reg);

if (false)
  {
    if (arg2 == NULL)
      {
        do_error("<if> requires at least 2 arguments.");
        return ;
      }
  }
if (arg2 != NULL && false && (!(arg2->type & Boolean)))
  {
    do_error("Argument 2 of <if> should be of type Boolean.");
    return ;
  }

;
          

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
          
          
          
data* arg3 = resolve(a.arg_array[3], reg);

if (false)
  {
    if (arg3 == NULL)
      {
        do_error("<if> requires at least 3 arguments.");
        return ;
      }
  }
if (arg3 != NULL && false && (!(arg3->type & Boolean)))
  {
    do_error("Argument 3 of <if> should be of type Boolean.");
    return ;
  }

;
          

          if (arg3 != NULL)
            {
              data* d = copy_data(arg3);
              ret_ans(reg,d);
            }
        }
    }
  
}

void
op_sit (arg a, registry* reg)
{
  for (int i=1; i < a.length; i++)
    {
      (void) resolve(a.arg_array[i], reg);
    }
  return;
}


void
op_move (arg a, registry* reg)
{
  
  
  check_length(&a, 2+1, "move");
if (is_error(-1)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<move> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Register)))
  {
    do_error("Argument 1 of <move> should be of type Register.");
    return ;
  }

;

  
  
data* arg2 = resolve(a.arg_array[2], reg);

if (true)
  {
    if (arg2 == NULL)
      {
        do_error("<move> requires at least 2 arguments.");
        return ;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & Register)))
  {
    do_error("Argument 2 of <move> should be of type Register.");
    return ;
  }

;

  mov(reg, (regstr*) arg1->data,
      (regstr*) arg2->data);
  
}

void
op_delete (arg a, registry* reg)
{
  
  
  check_length(&a, 1+1, "delete");
if (is_error(-1)) return ;;
  
  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<delete> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Register)))
  {
    do_error("Argument 1 of <delete> should be of type Register.");
    return ;
  }

;
  
  del(reg, ((regstr*) arg1->data)->key, 1, false);
}

void
op_free (arg a, registry* reg)
{
  
  
  check_length(&a, 1+1, "free");
if (is_error(-1)) return ;;
  
  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<free> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Register)))
  {
    do_error("Argument 1 of <free> should be of type Register.");
    return ;
  }

;
  
  del(reg, ((regstr*) arg1->data)->key, 1, true);
}

void
op_exit (arg a, registry* reg)
{
  int err = 0;
  if (a.length >= 2)
    {
      
      
      
      
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<exit> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Integer)))
  {
    do_error("Argument 1 of <exit> should be of type Integer.");
    return ;
  }

;

      err = mpz_get_si(*((mpz_t*) arg1->data));
    }
  is_exit(err+1);
  is_error(1);
}

void
op_exist (arg a, registry* reg)
{
  
  
  check_length(&a, 1+1, "exist");
if (is_error(-1)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<exist> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Register)))
  {
    do_error("Argument 1 of <exist> should be of type Register.");
    return ;
  }

;

  registry* to_use = reg;
  if (a.length >= 3)
    {
      
      
      
data* arg2 = resolve(a.arg_array[2], reg);

if (true)
  {
    if (arg2 == NULL)
      {
        do_error("<exist> requires at least 2 arguments.");
        return ;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & Registry)))
  {
    do_error("Argument 2 of <exist> should be of type Registry.");
    return ;
  }

;

      to_use = (registry*) arg2->data;
    }

  data* obj = get(to_use, ((regstr*) arg1->data)->key, a.length < 3 ? 1 : 0);
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
  
  
  check_length(&a, 1+1, "answer");
if (is_error(-1)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<answer> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && false && (!(arg1->type & Registry)))
  {
    do_error("Argument 1 of <answer> should be of type Registry.");
    return ;
  }

;
  

  data* d = copy_data(arg1);
  ret_ans(reg, d);
}

int
op_comparison (arg a, registry* reg)
{
  
  
  
  check_length(&a, 2+1, "comparison");
if (is_error(-1)) return -2;;
  

  
  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<comparison> requires at least 1 arguments.");
        return -2;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & (Integer|Real))))
  {
    do_error("Argument 1 of <comparison> should be of type (Integer|Real).");
    return -2;
  }

;

  
  
data* arg2 = resolve(a.arg_array[2], reg);

if (true)
  {
    if (arg2 == NULL)
      {
        do_error("<comparison> requires at least 2 arguments.");
        return -2;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & (Integer|Real))))
  {
    do_error("Argument 2 of <comparison> should be of type (Integer|Real).");
    return -2;
  }

;

  

  if (arg1->type == Integer && arg2->type == Integer)
    {
      return mpz_cmp(*((mpz_t*) arg1->data), *((mpz_t*) arg2->data));
    }
  else
    {
      double dval1;
      double dval2;
      if (arg1->type == Integer)
        {
          dval1 = mpz_get_d(*((mpz_t*) arg1->data));
          dval2 = *((double*) arg2->data);
        }
      else if (arg2->type == Integer)
        {
          dval1 = *((double*) arg1->data);
          dval2 = mpz_get_d(*((mpz_t*) arg2->data));
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
op_gt_eq (arg a, registry* reg)
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
op_lt_eq (arg a, registry* reg)
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
  
  
  check_length(&a, 1+1, "print");
if (is_error(-1)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<print> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && false && (!(arg1->type & (Integer|Real))))
  {
    do_error("Argument 1 of <print> should be of type (Integer|Real).");
    return ;
  }

;
  
  print_settings settings = PRINT_NEWLINE;

  if (a.length >= 3)
    {
      
      
      
      
      
data* arg2 = resolve(a.arg_array[2], reg);

if (false)
  {
    if (arg2 == NULL)
      {
        do_error("<print> requires at least 2 arguments.");
        return ;
      }
  }
if (arg2 != NULL && false && (!(arg2->type & Boolean)))
  {
    do_error("Argument 2 of <print> should be of type Boolean.");
    return ;
  }

;
      
      


      if (arg2 != NULL && arg2->type == Boolean)
        {
          if (!*((bool*) arg2->data))
            settings = PRINT_PLAIN;
        }
    }
             
  print_data(arg1,settings);
}

void
op_string_length (arg a, registry* reg)
{
  
  
  check_length(&a, 1+1, "string-length");
if (is_error(-1)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<string-length> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & String)))
  {
    do_error("Argument 1 of <string-length> should be of type String.");
    return ;
  }

;

  size_t len = u8_mbsnlen((unsigned char*) arg1->data,
                          strlen((char*) arg1->data));

  mpz_t len_z;
  mpz_init_set_ui(len_z, len);
  data* d;
  assign_int(&d, len_z);

  ret_ans(reg, d);
  
}

void
op_string_append (arg a, registry* reg)
{
  
  
  check_length(&a, 2+1, "string-append");
if (is_error(-1)) return ;;

  size_t sz = 0;
  data* args[a.length-1];
  for (int i = 1; i < a.length; i++)
    {
      
      
      
data* argi = resolve(a.arg_array[i], reg);

if (true)
  {
    if (argi == NULL)
      {
        do_error("<string-append> requires at least i arguments.");
        return ;
      }
  }
if (argi != NULL && true && (!(argi->type & String)))
  {
    do_error("Argument i of <string-append> should be of type String.");
    return ;
  }

;
      args[i-1] = argi;
      sz += strlen((char*) argi->data);
    }
  char* result = malloc(sizeof(char)*(sz+1));

  strcpy(result, (char*) args[0]->data);
  for (int i=2; i < a.length; i++)
    {
      strcat(result, (char*) args[i-1]->data);
    }

  data* d = new_data();
  d->type = String;
  d->data = result;

  ret_ans(reg, d);

}

void
op_source (arg a, registry* reg)
{
  
  
  check_length(&a, 1+1, "source");
if (is_error(-1)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<source> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & String)))
  {
    do_error("Argument 1 of <source> should be of type String.");
    return ;
  }

;

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
  free_state(&state);
  fclose(f);

}

void
op_do_to_all (arg a, registry* reg)
{
  
  
  check_length(&a, 2+1, "do-to-all");
if (is_error(-1)) return ;;

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
  

  if (arg1->type != Operation)
    {
      do_error("First argument to <do-to-all> must be an operation.");
      return;
    }

  for (int i = 0; i < (a.length - 2); i++)
    {
      if (arg_registries[i]->type != Registry)
        {
          do_error("Arguments to <do-to-all> must be registries.");
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

  a1.length = 1 + a.length-2;

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
                  a1.arg_array[1+j] = d;
                }
            }

          if (!in_all)
            continue;

          compute(arg1, reg, a1);
          d = lookup(reg, arbel_hash_ans, 0);
          if (d == NULL)
            {
              do_error("Instruction in <do-to-all> did not set /ans register.");
              break;
            }
          d = copy_data(d);
          set(ret_reg, d, c->name, 1);
          
          c = c->right;
        }

      if (d == NULL)
        break;
    }

  if (!is_error(-1))
    {
      d = new_data();
      d->type = Registry;
      d->data = ret_reg;
      ret_ans(reg, d);
    }
  
}

data*
register_arithmetic (regstr* rg, int diff)
{
  char* cur_name = malloc(sizeof(char)*(strlen(rg->name)+1));
  strcpy(cur_name, rg->name);

  int i = strlen(cur_name)-1;
  int j;
  while (i >= 0 && isdigit((int) cur_name[i]))
    i--;

  if (i == (strlen(cur_name)-1))
    {
      do_error("Register does not end in integer.");
      return NULL;
    }
  
  i++;
  
  char* cur_num = malloc(sizeof(char)*(strlen(cur_name)-i + 1));
  for (j = i; j < strlen(cur_name); j++)
    {
      cur_num[j-i] = cur_name[j];
    }
  cur_num[strlen(cur_name)-i] = '\0';
  cur_name[i] = '\0';
  int num = atoi(cur_num)+diff;
  if (num < 1) num = 1;

  free(cur_num);

  char* new_name = malloc(sizeof(char)*(strlen(cur_name)+2));
  sprintf(new_name, "%s%d", cur_name, num);
  data* d = new_data();
  d->type = Register;
  d->data = malloc(sizeof(regstr));
  ((regstr*) d->data)->name = new_name;
  ((regstr*) d->data)->key = hash_str(new_name);
  free(cur_name);
  
  return d;
}
  

void
op_next (arg a, registry* reg)
{
  
  
  check_length(&a, 1+1, "next");
if (is_error(-1)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<next> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Register)))
  {
    do_error("Argument 1 of <next> should be of type Register.");
    return ;
  }

;

  data* d = register_arithmetic((regstr*) arg1->data, 1);
  if (d != NULL)
    ret_ans(reg,d);
}

void
op_previous (arg a, registry* reg)
{
  
  
  check_length(&a, 1+1, "previous");
if (is_error(-1)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<previous> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Register)))
  {
    do_error("Argument 1 of <previous> should be of type Register.");
    return ;
  }

;

  data* d = register_arithmetic((regstr*) arg1->data, -1);
  if (d != NULL)
    ret_ans(reg,d);
}


void
op_last (arg a, registry* reg)
{
  
  
  check_length(&a, 2+1, "last");
if (is_error(-1)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<last> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Registry)))
  {
    do_error("Argument 1 of <last> should be of type Registry.");
    return ;
  }

;

  
  
  
data* arg2 = resolve(a.arg_array[2], reg);

if (true)
  {
    if (arg2 == NULL)
      {
        do_error("<last> requires at least 2 arguments.");
        return ;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & String)))
  {
    do_error("Argument 2 of <last> should be of type String.");
    return ;
  }

;

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

  d = new_data();
  d->type = Register;
  d->data = malloc(sizeof(regstr));
  ((regstr*) d->data)->name = name;
  ((regstr*) d->data)->key = hash_str(name);

  ret_ans(reg, d);
  
}

void
op_in (arg a, registry* reg)
{
  
  
  check_length(&a, 2+1, "in");
if (is_error(-1)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<in> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Registry)))
  {
    do_error("Argument 1 of <in> should be of type Registry.");
    return ;
  }

;

  
  
  
data* arg2 = resolve(a.arg_array[2], reg);

if (true)
  {
    if (arg2 == NULL)
      {
        do_error("<in> requires at least 2 arguments.");
        return ;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & Instruction)))
  {
    do_error("Argument 2 of <in> should be of type Instruction.");
    return ;
  }

;

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
  
  
  check_length(&a, 2+1, "while");
if (is_error(-1)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<while> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Instruction)))
  {
    do_error("Argument 1 of <while> should be of type Instruction.");
    return ;
  }

;

  
  
data* arg2 = resolve(a.arg_array[2], reg);

if (true)
  {
    if (arg2 == NULL)
      {
        do_error("<while> requires at least 2 arguments.");
        return ;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & Instruction)))
  {
    do_error("Argument 2 of <while> should be of type Instruction.");
    return ;
  }

;

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

      if (d->type != Boolean)
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
  
  
  check_length(&a, 2+1, "repeat");
if (is_error(-1)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<repeat> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Integer)))
  {
    do_error("Argument 1 of <repeat> should be of type Integer.");
    return ;
  }

;

  
  
  
data* arg2 = resolve(a.arg_array[2], reg);

if (true)
  {
    if (arg2 == NULL)
      {
        do_error("<repeat> requires at least 2 arguments.");
        return ;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & Instruction)))
  {
    do_error("Argument 2 of <repeat> should be of type Instruction.");
    return ;
  }

;

  mpz_t i;
  mpz_init_set_si(i, 0);
  while (mpz_cmp(i, *((mpz_t*) arg1->data)) < 0)
    {
      execute_0(arg2, reg);
      if (is_error(-1)) break;
      mpz_add_ui(i, i, 1);
    }
}

void
op_to_register (arg a, registry* reg)
{
  
  
  check_length(&a, 1+1, "to-register");
if (is_error(-1)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<to-register> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & (String|Integer))))
  {
    do_error("Argument 1 of <to-register> should be of type (String|Integer).");
    return ;
  }

;

  data* d;

  if (arg1->type == String)
    {
      assign_regstr(&d, (char*) arg1->data,
                    hash_str((char*) arg1->data));
    }
  else
    {
      int val = mpz_get_si(*((mpz_t*) arg1->data));
      char* name = argument_name(val);
      unsigned long hash_name = hash_str(name);
      assign_regstr(&d, name, hash_name);
      free(name);
    }

  ret_ans(reg,d);

}


void
op_collapse (arg a, registry* reg)
{
  
  
  check_length(&a, 2+1, "collapse");
if (is_error(-1)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<collapse> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Operation)))
  {
    do_error("Argument 1 of <collapse> should be of type Operation.");
    return ;
  }

;

  
  
  
data* arg2 = resolve(a.arg_array[2], reg);

if (true)
  {
    if (arg2 == NULL)
      {
        do_error("<collapse> requires at least 2 arguments.");
        return ;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & Registry)))
  {
    do_error("Argument 2 of <collapse> should be of type Registry.");
    return ;
  }

;

  const char* prefix = "t";
  if (a.length >= 4)
    {
      
      
      
data* arg3 = resolve(a.arg_array[3], reg);

if (true)
  {
    if (arg3 == NULL)
      {
        do_error("<collapse> requires at least 3 arguments.");
        return ;
      }
  }
if (arg3 != NULL && true && (!(arg3->type & String)))
  {
    do_error("Argument 3 of <collapse> should be of type String.");
    return ;
  }

;

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
  a1.length = 3;
  a1.free_data = malloc(sizeof(int)*a1.length);
  a1.arg_array = malloc(sizeof(data*)*a1.length);
  for (int j = 0; j < a1.length; j++)
    {
      a1.free_data[j] = 0;
    }

  a1.arg_array[0] = arg1;
  
  data* d1;
  data* d2;

  int is_first = 1;
  
  while ((d = lookup(to_walk, second_hash, 0)) != NULL)
    {

      if (is_first)
        d1 = lookup(to_walk, first_hash, 0);
      
      d2 = lookup(to_walk, second_hash, 0);

      a1.arg_array[1] = d1;
      a1.arg_array[2] = d2;
        
      compute(arg1, r, a1);
      d1 = lookup(r, arbel_hash_ans, 0);

      if (d1 == NULL)
        {
          do_error("Operation did not set /ans register.");
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

  del(r, arbel_hash_0, 0, false);

  free_registry(r);
  free(second_name);
  free_arg(&a1);


}

void
op_string_eq (arg a, registry* reg)
{
  
  
  check_length(&a, 2+1, "string-eq");
if (is_error(-1)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<string-eq> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & String)))
  {
    do_error("Argument 1 of <string-eq> should be of type String.");
    return ;
  }

;

  
  
  
data* arg2 = resolve(a.arg_array[2], reg);

if (true)
  {
    if (arg2 == NULL)
      {
        do_error("<string-eq> requires at least 2 arguments.");
        return ;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & String)))
  {
    do_error("Argument 2 of <string-eq> should be of type String.");
    return ;
  }

;

  unsigned char* str1 = (unsigned char*) arg1->data;
  unsigned char* str2 = (unsigned char*) arg2->data;
  data* d;
  if (u8_strcmp(str1,str2) == 0)
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
  
  
  check_length(&a, 2+1, "string-lt");
if (is_error(-1)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<string-lt> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & String)))
  {
    do_error("Argument 1 of <string-lt> should be of type String.");
    return ;
  }

;

  
  
  
data* arg2 = resolve(a.arg_array[2], reg);

if (true)
  {
    if (arg2 == NULL)
      {
        do_error("<string-lt> requires at least 2 arguments.");
        return ;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & String)))
  {
    do_error("Argument 2 of <string-lt> should be of type String.");
    return ;
  }

;

  unsigned char* str1 = (unsigned char*) arg1->data;
  unsigned char* str2 = (unsigned char*) arg2->data;
  data* d;
  if (u8_strcmp(str1,str2) < 0)
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
  
  
  check_length(&a, 2+1, "string-gt");
if (is_error(-1)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<string-gt> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & String)))
  {
    do_error("Argument 1 of <string-gt> should be of type String.");
    return ;
  }

;

  
  
  
data* arg2 = resolve(a.arg_array[2], reg);

if (true)
  {
    if (arg2 == NULL)
      {
        do_error("<string-gt> requires at least 2 arguments.");
        return ;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & String)))
  {
    do_error("Argument 2 of <string-gt> should be of type String.");
    return ;
  }

;

  unsigned char* str1 = (unsigned char*) arg1->data;
  unsigned char* str2 = (unsigned char*) arg2->data;
  data* d;
  if (u8_strcmp(str1,str2) > 0)
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
  
  
  check_length(&a, 2+1, "register-eq");
if (is_error(-1)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<register-eq> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Register)))
  {
    do_error("Argument 1 of <register-eq> should be of type Register.");
    return ;
  }

;

  
  
  
data* arg2 = resolve(a.arg_array[2], reg);

if (true)
  {
    if (arg2 == NULL)
      {
        do_error("<register-eq> requires at least 2 arguments.");
        return ;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & Register)))
  {
    do_error("Argument 2 of <register-eq> should be of type Register.");
    return ;
  }

;
      
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
  
  
  check_length(&a, 1+1, "go-in");
if (is_error(-1)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<go-in> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Registry)))
  {
    do_error("Argument 1 of <go-in> should be of type Registry.");
    return ;
  }

;

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
    
  
  check_length(&a, 1+1, "save");
if (is_error(-1)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<save> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & String)))
  {
    do_error("Argument 1 of <save> should be of type String.");
    return ;
  }

;

  char* fname = (char*) arg1->data;
  FILE* f = fopen(fname, "wb");
  save_registry(f, reg);
  data_type end = Nothing;
  fwrite(&end, sizeof(data_type), 1, f);
  fclose(f);

}

void
op_load (arg a, registry* reg)
{
  
  
  check_length(&a, 1+1, "load");
if (is_error(-1)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<load> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & String)))
  {
    do_error("Argument 1 of <load> should be of type String.");
    return ;
  }

;

  char* fname = (char*) arg1->data;
  FILE* f = fopen(fname, "rb");
  if (f == NULL)
    {
      do_error("File cannot be opened.");
      return;
    }
  
  if (a.length >= 3)
    {
      
      
      
data* arg2 = resolve(a.arg_array[2], reg);

if (true)
  {
    if (arg2 == NULL)
      {
        do_error("<load> requires at least 2 arguments.");
        return ;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & Registry)))
  {
    do_error("Argument 2 of <load> should be of type Registry.");
    return ;
  }

;

      read_registry(f, (registry*) arg2->data);
    }
  else
    {
      read_registry(f, reg);
    }
  fclose(f);

}

void
op_to_string (arg a, registry* reg)
{
  
  
  check_length(&a, 1+1, "to-string");
if (is_error(-1)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<to-string> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & (Integer | Real | Register))))
  {
    do_error("Argument 1 of <to-string> should be of type (Integer | Real | Register).");
    return ;
  }

;

  char* result;
  if (arg1->type == Integer)
    {
      result = mpz_get_str(NULL, 10, *((mpz_t*) arg1->data));
    }
  else if (arg1->type == Real)
    {
      data* arg2 = NULL;
      if (a.length >= 3)
        {
          arg2 = resolve(a.arg_array[2], reg);
        }
      
      int prec = 6;
      if (arg2 != NULL && arg2->type != Integer)
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
  
  
  check_length(&a, 1+1, "to-number");
if (is_error(-1)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<to-number> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & (String|Register))))
  {
    do_error("Argument 1 of <to-number> should be of type (String|Register).");
    return ;
  }

;

  if (arg1->type == String)
    {
      char* value = (char*) arg1->data;
      data* d;
      if (is_integer(value))
        {
          mpz_t result;
          mpz_init_set_str(result, value, 10);
          assign_int(&d, result);
          mpz_clear(result);
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
      mpz_t m;
      mpz_init_set_str(m, name, 10);
      assign_int(&d, m);
      mpz_clear(m);
      ret_ans(reg,d);
    }
  
}

void
op_to_real (arg a, registry* reg)
{
  
  
  check_length(&a, 1+1, "to-real");
if (is_error(-1)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<to-real> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & (Integer|Real|String))))
  {
    do_error("Argument 1 of <to-real> should be of type (Integer|Real|String).");
    return ;
  }

;

  data* d;
  if (arg1->type == Integer)
    assign_real(&d, mpz_get_d(*((mpz_t*) arg1->data)));
  else if (arg1->type == Real)
    d = copy_data(arg1);
  else
    {
      double result = atof((char*) arg1->data);
      assign_real(&d, result);
    }
    
  ret_ans(reg,d);
  
}

void
op_output_code (arg a, registry* reg)
{
  
  
  check_length(&a, 1+1, "output-code");
if (is_error(-1)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<output-code> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & String)))
  {
    do_error("Argument 1 of <output-code> should be of type String.");
    return ;
  }

;

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
  
  
  check_length(&a, 1+1, "error");
if (is_error(-1)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<error> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & String)))
  {
    do_error("Argument 1 of <error> should be of type String.");
    return ;
  }

;

  do_error((char*) arg1->data);

  if (a.length >= 3)
    {
      
      
      
      
data* arg2 = resolve(a.arg_array[2], reg);

if (false)
  {
    if (arg2 == NULL)
      {
        do_error("<error> requires at least 2 arguments.");
        return ;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & Integer)))
  {
    do_error("Argument 2 of <error> should be of type Integer.");
    return ;
  }

;
      
      if (arg2 != NULL)
        {
          is_error(mpz_get_si(*((mpz_t*) arg2->data)));
        }
    }
}

void
op_is_type (arg a, registry* reg, const data_type type)
{
  
  
  check_length(&a, 1+1, "is-Type");
if (is_error(-1)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<is-Type> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && false && (!(arg1->type & Integer)))
  {
    do_error("Argument 1 of <is-Type> should be of type Integer.");
    return ;
  }

;
  
  data* d;
  if (arg1->type == type || ((type==Instruction) &&
                             (arg1->type==Operation)))
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
  op_is_type(a, reg, Integer);
}

void
op_is_real (arg a, registry* reg)
{
  op_is_type(a, reg, Real);
}

void
op_is_string (arg a, registry* reg)
{
  op_is_type(a, reg, String);
}

void
op_is_register (arg a, registry* reg)
{
  op_is_type(a, reg, Register);
}

void
op_is_registry (arg a, registry* reg)
{
  op_is_type(a, reg, Registry);
}

void
op_is_instruction (arg a, registry* reg)
{
  op_is_type(a, reg, Instruction);
}

void
op_is_file (arg a, registry* reg)
{
  op_is_type(a, reg, File);
}

void
op_is_nothing (arg a, registry* reg)
{
  op_is_type(a, reg, Nothing);
}

void
op_is_boolean (arg a, registry* reg)
{
  op_is_type(a, reg, Boolean);
}

void
op_is_operation (arg a, registry* reg)
{
  op_is_type(a, reg, Operation);
}


void
op_open_text_file (arg a, registry* reg)
{
  
  
  check_length(&a, 1+1, "open-text-file");
if (is_error(-1)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<open-text-file> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & String)))
  {
    do_error("Argument 1 of <open-text-file> should be of type String.");
    return ;
  }

;
  
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
    
  
  check_length(&a, 1+1, "read");
if (is_error(-1)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<read> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & File)))
  {
    do_error("Argument 1 of <read> should be of type File.");
    return ;
  }

;

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
  
  
  check_length(&a, 1+1, "close");
if (is_error(-1)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<close> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Register)))
  {
    do_error("Argument 1 of <close> should be of type Register.");
    return ;
  }

;

  data* f = lookup(reg, ((regstr*) arg1->data)->key, 0);

  if (f == NULL)
    {
      do_error("Register does not exist.");
      return;
    }

  if (f->type != File)
    {
      do_error("Register does not contain a file.");
      return;
    }

  if (f->data != NULL)
    {
      fclose((FILE*) f->data);
    }

  del(reg, ((regstr*) arg1->data)->key, 1, false);
  
}

void
op_or (arg a, registry* reg)
{
  
  
  check_length(&a, 2+1, "or");
if (is_error(-1)) return ;;

  bool check = false;
  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<or> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Boolean)))
  {
    do_error("Argument 1 of <or> should be of type Boolean.");
    return ;
  }

;
  if (*((bool*) arg1->data))
    {
      check = true;
    }
  else
    {
      
      
data* arg2 = resolve(a.arg_array[2], reg);

if (true)
  {
    if (arg2 == NULL)
      {
        do_error("<or> requires at least 2 arguments.");
        return ;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & Boolean)))
  {
    do_error("Argument 2 of <or> should be of type Boolean.");
    return ;
  }

;
      if (*((bool*) arg2->data))
        check = true;
    }

  data* d;
  assign_boolean(&d, check);

  ret_ans(reg,d);
}

void
op_and (arg a, registry* reg)
{
  
  
  check_length(&a, 2+1, "and");
if (is_error(-1)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<and> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Boolean)))
  {
    do_error("Argument 1 of <and> should be of type Boolean.");
    return ;
  }

;

  bool check = true;

  if (!(*((bool*) arg1->data)))
    {
      check = false;
    }
  else
    {
      
      
data* arg2 = resolve(a.arg_array[2], reg);

if (true)
  {
    if (arg2 == NULL)
      {
        do_error("<and> requires at least 2 arguments.");
        return ;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & Boolean)))
  {
    do_error("Argument 2 of <and> should be of type Boolean.");
    return ;
  }

;
      if (!(*((bool*) arg2->data)))
        check = false;
    }

  data* d;
  assign_boolean(&d, check);

  ret_ans(reg,d);
}

void
op_not (arg a, registry* reg)
{
  
  
  check_length(&a, 1+1, "not");
if (is_error(-1)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<not> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Boolean)))
  {
    do_error("Argument 1 of <not> should be of type Boolean.");
    return ;
  }

;

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
  
  
  check_length(&a, 1+1, "read-line");
if (is_error(-1)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<read-line> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & File)))
  {
    do_error("Argument 1 of <read-line> should be of type File.");
    return ;
  }

;

  char* line  = NULL;
  size_t len = 0;
  ssize_t ret = getdelim(&line, &len, '\n', (FILE*) arg1->data);

  data* d;
  if (ret >= 0)
    {
      if (!feof((FILE*) arg1->data))
	line[strlen(line)-1] = '\0';
      assign_str(&d, line, 0);
      ret_ans(reg,d);
    }
  else
    {
      assign_nothing(&d);
      ret_ans(reg,d);
      #ifdef GARBAGE
	#undef free
	#endif
	free(line);
      #ifdef GARBAGE
      #define free(x)
      #endif

    }
}

void
op_write (arg a, registry* reg)
{
  
  
  check_length(&a, 2+1, "write");
if (is_error(-1)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<write> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & String)))
  {
    do_error("Argument 1 of <write> should be of type String.");
    return ;
  }

;

  
  
  
data* arg2 = resolve(a.arg_array[2], reg);

if (true)
  {
    if (arg2 == NULL)
      {
        do_error("<write> requires at least 2 arguments.");
        return ;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & File)))
  {
    do_error("Argument 2 of <write> should be of type File.");
    return ;
  }

;

  fwrite((char*) arg1->data, sizeof(char), strlen((char*) arg1->data), (FILE*) arg2->data);

}

void
op_input (arg a, registry* reg)
{
  
  check_length(&a, 1+1, "write");
if (is_error(-1)) return ;;

  
  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<input> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Register)))
  {
    do_error("Argument 1 of <input> should be of type Register.");
    return ;
  }

;

  char* input = readline("");
  data* d;
  assign_str(&d, input, 0);
  
  set(reg, d, ((regstr*) arg1->data)->name, 1);
  
}

void
op_shell (arg a, registry* reg)
{

  
  
  check_length(&a, 1+1, "shell");
if (is_error(-1)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<shell> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & String)))
  {
    do_error("Argument 1 of <shell> should be of type String.");
    return ;
  }

;

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
  
  
  check_length(&a, 3+1, "link");
if (is_error(-1)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<link> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & String)))
  {
    do_error("Argument 1 of <link> should be of type String.");
    return ;
  }

;

  
  
data* arg2 = resolve(a.arg_array[2], reg);

if (true)
  {
    if (arg2 == NULL)
      {
        do_error("<link> requires at least 2 arguments.");
        return ;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & String)))
  {
    do_error("Argument 2 of <link> should be of type String.");
    return ;
  }

;

  
  
data* arg3 = resolve(a.arg_array[3], reg);

if (true)
  {
    if (arg3 == NULL)
      {
        do_error("<link> requires at least 3 arguments.");
        return ;
      }
  }
if (arg3 != NULL && true && (!(arg3->type & String)))
  {
    do_error("Argument 3 of <link> should be of type String.");
    return ;
  }

;

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
  assign_op(&d, new_op, NULL, NULL, 0);
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
  
  
  check_length(&a, 2+1, "match");
if (is_error(-1)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<match> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & String)))
  {
    do_error("Argument 1 of <match> should be of type String.");
    return ;
  }

;

  
  
data* arg2 = resolve(a.arg_array[2], reg);

if (true)
  {
    if (arg2 == NULL)
      {
        do_error("<match> requires at least 2 arguments.");
        return ;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & String)))
  {
    do_error("Argument 2 of <match> should be of type String.");
    return ;
  }

;
  

  int max_matches = 0;
  if (a.length >= 4)
    {
      
      
      
data* arg3 = resolve(a.arg_array[3], reg);

if (true)
  {
    if (arg3 == NULL)
      {
        do_error("<match> requires at least 3 arguments.");
        return ;
      }
  }
if (arg3 != NULL && true && (!(arg3->type & Integer)))
  {
    do_error("Argument 3 of <match> should be of type Integer.");
    return ;
  }

;
      max_matches = mpz_get_ui(*((mpz_t*) arg3->data));
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
  assign_registry(&d, NULL, true);
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
          assign_registry(&d_reg, NULL, true);
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
  
  
  check_length(&a, 3+1, "replace");
if (is_error(-1)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<replace> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & String)))
  {
    do_error("Argument 1 of <replace> should be of type String.");
    return ;
  }

;

  
  
data* arg2 = resolve(a.arg_array[2], reg);

if (true)
  {
    if (arg2 == NULL)
      {
        do_error("<replace> requires at least 2 arguments.");
        return ;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & String)))
  {
    do_error("Argument 2 of <replace> should be of type String.");
    return ;
  }

;

  
  
data* arg3 = resolve(a.arg_array[3], reg);

if (true)
  {
    if (arg3 == NULL)
      {
        do_error("<replace> requires at least 3 arguments.");
        return ;
      }
  }
if (arg3 != NULL && true && (!(arg3->type & String)))
  {
    do_error("Argument 3 of <replace> should be of type String.");
    return ;
  }

;

  int max_replace = 0;
  if (a.length >= 5)
    {
      
      
      
data* arg4 = resolve(a.arg_array[4], reg);

if (true)
  {
    if (arg4 == NULL)
      {
        do_error("<replace> requires at least 4 arguments.");
        return ;
      }
  }
if (arg4 != NULL && true && (!(arg4->type & Integer)))
  {
    do_error("Argument 4 of <replace> should be of type Integer.");
    return ;
  }

;
      max_replace = mpz_get_ui(*((mpz_t*) arg4->data));
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
  assign_registry(&d, NULL, true);


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
  
  
  check_length(&a, 1+1, "log");
if (is_error(-1)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<log> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & (Integer|Real))))
  {
    do_error("Argument 1 of <log> should be of type (Integer|Real).");
    return ;
  }

;
  
  data* d;
  if (arg1->type == Integer)
    {
      assign_real(&d, log(mpz_get_d((*((mpz_t*) arg1->data)))));
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
  
  
  check_length(&a, 1+1, "exp");
if (is_error(-1)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<exp> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & (Integer|Real))))
  {
    do_error("Argument 1 of <exp> should be of type (Integer|Real).");
    return ;
  }

;

  data* d;
  if (arg1->type == Integer)
    {
      assign_real(&d, exp(mpz_get_d(*((mpz_t*) arg1->data))));
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
  
  
  check_length(&a, 2+1, "power");
if (is_error(-1)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<power> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & (Integer|Real))))
  {
    do_error("Argument 1 of <power> should be of type (Integer|Real).");
    return ;
  }

;

  
  
data* arg2 = resolve(a.arg_array[2], reg);

if (true)
  {
    if (arg2 == NULL)
      {
        do_error("<power> requires at least 2 arguments.");
        return ;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & (Integer|Real))))
  {
    do_error("Argument 2 of <power> should be of type (Integer|Real).");
    return ;
  }

;

  double base;
  double power;
  data* d;
  if (arg1->type == Integer)
    {
      base = mpz_get_d(*((mpz_t*) arg1->data));
    }
  else
    {
      base = *((double*) arg1->data);
    }

  if (arg2->type == Integer)
    {
      power = mpz_get_d(*((mpz_t*) arg2->data));
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
  
  
  check_length(&a, 1+1, "change-dir");
if (is_error(-1)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<change-dir> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & String)))
  {
    do_error("Argument 1 of <change-dir> should be of type String.");
    return ;
  }

;

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
  
  
  check_length(&a, 1+1, "import");
if (is_error(-1)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<import> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Registry)))
  {
    do_error("Argument 1 of <import> should be of type Registry.");
    return ;
  }

;

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
  
  
  check_length(&a, 3+1, "substring");
if (is_error(-1)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<substring> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & String)))
  {
    do_error("Argument 1 of <substring> should be of type String.");
    return ;
  }

;

  
  
  
data* arg2 = resolve(a.arg_array[2], reg);

if (true)
  {
    if (arg2 == NULL)
      {
        do_error("<substring> requires at least 2 arguments.");
        return ;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & Integer)))
  {
    do_error("Argument 2 of <substring> should be of type Integer.");
    return ;
  }

;

  
  
data* arg3 = resolve(a.arg_array[3], reg);

if (true)
  {
    if (arg3 == NULL)
      {
        do_error("<substring> requires at least 3 arguments.");
        return ;
      }
  }
if (arg3 != NULL && true && (!(arg3->type & Integer)))
  {
    do_error("Argument 3 of <substring> should be of type Integer.");
    return ;
  }

;

  unsigned char* str = (unsigned char*) arg1->data;
  size_t start = mpz_get_ui(*((mpz_t*) arg2->data));
  size_t end = mpz_get_ui(*((mpz_t*) arg3->data));

  size_t byte_length = strlen((char*) str)+1;
  size_t length = u8_mbsnlen((unsigned char*) str,
                             byte_length-1);

  start = arbel_location(start, length);
  end = arbel_location(end, length);

  if ((start > length) || (start <= 0))
    {
      do_error("Index out of range.");
      return;
    }


  if ((end > length) || (end <= 0))
    {
      do_error("Index out of range.");
      return;
    }

  if (start > end)
    {
      do_error("The starting position of the substring is greater than the ending position.");
      return;
    }

  unsigned char* first = str;
  ucs4_t c;
  for (int i=0; i < (start-1); i++)
    {
      first = u8_next(&c, first);
    }

  unsigned char* last = first;
  int sz = u8_mblen(last, byte_length);
  for (int i=0; i < (end-start); i++)
    {
      last = u8_next(&c, last);
      sz += u8_mblen(last, byte_length);
    }

  char* result = malloc(sizeof(char)*(sz + 1));

  for (int i=0; i < sz; i++)
    {
      result[i] = (char) first[i];
    }

  result[sz] = '\0';

  data* d;
  assign_str(&d, result, 0);
  ret_ans(reg, d);
  
}

void
op_up (arg a, registry* reg)
{
  
  
  check_length(&a, 1+1, "up");
if (is_error(-1)) return ;;

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
    
  
  check_length(&a, 2+1, "of");
if (is_error(-1)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<of> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & String)))
  {
    do_error("Argument 1 of <of> should be of type String.");
    return ;
  }

;

  
  
  
data* arg2 = resolve(a.arg_array[2], reg);

if (true)
  {
    if (arg2 == NULL)
      {
        do_error("<of> requires at least 2 arguments.");
        return ;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & Registry)))
  {
    do_error("Argument 2 of <of> should be of type Registry.");
    return ;
  }

;

  data* d;
  assign_str(&d, (char*) arg1->data, 1);
  set((registry*) arg2->data, d, "--of", 1);
}

void
op_is_of (arg a, registry* reg)
{
  
  
  check_length(&a, 2+1, "is-of");
if (is_error(-1)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<is-of> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & String)))
  {
    do_error("Argument 1 of <is-of> should be of type String.");
    return ;
  }

;

  
  
  
data* arg2 = resolve(a.arg_array[2], reg);

if (true)
  {
    if (arg2 == NULL)
      {
        do_error("<is-of> requires at least 2 arguments.");
        return ;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & Registry)))
  {
    do_error("Argument 2 of <is-of> should be of type Registry.");
    return ;
  }

;

  data* d = lookup((registry*) arg2->data, arbel_hash_class, 0);

  if (d == NULL || d->type != String)
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
  
  
  check_length(&a, 2+1, "dispatch");
if (is_error(-1)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<dispatch> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & String)))
  {
    do_error("Argument 1 of <dispatch> should be of type String.");
    return ;
  }

;

  
  
  
data* arg2 = resolve(a.arg_array[2], reg);

if (true)
  {
    if (arg2 == NULL)
      {
        do_error("<dispatch> requires at least 2 arguments.");
        return ;
      }
  }
if (arg2 != NULL && false && (!(arg2->type & String)))
  {
    do_error("Argument 2 of <dispatch> should be of type String.");
    return ;
  }

;
  

  const char* class;
  data* d = NULL;
  switch (arg2->type)
    {
    case Integer:
      class = "Integer";
      break;
    case Real:
      class = "Real";
      break;
    case String:
      class = "String";
      break;
    case Register:
      class = "Register";
      break;
    case Registry:
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
    case Instruction:
      class = "Instruction";
      break;
    case Operation:
      class = "Instruction";
      break;
    case File:
      class = "File";
      break;
    case Nothing:
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
  
  
  check_length(&a, 1+1, "code");
if (is_error(-1)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<code> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Instruction)))
  {
    do_error("Argument 1 of <code> should be of type Instruction.");
    return ;
  }

;

  data* d;
  assign_str(&d, ((instruction*) arg1->data)->code, 1);

  ret_ans(reg, d);
      
}

void
op_is_error (arg a, registry* reg)
{
  
  
  check_length(&a, 1+1, "is-error");
if (is_error(-1)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<is-error> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Instruction)))
  {
    do_error("Argument 1 of <is-error> should be of type Instruction.");
    return ;
  }

;

  int stop_thresh = arbel_stop_error_threshold;
  arbel_stop_error_threshold = 0;
  execute_0(arg1, reg);
  data* d;
  assign_boolean(&d, is_error(-1) > 0 ? true : false);
  arbel_stop_error_threshold = stop_thresh;

  ret_ans(reg, d);
  is_error(0);

}


void
op_call (arg a, registry* reg)
{
  
  
  check_length(&a, 1+1, "call");
if (is_error(-1)) return ;;
  _op_call(a,reg,1);
}

void
op_push_through (arg a, registry* reg)
{
  
  
  check_length(&a, 1+1, "push-through");
if (is_error(-1)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<push-through> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Instruction)))
  {
    do_error("Argument 1 of <push-through> should be of type Instruction.");
    return ;
  }

;

  int stop_thresh = arbel_stop_error_threshold;
  arbel_stop_error_threshold = 0;
  execute_0(arg1, reg);
  arbel_stop_error_threshold = stop_thresh;
}

void
op_version (arg a, registry* reg)
{
  data* d;
  assign_str(&d, PACKAGE_VERSION, 1);
  ret_ans(reg, d);
}

void
op_error_messages (arg a, registry* reg)
{
  
  
  check_length(&a, 1+1, "error-messages");
if (is_error(-1)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<error-messages> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Boolean)))
  {
    do_error("Argument 1 of <error-messages> should be of type Boolean.");
    return ;
  }

;

  if (*((bool*) arg1->data))
    arbel_print_error_messages = true;
  else
    arbel_print_error_messages = false;

}

void
op_find (arg a, registry* reg)
{
  
  
  check_length(&a, 2+1, "find");
if (is_error(-1)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<find> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Registry)))
  {
    do_error("Argument 1 of <find> should be of type Registry.");
    return ;
  }

;

  
  
  
data* arg2 = resolve(a.arg_array[2], reg);

if (true)
  {
    if (arg2 == NULL)
      {
        do_error("<find> requires at least 2 arguments.");
        return ;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & Instruction)))
  {
    do_error("Argument 2 of <find> should be of type Instruction.");
    return ;
  }

;

  registry* r = (registry*) arg1->data;
  registry* result = new_registry(reg, r->hash_size);
  arg a1;
  a1.length = 3;
  a1.free_data = malloc(sizeof(int)*a1.length);
  for (int i = 0; i < a1.length; i++)
    a1.free_data[i] = 0;

  a1.arg_array = malloc(sizeof(data*)*a1.length);
  a1.arg_array[0] = arg2;
  assign_regstr(&a1.arg_array[1], "t", arbel_hash_t);
  a1.arg_array[2] = NULL;
  
  for (int i = 0; i < r->hash_size; i++)
    {
      content* c = r->objects[i];
      if (c == NULL)
        continue;

      if (is_init_reg(c))
        continue;

      c = tail(c);
      while (c != NULL)
        {
          if (c->value != NULL)
            {
              a1.arg_array[2] = c->value;
              compute(arg2, reg, a1);
              data* d = lookup(reg, arbel_hash_ans, 0);
              if ((d != NULL) && (d->type == Boolean) &&
                  *((bool*) d->data))
                {
                  set(result, copy_data(c->value), c->name, 0);
                }
            }
          c = c->right;
        }
    }

  data* d;
  assign_registry(&d, result, false);
  ret_ans(reg, d);
}

void
op_please (arg a, registry* reg)
{
  

  
  check_length(&a, 2+1, "please");
if (is_error(-1)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<please> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Instruction)))
  {
    do_error("Argument 1 of <please> should be of type Instruction.");
    return ;
  }

;

  
  
  
data* arg2 = resolve(a.arg_array[2], reg);

if (true)
  {
    if (arg2 == NULL)
      {
        do_error("<please> requires at least 2 arguments.");
        return ;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & Instruction)))
  {
    do_error("Argument 2 of <please> should be of type Instruction.");
    return ;
  }

;

  execute_0(arg1, reg);

  if (is_error(-1))
    {
      is_error(0);
      execute_0(arg2, reg);
    }
}

void
op_mod (arg a, registry* reg)
{

  

  
  check_length(&a, 2+1, "mod");
if (is_error(-1)) return ;;
  
  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<mod> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & (Real | Integer))))
  {
    do_error("Argument 1 of <mod> should be of type (Real | Integer).");
    return ;
  }

;

  
  
  
data* arg2 = resolve(a.arg_array[2], reg);

if (true)
  {
    if (arg2 == NULL)
      {
        do_error("<mod> requires at least 2 arguments.");
        return ;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & (Real | Integer))))
  {
    do_error("Argument 2 of <mod> should be of type (Real | Integer).");
    return ;
  }

;

  data* d;
  if (arg1->type == Real)
    {
      if (arg2->type == Real)
        {
          double res = fmod(*((double*) arg1->data),
                            *((double*) arg2->data));
          assign_real(&d, res);
        }
      else
        {
          double res = fmod(*((double*) arg1->data),
                            mpz_get_d(*((mpz_t*) arg2->data)));
          assign_real(&d, res);
        }
    }
  else
    {
      if (arg2->type == Real)
        {
          double res = fmod((double) *((int*) arg1->data),
                            *((double*) arg2->data));
          assign_real(&d, res);
        }
      else
        {
          mpz_t res;
          mpz_init(res);
          mpz_fdiv_r(res, *((mpz_t*) arg1->data),
                     *((mpz_t*) arg2->data));
          assign_int(&d, res);
        }
    }

  ret_ans(reg,d);
}

void
op_op (arg a, registry* reg)
{
  /* design:
     op instruction /y /x .
     ans 2 3 .
     gives ans = whatever instruction /y 2 /x 3 would give

     I think the only reasonable solution is to make operation:

     struct operation
     {
       operator op;
       instruction* instr;
       char** names;
       unsigned long* hashes;
       int n_arg;
     };
     
     if (oper->instr != NULL)
       call the instruction with order arguments
     else
       call the C code in oper->op

     The main payoff here is that you can treat operators and instructions interchangeably.

     The other solution is to add an "instruction" operation which treats each argument of the
     operator as t1,t2,t3...  This is arguably the more ARBEL-ish way to do it.  Requires less code
     changes and is more in keeping with the system.

     ... instruction add .
     ... ans /t1 2 /t2 3 .
     ans = 5
  */

  

  
  check_length(&a, 1+1, "op");
if (is_error(-1)) return ;;
    
  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<op> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Instruction)))
  {
    do_error("Argument 1 of <op> should be of type Instruction.");
    return ;
  }

;

  op_wrapper* new_op = malloc(sizeof(op_wrapper));
  new_op->n_arg = a.length-2;
  new_op->instr = copy_data(arg1);
  new_op->args = malloc(sizeof(data*)*new_op->n_arg);
  for (int i=2; i < a.length; i++)
    {
      
      
      
data* argi = resolve(a.arg_array[i], reg);

if (true)
  {
    if (argi == NULL)
      {
        do_error("<op> requires at least i arguments.");
        return ;
      }
  }
if (argi != NULL && true && (!(argi->type & Register)))
  {
    do_error("Argument i of <op> should be of type Register.");
    return ;
  }

;

      new_op->args[i-2] = copy_data(argi);
    }

  data* d = malloc(sizeof(data));
  d->data = new_op;
  d->type = Operation;
  ret_ans(reg, d);
}

void
op_data (arg a, registry* reg)
{
  
  
  check_length(&a, 1+1, "data");
if (is_error(-1)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<data> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & String)))
  {
    do_error("Argument 1 of <data> should be of type String.");
    return ;
  }

;

  char* home = getenv("HOME");
  char* fname = malloc(sizeof(char)*(strlen(home)+
                                     strlen((char*) arg1->data)+
                                     strlen(".darbs/")+
                                     strlen("/.darb")+1));
  sprintf(fname, "%s/.darbs/%s.darb", home, (char*) arg1->data);
  FILE* f = fopen(fname, "rb");
  if (f == NULL)
    {
      do_error("File cannot be opened.");
      free(fname);
      return;
    }

  read_registry(f, reg);
  fclose(f);
  free(fname);
}

static void
incr (data* d, mpz_t* shift)
{
  mpz_add(*((mpz_t*) d->data),
          *((mpz_t*) d->data),
          *shift);
}

void
op_incr (arg a, registry* reg)
{
  
  
  check_length(&a, 1+1, "incr");
if (is_error(-1)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<incr> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Integer)))
  {
    do_error("Argument 1 of <incr> should be of type Integer.");
    return ;
  }

;

  if (a.length >= 3)
    {
      
      
      
data* arg2 = resolve(a.arg_array[2], reg);

if (true)
  {
    if (arg2 == NULL)
      {
        do_error("<incr> requires at least 2 arguments.");
        return ;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & Integer)))
  {
    do_error("Argument 2 of <incr> should be of type Integer.");
    return ;
  }

;
      incr(arg1, (mpz_t*) arg2->data);
    }
  else
    {
      mpz_t m;
      mpz_init_set_si(m, 1);
      incr(arg1, &m);
    }
}

void
op_decr (arg a, registry* reg)
{
  
  
  check_length(&a, 1+1, "decr");
if (is_error(-1)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        do_error("<decr> requires at least 1 arguments.");
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Integer)))
  {
    do_error("Argument 1 of <decr> should be of type Integer.");
    return ;
  }

;

  if (a.length >= 3)
    {
      
      
      
data* arg2 = resolve(a.arg_array[2], reg);

if (true)
  {
    if (arg2 == NULL)
      {
        do_error("<decr> requires at least 2 arguments.");
        return ;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & Integer)))
  {
    do_error("Argument 2 of <decr> should be of type Integer.");
    return ;
  }

;
      mpz_t m;
      mpz_init_set(m, *((mpz_t*) arg2->data));
      mpz_neg(m, m);
      incr(arg1, &m);
    }
  else
    {
      mpz_t m;
      mpz_init_set_si(m, -1);
      incr(arg1, &m);
    }
}


void
add_basic_ops (registry* reg)
{
  data* d;

  assign_op(&d, op_set, NULL, NULL, 0);
  set(reg, d, "set",1);
  
  assign_op(&d, op_add, NULL, NULL, 0);
  set(reg,d,"add",1);

  assign_op(&d, op_mul, NULL, NULL, 0);
  set(reg,d,"mul",1);

  assign_op(&d, op_sub, NULL, NULL, 0);
  set(reg,d,"sub",1);

  assign_op(&d, op_div, NULL, NULL, 0);
  set(reg,d,"div",1);

  assign_op(&d, op_if, NULL, NULL, 0);
  set(reg,d,"if",1);

  assign_op(&d, op_registry, NULL, NULL, 0);
  set(reg,d,"registry",1);

  assign_op(&d, op_get, NULL, NULL, 0);
  set(reg,d,"get",1);

  assign_op(&d, op_move, NULL, NULL, 0);
  set(reg,d,"move",1);

  assign_op(&d, op_delete, NULL, NULL, 0);
  set(reg,d,"delete",1);

  assign_op(&d, op_exit, NULL, NULL, 0);
  set(reg,d,"exit",1);

  assign_op(&d, op_answer, NULL, NULL, 0);
  set(reg,d,"answer",1);

  assign_op(&d, op_sit, NULL, NULL, 0);
  set(reg,d,"sit",1);

  assign_op(&d, op_exist, NULL, NULL, 0);
  set(reg,d,"exist",1);

  assign_op(&d, op_gt, NULL, NULL, 0);
  set(reg,d,"gt",1);

  assign_op(&d, op_lt, NULL, NULL, 0);
  set(reg,d,"lt",1);

  assign_op(&d, op_eq, NULL, NULL, 0);
  set(reg,d,"eq",1);

  assign_op(&d, op_lt_eq, NULL, NULL, 0);
  set(reg,d,"lt-eq",1);

  assign_op(&d, op_gt_eq, NULL, NULL, 0);
  set(reg,d,"gt-eq",1);
  
  assign_op(&d, op_print, NULL, NULL, 0);
  set(reg,d,"print",1);

  assign_op(&d, op_string_length, NULL, NULL, 0);
  set(reg,d,"string-length",1);

  assign_op(&d, op_string_append, NULL, NULL, 0);
  set(reg,d,"string-append",1);

  assign_op(&d, op_source, NULL, NULL, 0);
  set(reg,d,"source",1);

  assign_op(&d, op_do_to_all, NULL, NULL, 0);
  set(reg,d,"do-to-all",1);

  assign_op(&d, op_next, NULL, NULL, 0);
  set(reg,d,"next",1);

  assign_op(&d, op_last, NULL, NULL, 0);
  set(reg,d,"last",1);

  assign_op(&d, op_in, NULL, NULL, 0);
  set(reg,d,"in",1);

  assign_op(&d, op_while, NULL, NULL, 0);
  set(reg,d,"while",1);
  
  assign_op(&d, op_list, NULL, NULL, 0);
  set(reg,d,"list",1);

  assign_op(&d, op_to_register, NULL, NULL, 0);
  set(reg,d,"to-register",1);

  assign_op(&d, op_collapse, NULL, NULL, 0);
  set(reg,d,"collapse",1);

  assign_op(&d, op_string_eq, NULL, NULL, 0);
  set(reg,d,"string-eq",1);

  assign_op(&d, op_string_gt, NULL, NULL, 0);
  set(reg,d,"string-gt",1);
  
  assign_op(&d, op_string_lt, NULL, NULL, 0);
  set(reg,d,"string-lt",1);

  assign_op(&d, op_register_eq, NULL, NULL, 0);
  set(reg,d,"register-eq",1);

  assign_op(&d, op_go_in, NULL, NULL, 0);
  set(reg,d,"go-in",1);

  assign_op(&d, op_go_out, NULL, NULL, 0);
  set(reg,d,"go-out",1);

  assign_op(&d, op_save, NULL, NULL, 0);
  set(reg,d,"save",1);

  assign_op(&d, op_load, NULL, NULL, 0);
  set(reg,d,"load",1);

  assign_op(&d, op_to_string, NULL, NULL, 0);
  set(reg,d,"to-string",1);
  
  assign_op(&d, op_to_number, NULL, NULL, 0);
  set(reg,d,"to-number",1);

  assign_op(&d, op_output_code, NULL, NULL, 0);
  set(reg,d,"output-code",1);

  assign_op(&d, op_clear_code, NULL, NULL, 0);
  set(reg,d,"clear-code",1);

  assign_op(&d, op_error, NULL, NULL, 0);
  set(reg,d,"error",1);

  assign_op(&d, op_is_integer, NULL, NULL, 0);
  set(reg,d,"is-integer",1);

  assign_op(&d, op_is_real, NULL, NULL, 0);
  set(reg,d,"is-real",1);

  assign_op(&d, op_is_string, NULL, NULL, 0);
  set(reg,d,"is-string",1);
  
  assign_op(&d, op_is_register, NULL, NULL, 0);
  set(reg,d,"is-register",1);

  assign_op(&d, op_is_registry, NULL, NULL, 0);
  set(reg,d,"is-registry",1);

  assign_op(&d, op_is_instruction, NULL, NULL, 0);
  set(reg,d,"is-instruction",1);

  assign_op(&d, op_is_file, NULL, NULL, 0);
  set(reg,d,"is-file",1);

  assign_op(&d, op_is_nothing, NULL, NULL, 0);
  set(reg,d,"is-nothing",1);

  assign_op(&d, op_is_boolean, NULL, NULL, 0);
  set(reg,d,"is-boolean",1);

  assign_op(&d, op_is_operation, NULL, NULL, 0);
  set(reg,d,"is-operation",1);
  
  assign_op(&d, op_open_text_file, NULL, NULL, 0);
  set(reg,d,"open-text-file",1);

  assign_op(&d, op_read, NULL, NULL, 0);
  set(reg,d,"read",1);

  assign_op(&d, op_close, NULL, NULL, 0);
  set(reg,d,"close",1);

  assign_op(&d, op_and, NULL, NULL, 0);
  set(reg,d,"and",1);

  assign_op(&d, op_or, NULL, NULL, 0);
  set(reg,d,"or",1);

  assign_op(&d, op_not, NULL, NULL, 0);
  set(reg,d,"not",1);

  assign_op(&d, op_read_line, NULL, NULL, 0);
  set(reg,d,"read-line",1);
  
  assign_op(&d, op_write, NULL, NULL, 0);
  set(reg,d,"write",1);
  
  assign_op(&d, op_input, NULL, NULL, 0);
  set(reg,d,"input",1);

  assign_op(&d, op_shell, NULL, NULL, 0);
  set(reg,d,"shell",1);

  assign_op(&d, op_link, NULL, NULL, 0);
  set(reg,d,"link",1);

  assign_op(&d, op_match, NULL, NULL, 0);
  set(reg,d,"match",1);

  assign_op(&d, op_replace, NULL, NULL, 0);
  set(reg,d,"replace",1);

  assign_op(&d, op_log, NULL, NULL, 0);
  set(reg,d,"log",1);

  assign_op(&d, op_exp, NULL, NULL, 0);
  set(reg,d,"exp",1);

  assign_op(&d, op_power, NULL, NULL, 0);
  set(reg,d,"power",1);

  assign_op(&d, op_change_dir, NULL, NULL, 0);
  set(reg,d,"change-dir",1);

  assign_op(&d, op_current_dir, NULL, NULL, 0);
  set(reg,d,"current-dir",1);

  assign_op(&d, op_import, NULL, NULL, 0);
  set(reg,d,"import",1);
  
  assign_op(&d, op_repeat, NULL, NULL, 0);
  set(reg,d,"repeat",1);

  assign_op(&d, op_substring, NULL, NULL, 0);
  set(reg,d,"substring",1);

  assign_op(&d, op_up, NULL, NULL, 0);
  set(reg,d,"up",1);
  
  assign_op(&d, op_of, NULL, NULL, 0);
  set(reg,d,"of",1);

  assign_op(&d, op_is_of, NULL, NULL, 0);
  set(reg,d,"is-of",1);

  assign_op(&d, op_dispatch, NULL, NULL, 0);
  set(reg,d,"dispatch",1);

  assign_op(&d, op_to_real, NULL, NULL, 0);
  set(reg,d,"to-real",1);

  assign_op(&d, op_call, NULL, NULL, 0);
  set(reg,d,"call",1);

  assign_op(&d, op_code, NULL, NULL, 0);
  set(reg,d,"code",1);

  assign_op(&d, op_is_error, NULL, NULL, 0);
  set(reg,d,"is-error",1);

  assign_op(&d, op_range, NULL, NULL, 0);
  set(reg,d,"range",1);

  assign_op(&d, op_push_through, NULL, NULL, 0);
  set(reg,d,"push-through",1);

  assign_op(&d, op_error_messages, NULL, NULL, 0);
  set(reg,d,"error-messages",1);

  assign_op(&d, op_version, NULL, NULL, 0);
  set(reg,d,"version",1);

  assign_op(&d, op_free, NULL, NULL, 0);
  set(reg,d,"free",1);

  assign_op(&d, op_previous, NULL, NULL, 0);
  set(reg,d,"previous",1);

  assign_op(&d, op_find, NULL, NULL, 0);
  set(reg,d,"find",1);

  assign_op(&d, op_please, NULL, NULL, 0);
  set(reg,d,"please",1);

  assign_op(&d, op_mod, NULL, NULL, 0);
  set(reg,d,"mod",1);

  assign_op(&d, op_op, NULL, NULL, 0);
  set(reg,d,"op",1);

  assign_op(&d, op_data, NULL, NULL, 0);
  set(reg,d,"data",1);

  assign_op(&d, op_incr, NULL, NULL, 0);
  set(reg,d,"incr",1);

  assign_op(&d, op_decr, NULL, NULL, 0);
  set(reg,d,"decr",1);
  
  
}
  
