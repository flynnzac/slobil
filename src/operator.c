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
  

  
  
  check_length(&a, 2+1, "range", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<range> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<range> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Integer)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <range> should be of type Integer.")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <range> should be of type Integer.", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
    return ;
  }

;

  
  
data* arg2 = resolve(a.arg_array[2], reg);

if (true)
  {
    if (arg2 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<range> requires at least  arguments.")+digits(2)+1));
        sprintf(err_msg, "<range> requires at least %d arguments.", 2);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & Integer)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <range> should be of type Integer.")+digits(2)+1));
    sprintf(err_msg, "Argument %d of <range> should be of type Integer.", 2);
    do_error(err_msg, reg->task);
    free(err_msg);
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
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<range> requires at least  arguments.")+digits(3)+1));
        sprintf(err_msg, "<range> requires at least %d arguments.", 3);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg3 != NULL && true && (!(arg3->type & Integer)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <range> should be of type Integer.")+digits(3)+1));
    sprintf(err_msg, "Argument %d of <range> should be of type Integer.", 3);
    do_error(err_msg, reg->task);
    free(err_msg);
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
    (reg,new_hash_size(mpz_get_ui(elements)), reg->task);

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
  registry* r_new = new_registry(reg, new_hash_size(a.length), reg->task);
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
      
      
      check_length(&a, 2+1, "registry", reg->task);
if (is_error(-1, reg->task)) return ;;
    }
  
  registry* r_new = new_registry(reg, new_hash_size(a.length / 2 + 1),
                                 reg->task);
  data* d = NULL;
  data* d_data = NULL;
  data* d_new;

  for (int i = 1; i < a.length; i = i + 2)
    {
      d = a.arg_array[i];
      
      if (d->type != Register)
        {
          do_error("Expected a register", reg->task);
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
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<arithmetic> requires at least  arguments.")+digits(i)+1));
        sprintf(err_msg, "<arithmetic> requires at least %d arguments.", i);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (argi != NULL && true && (!(argi->type & (Real|Integer))))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <arithmetic> should be of type (Real|Integer).")+digits(i)+1));
    sprintf(err_msg, "Argument %d of <arithmetic> should be of type (Real|Integer).", i);
    do_error(err_msg, reg->task);
    free(err_msg);
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
  
  
  check_length(&a, 2+1, "add", reg->task);
if (is_error(-1, reg->task)) return ;;

  op_arithmetic(a, reg, 1);
}

void
op_mul (arg a, registry* reg)
{
  
  
  check_length(&a, 2+1, "mul", reg->task);
if (is_error(-1, reg->task)) return ;;

  op_arithmetic(a, reg, 2);
}

void
op_sub (arg a, registry* reg)
{
  
  
  check_length(&a, 2+1, "sub", reg->task);
if (is_error(-1, reg->task)) return ;;

  op_arithmetic(a, reg, 3);
}

void
op_div (arg a, registry* reg)
{
  
  
  check_length(&a, 2+1, "div", reg->task);
if (is_error(-1, reg->task)) return ;;

  op_arithmetic(a, reg, 4);
}

void
op_set (arg a, registry* reg)
{
  

  
  check_length(&a, 2+1, "set", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<set> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<set> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Register)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <set> should be of type Register.")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <set> should be of type Register.", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
    return ;
  }

;

  
  
  
data* arg2 = resolve(a.arg_array[2], reg);

if (true)
  {
    if (arg2 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<set> requires at least  arguments.")+digits(2)+1));
        sprintf(err_msg, "<set> requires at least %d arguments.", 2);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg2 != NULL && false && (!(arg2->type & Register)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <set> should be of type Register.")+digits(2)+1));
    sprintf(err_msg, "Argument %d of <set> should be of type Register.", 2);
    do_error(err_msg, reg->task);
    free(err_msg);
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
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<set> requires at least  arguments.")+digits(3)+1));
        sprintf(err_msg, "<set> requires at least %d arguments.", 3);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg3 != NULL && true && (!(arg3->type & Registry)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <set> should be of type Registry.")+digits(3)+1));
    sprintf(err_msg, "Argument %d of <set> should be of type Registry.", 3);
    do_error(err_msg, reg->task);
    free(err_msg);
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
  
  
  check_length(&a, 1+1, "get", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<get> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<get> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Register)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <get> should be of type Register.")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <get> should be of type Register.", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
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
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<get> requires at least  arguments.")+digits(2)+1));
        sprintf(err_msg, "<get> requires at least %d arguments.", 2);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & Registry)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <get> should be of type Registry.")+digits(2)+1));
    sprintf(err_msg, "Argument %d of <get> should be of type Registry.", 2);
    do_error(err_msg, reg->task);
    free(err_msg);
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
  
  
  check_length(&a, 1+1, "if", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<if> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<if> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Boolean)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <if> should be of type Boolean.")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <if> should be of type Boolean.", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
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
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<if> requires at least  arguments.")+digits(2)+1));
        sprintf(err_msg, "<if> requires at least %d arguments.", 2);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg2 != NULL && false && (!(arg2->type & Boolean)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <if> should be of type Boolean.")+digits(2)+1));
    sprintf(err_msg, "Argument %d of <if> should be of type Boolean.", 2);
    do_error(err_msg, reg->task);
    free(err_msg);
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
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<if> requires at least  arguments.")+digits(3)+1));
        sprintf(err_msg, "<if> requires at least %d arguments.", 3);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg3 != NULL && false && (!(arg3->type & Boolean)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <if> should be of type Boolean.")+digits(3)+1));
    sprintf(err_msg, "Argument %d of <if> should be of type Boolean.", 3);
    do_error(err_msg, reg->task);
    free(err_msg);
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
  
  
  check_length(&a, 2+1, "move", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<move> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<move> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Register)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <move> should be of type Register.")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <move> should be of type Register.", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
    return ;
  }

;

  
  
data* arg2 = resolve(a.arg_array[2], reg);

if (true)
  {
    if (arg2 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<move> requires at least  arguments.")+digits(2)+1));
        sprintf(err_msg, "<move> requires at least %d arguments.", 2);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & Register)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <move> should be of type Register.")+digits(2)+1));
    sprintf(err_msg, "Argument %d of <move> should be of type Register.", 2);
    do_error(err_msg, reg->task);
    free(err_msg);
    return ;
  }

;

  mov(reg, (regstr*) arg1->data,
      (regstr*) arg2->data);
  
}

void
op_delete (arg a, registry* reg)
{
  
  
  check_length(&a, 1+1, "delete", reg->task);
if (is_error(-1, reg->task)) return ;;
  
  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<delete> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<delete> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Register)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <delete> should be of type Register.")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <delete> should be of type Register.", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
    return ;
  }

;
  
  del(reg, ((regstr*) arg1->data)->key, 1, false);
}

void
op_free (arg a, registry* reg)
{
  
  
  check_length(&a, 1+1, "free", reg->task);
if (is_error(-1, reg->task)) return ;;
  
  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<free> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<free> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Register)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <free> should be of type Register.")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <free> should be of type Register.", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
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
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<exit> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<exit> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Integer)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <exit> should be of type Integer.")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <exit> should be of type Integer.", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
    return ;
  }

;

      err = mpz_get_si(*((mpz_t*) arg1->data));
    }
  is_exit(err+1, reg->task);
  is_error(1, reg->task);
}

void
op_exist (arg a, registry* reg)
{
  
  
  check_length(&a, 1+1, "exist", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<exist> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<exist> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Register)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <exist> should be of type Register.")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <exist> should be of type Register.", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
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
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<exist> requires at least  arguments.")+digits(2)+1));
        sprintf(err_msg, "<exist> requires at least %d arguments.", 2);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & Registry)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <exist> should be of type Registry.")+digits(2)+1));
    sprintf(err_msg, "Argument %d of <exist> should be of type Registry.", 2);
    do_error(err_msg, reg->task);
    free(err_msg);
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
  
  
  check_length(&a, 1+1, "answer", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<answer> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<answer> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && false && (!(arg1->type & Registry)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <answer> should be of type Registry.")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <answer> should be of type Registry.", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
    return ;
  }

;
  

  data* d = copy_data(arg1);
  ret_ans(reg, d);
}

int
op_comparison (arg a, registry* reg)
{
  
  
  
  check_length(&a, 2+1, "comparison", reg->task);
if (is_error(-1, reg->task)) return -2;;
  

  
  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<comparison> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<comparison> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return -2;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & (Integer|Real))))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <comparison> should be of type (Integer|Real).")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <comparison> should be of type (Integer|Real).", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
    return -2;
  }

;

  
  
data* arg2 = resolve(a.arg_array[2], reg);

if (true)
  {
    if (arg2 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<comparison> requires at least  arguments.")+digits(2)+1));
        sprintf(err_msg, "<comparison> requires at least %d arguments.", 2);
        do_error(err_msg, reg->task);
        free(err_msg);
        return -2;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & (Integer|Real))))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <comparison> should be of type (Integer|Real).")+digits(2)+1));
    sprintf(err_msg, "Argument %d of <comparison> should be of type (Integer|Real).", 2);
    do_error(err_msg, reg->task);
    free(err_msg);
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

  if (is_error(-1, reg->task))
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

  if (is_error(-1, reg->task))
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

  if (is_error(-1, reg->task))
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

  if (is_error(-1, reg->task))
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

  if (is_error(-1, reg->task))
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
  
  
  check_length(&a, 1+1, "print", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<print> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<print> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && false && (!(arg1->type & (Integer|Real))))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <print> should be of type (Integer|Real).")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <print> should be of type (Integer|Real).", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
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
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<print> requires at least  arguments.")+digits(2)+1));
        sprintf(err_msg, "<print> requires at least %d arguments.", 2);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg2 != NULL && false && (!(arg2->type & Boolean)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <print> should be of type Boolean.")+digits(2)+1));
    sprintf(err_msg, "Argument %d of <print> should be of type Boolean.", 2);
    do_error(err_msg, reg->task);
    free(err_msg);
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
  
  
  check_length(&a, 1+1, "string-length", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<string-length> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<string-length> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & String)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <string-length> should be of type String.")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <string-length> should be of type String.", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
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
  
  
  check_length(&a, 2+1, "string-append", reg->task);
if (is_error(-1, reg->task)) return ;;

  size_t sz = 0;
  data* args[a.length-1];
  for (int i = 1; i < a.length; i++)
    {
      
      
      
data* argi = resolve(a.arg_array[i], reg);

if (true)
  {
    if (argi == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<string-append> requires at least  arguments.")+digits(i)+1));
        sprintf(err_msg, "<string-append> requires at least %d arguments.", i);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (argi != NULL && true && (!(argi->type & String)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <string-append> should be of type String.")+digits(i)+1));
    sprintf(err_msg, "Argument %d of <string-append> should be of type String.", i);
    do_error(err_msg, reg->task);
    free(err_msg);
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
  
  
  check_length(&a, 1+1, "source", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<source> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<source> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & String)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <source> should be of type String.")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <source> should be of type String.", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
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
      do_error(msg, reg->task);
      free(msg);
      return;
    }

  struct parser_state state = fresh_state(0);
  interact(f, &state, reg);
  free_state(&state);
  fclose(f);

}

void
op_do (arg a, registry* reg)
{
  
  
  check_length(&a, 2+1, "do", reg->task);
if (is_error(-1, reg->task)) return ;;

  data* arg1 = resolve(a.arg_array[1], reg);
  data** arg_registries = malloc(sizeof(data*)*
                                 (a.length-2));
  for (int i = 0; i < (a.length - 2); i++)
    {
      arg_registries[i] = resolve(a.arg_array[i+2], reg);
    }

  if (arg1 == NULL)
    {
      do_error("<do> requires an argument.", reg->task);
      free(arg_registries);
      return;
    }

  for (int i = 0; i < (a.length-2); i++)
    {
      if (arg_registries[i] == NULL)
        {
          do_error("<do> requires more than one argument.", reg->task);
          free(arg_registries);
          return;
        }
    }
  

  if (arg1->type != Operation)
    {
      do_error("First argument to <do> must be an operation.", reg->task);
      return;
    }

  for (int i = 0; i < (a.length - 2); i++)
    {
      if (arg_registries[i]->type != Registry)
        {
          do_error("Arguments to <do> must be registries.", reg->task);
          free(arg_registries);
          return;
        }
    }

  registry** arg_reg = malloc(sizeof(registry*)*(a.length-2));
  for (int i = 0; i < (a.length - 2); i++)
    {
      arg_reg[i] = (registry*) arg_registries[i]->data;
    }
  registry* ret_reg = new_registry(reg, ARBEL_HASH_SIZE, reg->task);
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
              do_error("Instruction in <do> did not set /ans register.",
                       reg->task);
              break;
            }
          d = copy_data(d);
          set(ret_reg, d, c->name, 1);
          
          c = c->right;
        }

      if (d == NULL)
        break;
    }

  if (!is_error(-1, reg->task))
    {
      d = new_data();
      d->type = Registry;
      d->data = ret_reg;
      ret_ans(reg, d);
    }
  
}

data*
register_arithmetic (regstr* rg, int diff, arbel_task* task)
{
  char* cur_name = malloc(sizeof(char)*(strlen(rg->name)+1));
  strcpy(cur_name, rg->name);

  int i = strlen(cur_name)-1;
  int j;
  while (i >= 0 && isdigit((int) cur_name[i]))
    i--;

  if (i == (strlen(cur_name)-1))
    {
      do_error("Register does not end in integer.",
               task);
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
  
  
  check_length(&a, 1+1, "next", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<next> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<next> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Register)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <next> should be of type Register.")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <next> should be of type Register.", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
    return ;
  }

;

  data* d = register_arithmetic((regstr*) arg1->data, 1, reg->task);
  if (d != NULL)
    ret_ans(reg,d);
}

void
op_previous (arg a, registry* reg)
{
  
  
  check_length(&a, 1+1, "previous", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<previous> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<previous> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Register)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <previous> should be of type Register.")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <previous> should be of type Register.", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
    return ;
  }

;

  data* d = register_arithmetic((regstr*) arg1->data, -1, reg->task);
  if (d != NULL)
    ret_ans(reg,d);
}


void
op_last (arg a, registry* reg)
{
  
  
  check_length(&a, 2+1, "last", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<last> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<last> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Registry)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <last> should be of type Registry.")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <last> should be of type Registry.", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
    return ;
  }

;

  
  
  
data* arg2 = resolve(a.arg_array[2], reg);

if (true)
  {
    if (arg2 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<last> requires at least  arguments.")+digits(2)+1));
        sprintf(err_msg, "<last> requires at least %d arguments.", 2);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & String)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <last> should be of type String.")+digits(2)+1));
    sprintf(err_msg, "Argument %d of <last> should be of type String.", 2);
    do_error(err_msg, reg->task);
    free(err_msg);
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
  
  
  check_length(&a, 2+1, "in", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<in> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<in> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Registry)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <in> should be of type Registry.")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <in> should be of type Registry.", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
    return ;
  }

;

  
  
  
data* arg2 = resolve(a.arg_array[2], reg);

if (true)
  {
    if (arg2 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<in> requires at least  arguments.")+digits(2)+1));
        sprintf(err_msg, "<in> requires at least %d arguments.", 2);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & Instruction)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <in> should be of type Instruction.")+digits(2)+1));
    sprintf(err_msg, "Argument %d of <in> should be of type Instruction.", 2);
    do_error(err_msg, reg->task);
    free(err_msg);
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
  
  
  check_length(&a, 2+1, "while", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<while> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<while> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Instruction)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <while> should be of type Instruction.")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <while> should be of type Instruction.", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
    return ;
  }

;

  
  
data* arg2 = resolve(a.arg_array[2], reg);

if (true)
  {
    if (arg2 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<while> requires at least  arguments.")+digits(2)+1));
        sprintf(err_msg, "<while> requires at least %d arguments.", 2);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & Instruction)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <while> should be of type Instruction.")+digits(2)+1));
    sprintf(err_msg, "Argument %d of <while> should be of type Instruction.", 2);
    do_error(err_msg, reg->task);
    free(err_msg);
    return ;
  }

;

  data* d;
  while (1)
    {
      execute_0(arg1, reg);
      
      if (is_error(-1, reg->task))
        break;
          
      d = get(reg, arbel_hash_ans, 0);

      if (d == NULL)
        {
          do_error("Instruction did not set *ans* to a value.",
                   reg->task);
          break;
        }

      if (d->type != Boolean)
        {
          do_error("First instruction should set *ans* to a Boolean.",
                   reg->task);
          break;
        }

      if (!(*(bool*) d->data))
        {
          break;
        }

      execute_0(arg2, reg);
      if (is_error(-1, reg->task))
        break;

    }

}

void
op_repeat (arg a, registry* reg)
{
  
  
  check_length(&a, 2+1, "repeat", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<repeat> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<repeat> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Integer)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <repeat> should be of type Integer.")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <repeat> should be of type Integer.", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
    return ;
  }

;

  
  
  
data* arg2 = resolve(a.arg_array[2], reg);

if (true)
  {
    if (arg2 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<repeat> requires at least  arguments.")+digits(2)+1));
        sprintf(err_msg, "<repeat> requires at least %d arguments.", 2);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & Instruction)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <repeat> should be of type Instruction.")+digits(2)+1));
    sprintf(err_msg, "Argument %d of <repeat> should be of type Instruction.", 2);
    do_error(err_msg, reg->task);
    free(err_msg);
    return ;
  }

;

  mpz_t i;
  mpz_init_set_si(i, 0);
  while (mpz_cmp(i, *((mpz_t*) arg1->data)) < 0)
    {
      execute_0(arg2, reg);
      if (is_error(-1, reg->task)) break;
      mpz_add_ui(i, i, 1);
    }
}

void
op_to_register (arg a, registry* reg)
{
  
  
  check_length(&a, 1+1, "to-register", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<to-register> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<to-register> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & (String|Integer))))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <to-register> should be of type (String|Integer).")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <to-register> should be of type (String|Integer).", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
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
  
  
  check_length(&a, 2+1, "collapse", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<collapse> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<collapse> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Operation)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <collapse> should be of type Operation.")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <collapse> should be of type Operation.", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
    return ;
  }

;

  
  
  
data* arg2 = resolve(a.arg_array[2], reg);

if (true)
  {
    if (arg2 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<collapse> requires at least  arguments.")+digits(2)+1));
        sprintf(err_msg, "<collapse> requires at least %d arguments.", 2);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & Registry)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <collapse> should be of type Registry.")+digits(2)+1));
    sprintf(err_msg, "Argument %d of <collapse> should be of type Registry.", 2);
    do_error(err_msg, reg->task);
    free(err_msg);
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
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<collapse> requires at least  arguments.")+digits(3)+1));
        sprintf(err_msg, "<collapse> requires at least %d arguments.", 3);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg3 != NULL && true && (!(arg3->type & String)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <collapse> should be of type String.")+digits(3)+1));
    sprintf(err_msg, "Argument %d of <collapse> should be of type String.", 3);
    do_error(err_msg, reg->task);
    free(err_msg);
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
  registry* r = new_registry(to_walk, ARBEL_HASH_SIZE, reg->task);

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
          do_error("Operation did not set /ans register.",
                   reg->task);
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
  
  
  check_length(&a, 2+1, "string-eq", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<string-eq> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<string-eq> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & String)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <string-eq> should be of type String.")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <string-eq> should be of type String.", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
    return ;
  }

;

  
  
  
data* arg2 = resolve(a.arg_array[2], reg);

if (true)
  {
    if (arg2 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<string-eq> requires at least  arguments.")+digits(2)+1));
        sprintf(err_msg, "<string-eq> requires at least %d arguments.", 2);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & String)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <string-eq> should be of type String.")+digits(2)+1));
    sprintf(err_msg, "Argument %d of <string-eq> should be of type String.", 2);
    do_error(err_msg, reg->task);
    free(err_msg);
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
  
  
  check_length(&a, 2+1, "string-lt", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<string-lt> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<string-lt> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & String)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <string-lt> should be of type String.")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <string-lt> should be of type String.", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
    return ;
  }

;

  
  
  
data* arg2 = resolve(a.arg_array[2], reg);

if (true)
  {
    if (arg2 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<string-lt> requires at least  arguments.")+digits(2)+1));
        sprintf(err_msg, "<string-lt> requires at least %d arguments.", 2);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & String)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <string-lt> should be of type String.")+digits(2)+1));
    sprintf(err_msg, "Argument %d of <string-lt> should be of type String.", 2);
    do_error(err_msg, reg->task);
    free(err_msg);
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
  
  
  check_length(&a, 2+1, "string-gt", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<string-gt> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<string-gt> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & String)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <string-gt> should be of type String.")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <string-gt> should be of type String.", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
    return ;
  }

;

  
  
  
data* arg2 = resolve(a.arg_array[2], reg);

if (true)
  {
    if (arg2 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<string-gt> requires at least  arguments.")+digits(2)+1));
        sprintf(err_msg, "<string-gt> requires at least %d arguments.", 2);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & String)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <string-gt> should be of type String.")+digits(2)+1));
    sprintf(err_msg, "Argument %d of <string-gt> should be of type String.", 2);
    do_error(err_msg, reg->task);
    free(err_msg);
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
  
  
  check_length(&a, 2+1, "register-eq", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<register-eq> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<register-eq> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Register)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <register-eq> should be of type Register.")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <register-eq> should be of type Register.", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
    return ;
  }

;

  
  
  
data* arg2 = resolve(a.arg_array[2], reg);

if (true)
  {
    if (arg2 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<register-eq> requires at least  arguments.")+digits(2)+1));
        sprintf(err_msg, "<register-eq> requires at least %d arguments.", 2);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & Register)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <register-eq> should be of type Register.")+digits(2)+1));
    sprintf(err_msg, "Argument %d of <register-eq> should be of type Register.", 2);
    do_error(err_msg, reg->task);
    free(err_msg);
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
  
  
  check_length(&a, 1+1, "go-in", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<go-in> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<go-in> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Registry)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <go-in> should be of type Registry.")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <go-in> should be of type Registry.", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
    return ;
  }

;

  ((registry*) arg1->data)->up = reg;

  reg->task->current_parse_registry = (registry*) arg1->data;
  
}

void
op_go_out (arg a, registry* reg)
{
  if (reg->task->current_parse_registry->up == NULL)
    {
      do_error("Already at top level registry.",
               reg->task);
      return;
    }

  reg->task->current_parse_registry = reg->task->current_parse_registry->up;
}

void
op_save (arg a, registry* reg)
{
    
  
  check_length(&a, 1+1, "save", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<save> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<save> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & String)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <save> should be of type String.")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <save> should be of type String.", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
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
  
  
  check_length(&a, 1+1, "load", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<load> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<load> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & String)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <load> should be of type String.")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <load> should be of type String.", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
    return ;
  }

;

  char* fname = (char*) arg1->data;
  FILE* f = fopen(fname, "rb");
  if (f == NULL)
    {
      do_error("File cannot be opened.",
               reg->task);
      return;
    }
  if (a.length >= 3)
    {
      
      
      
data* arg2 = resolve(a.arg_array[2], reg);

if (true)
  {
    if (arg2 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<load> requires at least  arguments.")+digits(2)+1));
        sprintf(err_msg, "<load> requires at least %d arguments.", 2);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & Registry)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <load> should be of type Registry.")+digits(2)+1));
    sprintf(err_msg, "Argument %d of <load> should be of type Registry.", 2);
    do_error(err_msg, reg->task);
    free(err_msg);
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
  
  
  check_length(&a, 1+1, "to-string", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<to-string> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<to-string> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & (Integer | Real | Register))))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <to-string> should be of type (Integer | Real | Register).")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <to-string> should be of type (Integer | Real | Register).", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
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
          do_error("The second argument to *to-string* should be an integer.",
                   reg->task);
          return;
        }
      if (arg2 != NULL)
        {
          prec = *((int*) arg2->data);
          if (prec < 0)
            {
              do_error("The second argument to *to-string* must be non-negative.",
                       reg->task);
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
  
  
  check_length(&a, 1+1, "to-number", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<to-number> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<to-number> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & (String|Register))))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <to-number> should be of type (String|Register).")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <to-number> should be of type (String|Register).", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
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
          do_error("String not a number.", reg->task);
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
          do_error("Register does not end in a number.", reg->task);
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
  
  
  check_length(&a, 1+1, "to-real", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<to-real> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<to-real> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & (Integer|Real|String))))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <to-real> should be of type (Integer|Real|String).")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <to-real> should be of type (Integer|Real|String).", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
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
  
  
  check_length(&a, 1+1, "output-code", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<output-code> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<output-code> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & String)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <output-code> should be of type String.")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <output-code> should be of type String.", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
    return ;
  }

;

  if (reg->task->source_code != NULL)
    {
      FILE* f = fopen((char*) arg1->data, "w");
      fwrite(reg->task->source_code, sizeof(char),
             strlen(reg->task->source_code), f);
      fclose(f);
    }
  else
    {
      do_error("No source saved yet.", reg->task);
      return;
    }
}

void
op_clear_code (arg a, registry* reg)
{
  if (reg->task->source_code != NULL)
    free(reg->task->source_code);

  reg->task->source_code = NULL;
}

void
op_error (arg a, registry* reg)
{
  
  
  check_length(&a, 1+1, "error", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<error> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<error> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & String)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <error> should be of type String.")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <error> should be of type String.", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
    return ;
  }

;

  do_error((char*) arg1->data, reg->task);

  if (a.length >= 3)
    {
      
      
      
      
data* arg2 = resolve(a.arg_array[2], reg);

if (false)
  {
    if (arg2 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<error> requires at least  arguments.")+digits(2)+1));
        sprintf(err_msg, "<error> requires at least %d arguments.", 2);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & Integer)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <error> should be of type Integer.")+digits(2)+1));
    sprintf(err_msg, "Argument %d of <error> should be of type Integer.", 2);
    do_error(err_msg, reg->task);
    free(err_msg);
    return ;
  }

;
      
      if (arg2 != NULL)
        {
          is_error(mpz_get_si(*((mpz_t*) arg2->data)), reg->task);
        }
    }
}

void
op_is_type (arg a, registry* reg, const data_type type)
{
  
  
  check_length(&a, 1+1, "is-Type", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<is-Type> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<is-Type> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && false && (!(arg1->type & Integer)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <is-Type> should be of type Integer.")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <is-Type> should be of type Integer.", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
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
op_open_file (arg a, registry* reg)
{
  
  
  check_length(&a, 1+1, "open-file", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<open-file> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<open-file> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & String)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <open-file> should be of type String.")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <open-file> should be of type String.", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
    return ;
  }

;

  FILE* f;
  if (a.length >= 3)
    {
      
      
      
data* arg2 = resolve(a.arg_array[2], reg);

if (true)
  {
    if (arg2 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<open-file> requires at least  arguments.")+digits(2)+1));
        sprintf(err_msg, "<open-file> requires at least %d arguments.", 2);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & String)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <open-file> should be of type String.")+digits(2)+1));
    sprintf(err_msg, "Argument %d of <open-file> should be of type String.", 2);
    do_error(err_msg, reg->task);
    free(err_msg);
    return ;
  }

;
  
      f = fopen((char*) arg1->data, (char*) arg2->data);
    }
  else
    {
      f = fopen((char*) arg1->data, "r+");
    }
  
  if (f == NULL)
    {
      do_error("File did not open.  Possibly, it does not exist.", reg->task);
      return;
    }

  data* d;

  assign_file(&d,f);
  ret_ans(reg,d);

}

void
op_read (arg a, registry* reg)
{
  
  
  check_length(&a, 1+1, "read", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<read> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<read> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & File)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <read> should be of type File.")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <read> should be of type File.", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
    return ;
  }

;

  int c = fgetc((FILE*) arg1->data);
  data* d;

  if (c == EOF || c == '\0')
    {
      assign_nothing(&d);
    }
  else
    {
      mpz_t m;
      mpz_init_set_si(m, c);
      assign_int(&d, m);
    }
  ret_ans(reg, d);
}

void
op_read_char (arg a, registry* reg)
{
    
  
  check_length(&a, 1+1, "read-char", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<read-char> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<read-char> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & File)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <read-char> should be of type File.")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <read-char> should be of type File.", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
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
  
  
  check_length(&a, 1+1, "close", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<close> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<close> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Register)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <close> should be of type Register.")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <close> should be of type Register.", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
    return ;
  }

;

  data* f = lookup(reg, ((regstr*) arg1->data)->key, 0);

  if (f == NULL)
    {
      do_error("Register does not exist.", reg->task);
      return;
    }

  if (f->type != File)
    {
      do_error("Register does not contain a file.", reg->task);
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
  
  
  check_length(&a, 2+1, "or", reg->task);
if (is_error(-1, reg->task)) return ;;

  bool check = false;
  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<or> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<or> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Boolean)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <or> should be of type Boolean.")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <or> should be of type Boolean.", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
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
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<or> requires at least  arguments.")+digits(2)+1));
        sprintf(err_msg, "<or> requires at least %d arguments.", 2);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & Boolean)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <or> should be of type Boolean.")+digits(2)+1));
    sprintf(err_msg, "Argument %d of <or> should be of type Boolean.", 2);
    do_error(err_msg, reg->task);
    free(err_msg);
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
  
  
  check_length(&a, 2+1, "and", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<and> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<and> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Boolean)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <and> should be of type Boolean.")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <and> should be of type Boolean.", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
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
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<and> requires at least  arguments.")+digits(2)+1));
        sprintf(err_msg, "<and> requires at least %d arguments.", 2);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & Boolean)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <and> should be of type Boolean.")+digits(2)+1));
    sprintf(err_msg, "Argument %d of <and> should be of type Boolean.", 2);
    do_error(err_msg, reg->task);
    free(err_msg);
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
  
  
  check_length(&a, 1+1, "not", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<not> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<not> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Boolean)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <not> should be of type Boolean.")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <not> should be of type Boolean.", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
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
  
  
  check_length(&a, 1+1, "read-line", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<read-line> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<read-line> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & File)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <read-line> should be of type File.")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <read-line> should be of type File.", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
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
op_write_string (arg a, registry* reg)
{
  
  
  check_length(&a, 2+1, "write-string", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<write-string> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<write-string> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & String)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <write-string> should be of type String.")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <write-string> should be of type String.", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
    return ;
  }

;

  
  
  
data* arg2 = resolve(a.arg_array[2], reg);

if (true)
  {
    if (arg2 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<write-string> requires at least  arguments.")+digits(2)+1));
        sprintf(err_msg, "<write-string> requires at least %d arguments.", 2);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & File)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <write-string> should be of type File.")+digits(2)+1));
    sprintf(err_msg, "Argument %d of <write-string> should be of type File.", 2);
    do_error(err_msg, reg->task);
    free(err_msg);
    return ;
  }

;

  fwrite((char*) arg1->data, sizeof(char), strlen((char*) arg1->data), (FILE*) arg2->data);

}

void
op_write (arg a, registry* reg)
{
  
  

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<write> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<write> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Integer)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <write> should be of type Integer.")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <write> should be of type Integer.", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
    return ;
  }

;

  
  
  
data* arg2 = resolve(a.arg_array[2], reg);

if (true)
  {
    if (arg2 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<write> requires at least  arguments.")+digits(2)+1));
        sprintf(err_msg, "<write> requires at least %d arguments.", 2);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & File)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <write> should be of type File.")+digits(2)+1));
    sprintf(err_msg, "Argument %d of <write> should be of type File.", 2);
    do_error(err_msg, reg->task);
    free(err_msg);
    return ;
  }

;

  int i = mpz_get_ui(*((mpz_t*) arg1->data));
  if ((i >= 255) || (i < 0))
    {
      do_error("Value should be in range [0,255].", reg->task);
      return;
    }
  else
    {
      char c = (char) i;
      fwrite(&c, sizeof(char), 1, (FILE*) arg2->data);
    }
}

void
op_input (arg a, registry* reg)
{
  
  check_length(&a, 1+1, "write", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<input> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<input> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Register)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <input> should be of type Register.")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <input> should be of type Register.", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
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

  
  
  check_length(&a, 1+1, "shell", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<shell> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<shell> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & String)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <shell> should be of type String.")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <shell> should be of type String.", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
    return ;
  }

;

  FILE* f = popen((char*) arg1->data, "r");
  
  if (f == NULL)
    {
      do_error("Command failed.", reg->task);
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
  
  
  check_length(&a, 3+1, "link", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<link> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<link> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & String)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <link> should be of type String.")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <link> should be of type String.", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
    return ;
  }

;

  
  
data* arg2 = resolve(a.arg_array[2], reg);

if (true)
  {
    if (arg2 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<link> requires at least  arguments.")+digits(2)+1));
        sprintf(err_msg, "<link> requires at least %d arguments.", 2);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & String)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <link> should be of type String.")+digits(2)+1));
    sprintf(err_msg, "Argument %d of <link> should be of type String.", 2);
    do_error(err_msg, reg->task);
    free(err_msg);
    return ;
  }

;

  
  
data* arg3 = resolve(a.arg_array[3], reg);

if (true)
  {
    if (arg3 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<link> requires at least  arguments.")+digits(3)+1));
        sprintf(err_msg, "<link> requires at least %d arguments.", 3);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg3 != NULL && true && (!(arg3->type & String)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <link> should be of type String.")+digits(3)+1));
    sprintf(err_msg, "Argument %d of <link> should be of type String.", 3);
    do_error(err_msg, reg->task);
    free(err_msg);
    return ;
  }

;

  void* lib = dlopen((char*) arg1->data, RTLD_LAZY);

  if (lib == NULL)
    {
      printf("%s\n", dlerror());
      do_error("Library failed to open.", reg->task);
      return;
    }

  operation new_op = dlsym(lib, (char*) arg2->data);
  if (new_op == NULL)
    {
      do_error("Error loading function.", reg->task);
      dlclose(lib);
      return;
    }

  data* d;
  assign_op(&d, new_op, NULL, NULL, 0);
  set(reg, d, (char*) arg3->data, 1);

  if (reg->task->arbel_ll == NULL)
    {
      
      reg->task->arbel_ll = malloc(sizeof(void*));
    }
  else
    {
      reg->task->arbel_ll = realloc(reg->task->arbel_ll,
                                    sizeof(void*)*(reg->task->arbel_ll_cnt+1));
    }
  reg->task->arbel_ll[reg->task->arbel_ll_cnt] = lib;
  reg->task->arbel_ll_cnt++;

}

void
op_match (arg a, registry* reg)
{
  
  
  check_length(&a, 2+1, "match", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<match> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<match> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & String)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <match> should be of type String.")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <match> should be of type String.", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
    return ;
  }

;

  
  
data* arg2 = resolve(a.arg_array[2], reg);

if (true)
  {
    if (arg2 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<match> requires at least  arguments.")+digits(2)+1));
        sprintf(err_msg, "<match> requires at least %d arguments.", 2);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & String)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <match> should be of type String.")+digits(2)+1));
    sprintf(err_msg, "Argument %d of <match> should be of type String.", 2);
    do_error(err_msg, reg->task);
    free(err_msg);
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
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<match> requires at least  arguments.")+digits(3)+1));
        sprintf(err_msg, "<match> requires at least %d arguments.", 3);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg3 != NULL && true && (!(arg3->type & Integer)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <match> should be of type Integer.")+digits(3)+1));
    sprintf(err_msg, "Argument %d of <match> should be of type Integer.", 3);
    do_error(err_msg, reg->task);
    free(err_msg);
    return ;
  }

;
      max_matches = mpz_get_ui(*((mpz_t*) arg3->data));
    }

  regex_t regex;
  int error = regcomp(&regex, (char*) arg1->data, REG_EXTENDED);
  if (error)
    {
      do_error("Error compiling regular expression.", reg->task);
      return;
    }

  size_t n_groups = 10*(regex.re_nsub+1);
  regmatch_t* matches;


  data* d;
  assign_registry(&d, NULL, true, reg->task);
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
          assign_registry(&d_reg, NULL, true, reg->task);
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
  
  
  check_length(&a, 3+1, "replace", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<replace> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<replace> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & String)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <replace> should be of type String.")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <replace> should be of type String.", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
    return ;
  }

;

  
  
data* arg2 = resolve(a.arg_array[2], reg);

if (true)
  {
    if (arg2 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<replace> requires at least  arguments.")+digits(2)+1));
        sprintf(err_msg, "<replace> requires at least %d arguments.", 2);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & String)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <replace> should be of type String.")+digits(2)+1));
    sprintf(err_msg, "Argument %d of <replace> should be of type String.", 2);
    do_error(err_msg, reg->task);
    free(err_msg);
    return ;
  }

;

  
  
data* arg3 = resolve(a.arg_array[3], reg);

if (true)
  {
    if (arg3 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<replace> requires at least  arguments.")+digits(3)+1));
        sprintf(err_msg, "<replace> requires at least %d arguments.", 3);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg3 != NULL && true && (!(arg3->type & String)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <replace> should be of type String.")+digits(3)+1));
    sprintf(err_msg, "Argument %d of <replace> should be of type String.", 3);
    do_error(err_msg, reg->task);
    free(err_msg);
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
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<replace> requires at least  arguments.")+digits(4)+1));
        sprintf(err_msg, "<replace> requires at least %d arguments.", 4);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg4 != NULL && true && (!(arg4->type & Integer)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <replace> should be of type Integer.")+digits(4)+1));
    sprintf(err_msg, "Argument %d of <replace> should be of type Integer.", 4);
    do_error(err_msg, reg->task);
    free(err_msg);
    return ;
  }

;
      max_replace = mpz_get_ui(*((mpz_t*) arg4->data));
    }

  regex_t regex;
  int error = regcomp(&regex, (char*) arg1->data, REG_EXTENDED);
  if (error)
    {
      do_error("Error compiling regular expression.", reg->task);
      return;
    }

  regmatch_t* matches = malloc(sizeof(regmatch_t));

  data* d;
  assign_registry(&d, NULL, true, reg->task);


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
  
  
  check_length(&a, 1+1, "log", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<log> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<log> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & (Integer|Real))))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <log> should be of type (Integer|Real).")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <log> should be of type (Integer|Real).", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
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
  
  
  check_length(&a, 1+1, "exp", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<exp> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<exp> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & (Integer|Real))))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <exp> should be of type (Integer|Real).")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <exp> should be of type (Integer|Real).", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
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
  
  
  check_length(&a, 2+1, "power", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<power> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<power> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & (Integer|Real))))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <power> should be of type (Integer|Real).")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <power> should be of type (Integer|Real).", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
    return ;
  }

;

  
  
data* arg2 = resolve(a.arg_array[2], reg);

if (true)
  {
    if (arg2 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<power> requires at least  arguments.")+digits(2)+1));
        sprintf(err_msg, "<power> requires at least %d arguments.", 2);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & (Integer|Real))))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <power> should be of type (Integer|Real).")+digits(2)+1));
    sprintf(err_msg, "Argument %d of <power> should be of type (Integer|Real).", 2);
    do_error(err_msg, reg->task);
    free(err_msg);
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
  
  
  check_length(&a, 1+1, "change-dir", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<change-dir> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<change-dir> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & String)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <change-dir> should be of type String.")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <change-dir> should be of type String.", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
    return ;
  }

;

  int error = chdir((char*) arg1->data);
  if (error)
    {
      do_error("Could not change directory.", reg->task);
      return;
    }

}


void
op_import (arg a, registry* reg)
{
  
  
  check_length(&a, 1+1, "import", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<import> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<import> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Registry)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <import> should be of type Registry.")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <import> should be of type Registry.", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
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
      do_error("Error getting current directory.", reg->task);
      return;
    }
  data* d;
  assign_str(&d, dir, 0);
  ret_ans(reg,d);
}

void
op_substring (arg a, registry* reg)
{
  
  
  check_length(&a, 3+1, "substring", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<substring> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<substring> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & String)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <substring> should be of type String.")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <substring> should be of type String.", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
    return ;
  }

;

  
  
  
data* arg2 = resolve(a.arg_array[2], reg);

if (true)
  {
    if (arg2 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<substring> requires at least  arguments.")+digits(2)+1));
        sprintf(err_msg, "<substring> requires at least %d arguments.", 2);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & Integer)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <substring> should be of type Integer.")+digits(2)+1));
    sprintf(err_msg, "Argument %d of <substring> should be of type Integer.", 2);
    do_error(err_msg, reg->task);
    free(err_msg);
    return ;
  }

;

  
  
data* arg3 = resolve(a.arg_array[3], reg);

if (true)
  {
    if (arg3 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<substring> requires at least  arguments.")+digits(3)+1));
        sprintf(err_msg, "<substring> requires at least %d arguments.", 3);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg3 != NULL && true && (!(arg3->type & Integer)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <substring> should be of type Integer.")+digits(3)+1));
    sprintf(err_msg, "Argument %d of <substring> should be of type Integer.", 3);
    do_error(err_msg, reg->task);
    free(err_msg);
    return ;
  }

;

  unsigned char* str = (unsigned char*) arg1->data;
  int start = mpz_get_si(*((mpz_t*) arg2->data));
  int end = mpz_get_si(*((mpz_t*) arg3->data));

  size_t byte_length = strlen((char*) str)+1;
  size_t length = u8_mbsnlen((unsigned char*) str,
                             byte_length-1);

  start = arbel_location(start, length);
  end = arbel_location(end, length);

  if ((start > length) || (start <= 0))
    {
      do_error("Index out of range.", reg->task);
      return;
    }


  if ((end > length) || (end <= 0))
    {
      do_error("Index out of range.", reg->task);
      return;
    }

  if (start > end)
    {
      do_error("The starting position of the substring is greater than the ending position.", reg->task);
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
  
  
  check_length(&a, 1+1, "up", reg->task);
if (is_error(-1, reg->task)) return ;;

  if (reg->up == NULL)
    {
      do_error("Cannot use *up* instruction at top-level registry.",
               reg->task);
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
    
  
  check_length(&a, 2+1, "of", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<of> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<of> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & String)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <of> should be of type String.")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <of> should be of type String.", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
    return ;
  }

;

  
  
  
data* arg2 = resolve(a.arg_array[2], reg);

if (true)
  {
    if (arg2 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<of> requires at least  arguments.")+digits(2)+1));
        sprintf(err_msg, "<of> requires at least %d arguments.", 2);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & Registry)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <of> should be of type Registry.")+digits(2)+1));
    sprintf(err_msg, "Argument %d of <of> should be of type Registry.", 2);
    do_error(err_msg, reg->task);
    free(err_msg);
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
  
  
  check_length(&a, 2+1, "is-of", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<is-of> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<is-of> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & String)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <is-of> should be of type String.")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <is-of> should be of type String.", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
    return ;
  }

;

  
  
  
data* arg2 = resolve(a.arg_array[2], reg);

if (true)
  {
    if (arg2 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<is-of> requires at least  arguments.")+digits(2)+1));
        sprintf(err_msg, "<is-of> requires at least %d arguments.", 2);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & Registry)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <is-of> should be of type Registry.")+digits(2)+1));
    sprintf(err_msg, "Argument %d of <is-of> should be of type Registry.", 2);
    do_error(err_msg, reg->task);
    free(err_msg);
    return ;
  }

;

  data* d = lookup((registry*) arg2->data, arbel_hash_class, 0);

  if (d == NULL || d->type != String)
    {
      do_error("*class* register not found.", reg->task);
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
  
  
  check_length(&a, 2+1, "dispatch", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<dispatch> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<dispatch> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & String)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <dispatch> should be of type String.")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <dispatch> should be of type String.", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
    return ;
  }

;

  
  
  
data* arg2 = resolve(a.arg_array[2], reg);

if (true)
  {
    if (arg2 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<dispatch> requires at least  arguments.")+digits(2)+1));
        sprintf(err_msg, "<dispatch> requires at least %d arguments.", 2);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg2 != NULL && false && (!(arg2->type & String)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <dispatch> should be of type String.")+digits(2)+1));
    sprintf(err_msg, "Argument %d of <dispatch> should be of type String.", 2);
    do_error(err_msg, reg->task);
    free(err_msg);
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
  
  
  check_length(&a, 1+1, "code", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<code> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<code> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Instruction)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <code> should be of type Instruction.")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <code> should be of type Instruction.", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
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
  
  
  check_length(&a, 1+1, "is-error", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<is-error> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<is-error> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Instruction)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <is-error> should be of type Instruction.")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <is-error> should be of type Instruction.", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
    return ;
  }

;

  int stop_thresh = reg->task->arbel_stop_error_threshold;
  reg->task->arbel_stop_error_threshold = 0;
  execute_0(arg1, reg);
  data* d;
  assign_boolean(&d, is_error(-1, reg->task) > 0 ? true : false);
  reg->task->arbel_stop_error_threshold = stop_thresh;

  ret_ans(reg, d);
  is_error(0, reg->task);

}


void
op_call (arg a, registry* reg)
{
  
  
  check_length(&a, 1+1, "call", reg->task);
if (is_error(-1, reg->task)) return ;;
  _op_call(a,reg,1);
}

void
op_push_through (arg a, registry* reg)
{
  
  
  check_length(&a, 1+1, "push-through", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<push-through> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<push-through> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Instruction)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <push-through> should be of type Instruction.")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <push-through> should be of type Instruction.", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
    return ;
  }

;

  int stop_thresh = reg->task->arbel_stop_error_threshold;
  reg->task->arbel_stop_error_threshold = 0;
  execute_0(arg1, reg);
  reg->task->arbel_stop_error_threshold = stop_thresh;
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
  
  
  check_length(&a, 1+1, "error-messages", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<error-messages> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<error-messages> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Boolean)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <error-messages> should be of type Boolean.")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <error-messages> should be of type Boolean.", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
    return ;
  }

;

  if (*((bool*) arg1->data))
    reg->task->arbel_print_error_messages = true;
  else
    reg->task->arbel_print_error_messages = false;

}

void
op_find (arg a, registry* reg)
{
  
  
  check_length(&a, 2+1, "find", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<find> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<find> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Registry)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <find> should be of type Registry.")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <find> should be of type Registry.", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
    return ;
  }

;

  
  
  
data* arg2 = resolve(a.arg_array[2], reg);

if (true)
  {
    if (arg2 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<find> requires at least  arguments.")+digits(2)+1));
        sprintf(err_msg, "<find> requires at least %d arguments.", 2);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & Operation)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <find> should be of type Operation.")+digits(2)+1));
    sprintf(err_msg, "Argument %d of <find> should be of type Operation.", 2);
    do_error(err_msg, reg->task);
    free(err_msg);
    return ;
  }

;

  registry* r = (registry*) arg1->data;
  registry* result = new_registry(reg, r->hash_size, reg->task);
  arg a1;
  a1.length = 3;
  a1.free_data = malloc(sizeof(int)*a1.length);
  for (int i = 0; i < a1.length; i++)
    a1.free_data[i] = 0;

  a1.arg_array = malloc(sizeof(data*)*a1.length);
  a1.arg_array[0] = arg2;
  a1.arg_array[1] = NULL;
  
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
              a1.arg_array[1] = c->value;
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
  assign_registry(&d, result, false, reg->task);
  ret_ans(reg, d);
}

void
op_please (arg a, registry* reg)
{
  

  
  check_length(&a, 2+1, "please", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<please> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<please> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Instruction)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <please> should be of type Instruction.")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <please> should be of type Instruction.", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
    return ;
  }

;

  
  
  
data* arg2 = resolve(a.arg_array[2], reg);

if (true)
  {
    if (arg2 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<please> requires at least  arguments.")+digits(2)+1));
        sprintf(err_msg, "<please> requires at least %d arguments.", 2);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & Instruction)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <please> should be of type Instruction.")+digits(2)+1));
    sprintf(err_msg, "Argument %d of <please> should be of type Instruction.", 2);
    do_error(err_msg, reg->task);
    free(err_msg);
    return ;
  }

;

  execute_0(arg1, reg);

  if (is_error(-1, reg->task))
    {
      is_error(0, reg->task);
      execute_0(arg2, reg);
    }
}

void
op_mod (arg a, registry* reg)
{

  

  
  check_length(&a, 2+1, "mod", reg->task);
if (is_error(-1, reg->task)) return ;;
  
  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<mod> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<mod> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & (Real | Integer))))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <mod> should be of type (Real | Integer).")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <mod> should be of type (Real | Integer).", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
    return ;
  }

;

  
  
  
data* arg2 = resolve(a.arg_array[2], reg);

if (true)
  {
    if (arg2 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<mod> requires at least  arguments.")+digits(2)+1));
        sprintf(err_msg, "<mod> requires at least %d arguments.", 2);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & (Real | Integer))))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <mod> should be of type (Real | Integer).")+digits(2)+1));
    sprintf(err_msg, "Argument %d of <mod> should be of type (Real | Integer).", 2);
    do_error(err_msg, reg->task);
    free(err_msg);
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
  

  
  check_length(&a, 1+1, "op", reg->task);
if (is_error(-1, reg->task)) return ;;
    
  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<op> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<op> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Instruction)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <op> should be of type Instruction.")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <op> should be of type Instruction.", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
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
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<op> requires at least  arguments.")+digits(i)+1));
        sprintf(err_msg, "<op> requires at least %d arguments.", i);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (argi != NULL && true && (!(argi->type & Register)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <op> should be of type Register.")+digits(i)+1));
    sprintf(err_msg, "Argument %d of <op> should be of type Register.", i);
    do_error(err_msg, reg->task);
    free(err_msg);
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
  
  
  check_length(&a, 1+1, "data", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<data> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<data> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & String)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <data> should be of type String.")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <data> should be of type String.", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
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
      do_error("File cannot be opened.",
               reg->task);
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
  
  
  check_length(&a, 1+1, "incr", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<incr> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<incr> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & (Integer|Real))))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <incr> should be of type (Integer|Real).")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <incr> should be of type (Integer|Real).", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
    return ;
  }

;

  if (arg1->type == Integer)
    {

      if (a.length >= 3)
        {
          
          
          
data* arg2 = resolve(a.arg_array[2], reg);

if (true)
  {
    if (arg2 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<incr> requires at least  arguments.")+digits(2)+1));
        sprintf(err_msg, "<incr> requires at least %d arguments.", 2);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & Integer)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <incr> should be of type Integer.")+digits(2)+1));
    sprintf(err_msg, "Argument %d of <incr> should be of type Integer.", 2);
    do_error(err_msg, reg->task);
    free(err_msg);
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
  else
    {
      double inc;
      if (a.length >= 3)
        {
          
          
          
data* arg2 = resolve(a.arg_array[2], reg);

if (true)
  {
    if (arg2 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<incr> requires at least  arguments.")+digits(2)+1));
        sprintf(err_msg, "<incr> requires at least %d arguments.", 2);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & (Real|Integer))))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <incr> should be of type (Real|Integer).")+digits(2)+1));
    sprintf(err_msg, "Argument %d of <incr> should be of type (Real|Integer).", 2);
    do_error(err_msg, reg->task);
    free(err_msg);
    return ;
  }

;


          if (arg2->type == Integer)
            {
              inc = mpz_get_d(*((mpz_t*) arg2->data));
            }
          else
            {
              inc = *((double*) arg2->data);
            }
        }
      else
        {
          inc = 1;
        }
      *((double*) arg1->data) += inc;
    }
}


void
op_decr (arg a, registry* reg)
{
  
  
  check_length(&a, 1+1, "decr", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<decr> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<decr> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Integer)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <decr> should be of type Integer.")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <decr> should be of type Integer.", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
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
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<decr> requires at least  arguments.")+digits(2)+1));
        sprintf(err_msg, "<decr> requires at least %d arguments.", 2);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg2 != NULL && true && (!(arg2->type & Integer)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <decr> should be of type Integer.")+digits(2)+1));
    sprintf(err_msg, "Argument %d of <decr> should be of type Integer.", 2);
    do_error(err_msg, reg->task);
    free(err_msg);
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
op_auto_rehash (arg a, registry* reg)
{
  
  
  check_length(&a, 1+1, "auto-rehash", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<auto-rehash> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<auto-rehash> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Boolean)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <auto-rehash> should be of type Boolean.")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <auto-rehash> should be of type Boolean.", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
    return ;
  }

;

  reg->task->arbel_rehash = *((bool*) arg1->data);
}

void
op_rehash (arg a, registry* reg)
{
  
  
  check_length(&a, 1+1, "rehash", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<rehash> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<rehash> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Registry)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <rehash> should be of type Registry.")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <rehash> should be of type Registry.", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
    return ;
  }

;

  rehash((registry*) arg1->data);
  
}

void
op_clock (arg a, registry* reg)
{
  
  
  check_length(&a, 0+1, "clock", reg->task);
if (is_error(-1, reg->task)) return ;;

  struct timespec spec;
  int time_since_epoch;

  clock_gettime(CLOCK_REALTIME, &spec);
  time_since_epoch = spec.tv_sec;
  
  mpz_t z;
  mpz_init_set_si(z, time_since_epoch);
  mpz_mul_si(z, z, 1000);
  mpz_add_ui(z, z, floor(spec.tv_nsec / 1000000.0));

  data* d;

  assign_int(&d, z);
  mpz_clear(z);
  ret_ans(reg, d);  
}

void
op_make_time (arg a, registry* reg)
{
  
  
  check_length(&a, 1+1, "make-time", reg->task);
if (is_error(-1, reg->task)) return ;;

  
  
  
data* arg1 = resolve(a.arg_array[1], reg);

if (true)
  {
    if (arg1 == NULL)
      {
        char* err_msg;
        err_msg = malloc(sizeof(char)*(strlen("<make-time> requires at least  arguments.")+digits(1)+1));
        sprintf(err_msg, "<make-time> requires at least %d arguments.", 1);
        do_error(err_msg, reg->task);
        free(err_msg);
        return ;
      }
  }
if (arg1 != NULL && true && (!(arg1->type & Integer)))
  {
    char* err_msg = malloc(sizeof(char)*(strlen("Argument  of <make-time> should be of type Integer.")+digits(1)+1));
    sprintf(err_msg, "Argument %d of <make-time> should be of type Integer.", 1);
    do_error(err_msg, reg->task);
    free(err_msg);
    return ;
  }

;

  mpz_t z;
  mpz_init_set(z, *((mpz_t*) arg1->data));
  mpz_fdiv_q_ui(z, z, 1000);
  time_t t = mpz_get_si(z);
  mpz_clear(z);
  struct tm* loc = localtime(&t);

  registry* r = new_registry(reg, ARBEL_HASH_SIZE, reg->task);
  data* d;


  mpz_init_set_si(z, loc->tm_sec);
  assign_int(&d, z);
  mpz_clear(z);
  set(r, d, "second", 0);

  mpz_init_set_si(z, loc->tm_min);
  assign_int(&d, z);
  mpz_clear(z);
  set(r, d, "minute", 0);
  
  mpz_init_set_si(z, loc->tm_hour);
  assign_int(&d, z);
  mpz_clear(z);
  set(r, d, "hour", 0);
  
  mpz_init_set_si(z, loc->tm_mday);
  assign_int(&d, z);
  mpz_clear(z);
  set(r, d, "day", 0);
  
  mpz_init_set_si(z, loc->tm_mon);
  assign_int(&d, z);
  mpz_clear(z);
  set(r, d, "month", 0);

  mpz_init_set_si(z, loc->tm_year);
  assign_int(&d, z);
  mpz_clear(z);
  set(r, d, "year", 0);

  mpz_init_set_si(z,loc->tm_wday);
  assign_int(&d, z);
  mpz_clear(z);
  set(r, d, "day-of-week", 0);

  assign_registry(&d, r, false, reg->task);
  ret_ans(reg, d);
  
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

  assign_op(&d, op_do, NULL, NULL, 0);
  set(reg,d,"do",1);

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
  
  assign_op(&d, op_open_file, NULL, NULL, 0);
  set(reg,d,"open-file",1);

  assign_op(&d, op_read_char, NULL, NULL, 0);
  set(reg,d,"read-char",1);

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

  assign_op(&d, op_write_string, NULL, NULL, 0);
  set(reg,d,"write-string",1);
  
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

  assign_op(&d, op_auto_rehash, NULL, NULL, 0);
  set(reg,d,"auto-rehash",1);

  assign_op(&d, op_rehash, NULL, NULL, 0);
  set(reg,d,"rehash",1);

  assign_op(&d, op_clock, NULL, NULL, 0);
  set(reg,d,"clock",1);

  assign_op(&d, op_make_time, NULL, NULL, 0);
  set(reg,d,"make-time",1);
  

}
  
