/* 
   A C Function to compute log(x+1) 
*/

/* Change to wherever arbel.h is located or just "arbel.h" if it is on the path */
#include "../src/arbel.h"
#include <math.h>

void
log_plus_one (arg a, registry* reg)
{
  
  check_length(&a, 2, "log1", reg->task->task);
  if (is_error(-1, reg->task->task)) return;

  data* arg1 = resolve(a.arg_array[1], reg);

  if (arg1 == NULL)
    {
      do_error("<log1> requires an argument.", reg->task->task);
      return;
    }

  if (arg1->type != Integer && arg1->type != Real)
    {
      do_error("Argument to <log1> must be numeric.", reg->task->task);
      return;
    }

  double ans;
  if (arg1->type == Integer)
    {
      double z = mpz_get_d(*((mpz_t*) arg1->data));
      ans = log(z + 1.0);
    }
  else
    {
      ans = log(*((double*) arg1->data) + 1.0);
    }

  data* d;
  assign_real(&d, ans);
  ret_ans(reg,d);
}

