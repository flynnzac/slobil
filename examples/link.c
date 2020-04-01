/* 
   A C Function to compute log(x+1) 
*/

#include "../src/arbel.h"
#include <math.h>
void
log_plus_one (arg a, registry* reg)
{
  check_length(&a, 1, "log1");
  if (is_error(-1)) return;

  data* arg1 = resolve(a.arg_array[1], reg);

  if (arg1 == NULL)
    {
      do_error("<log1> requires an argument.");
      return;
    }

  if (arg1->type != Integer && arg1->type != Real)
    {
      do_error("Argument to <log1> must be numeric.");
      return;
    }

  double ans;
  if (arg1->type == Integer)
    {
      ans = log(*((int*) arg1->data) + 1.0);
    }
  else
    {
      ans = log(*((double*) arg1->data) + 1.0);
    }

  data* d;
  assign_real(&d, ans);
  ret_ans(reg,d);
}

