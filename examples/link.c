/* example of linking C code in.  

   Here is a log+1 function in C that can be called from ARBEL.  See
   the link.arb code for how to load it from ARBEL.
*/

#include "../src/arbel.h"
#include <math.h>
void
log_plus_one (arg a, registry* reg)
{
  check_length(&a, 1, "log-plus-one");
  if (is_error(-1)) return;

  data* arg1 = resolve(a.arg_array[1], reg);

  if (arg1 == NULL)
    {
      do_error("<log-plus-one> requires an argument.");
      return;
    }

  if (arg1->type != Integer && arg1->type != Real)
    {
      do_error("Argument to <log1-plus-one> must be numeric.");
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

