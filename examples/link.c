/* example of linking C code in.  

   Here is a log+1 function in C that can be called from ARBEL.  See
   the link.arb code for how to load it from ARBEL.
*/

#include "arbel.h"
#include <math.h>
void
log_plus_one (registry* reg)
{
  data* arg1 = lookup(reg, "#1", 0);

  if (arg1 == NULL)
    {
      do_error("`log1` requires an argument.");
      return;
    }

  if (arg1->type != INTEGER && arg1->type != DECIMAL)
    {
      do_error("Argument to `log1` must be numeric.");
      return;
    }

  double ans;
  if (arg1->type == INTEGER)
    {
      ans = log(*((int*) arg1->data) + 1.0);
    }
  else
    {
      ans = log(*((double*) arg1->data) + 1.0);
    }

  data* d;
  assign_dec(&d, ans);
  ret_ans(reg,d);
}

