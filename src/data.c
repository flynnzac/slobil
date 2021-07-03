#include "wob.h"

data*
new_data ()
{
  data* d = malloc(sizeof(data));
  d->data = NULL;
  d->type = Nothing;
  return d;
}
