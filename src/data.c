#include "arbel.h"

data*
new_data ()
{
  data* d = malloc(sizeof(data));
  d->data = NULL;
  d->document = NULL;
  d->type = Nothing;
  return d;
}
