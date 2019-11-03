#include "arbel.h"

arg
gen_arg (int length, int def_free)
{
  arg a;
  a.length = length;
  a.free_data = malloc(length*sizeof(int));

  for (int i=0; i < length; i++)
    a.free_data[i] = def_free;

  return a;
}

