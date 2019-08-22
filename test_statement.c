#include "arbel.h"

int
main (int argc, char** argv)
{
  char* code = malloc(sizeof(char)*(strlen("add 1 2 . ")+1));
  strcpy(code, "add 1 2 . ");

  FILE* f = fmemopen(code, sizeof(char)*strlen(code), "r");
  parser_state state = fresh_state(0);

  int complete = 0;

  statement* s;
  complete = new_parse(f, &state, &s);

  registry* reg = new_registry(NULL);
  add_basic_ops(reg);
  execute_statement(s, reg);
  data* d = get(reg, "ans", 0);
  printf("Value: %d\n", *((int*) d->data));
  fclose(f);

  free(code);
  return 0;
}
  
