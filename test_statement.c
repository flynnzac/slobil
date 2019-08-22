#include "arbel.h"

int
main (int argc, char** argv)
{
  char* code = malloc(sizeof(char)*(strlen("add [ subtract 1 10 . ] 2 .                                ")+1));
  strcpy(code, "set $addone ( add #1 1 . ) . ");

  FILE* f = fmemopen(code, sizeof(char)*strlen(code), "r");
  parser_state state = fresh_state(0);

  int complete = 0;

  statement* s = NULL;
  complete = new_parse(f, &state, &s);
  fclose(f);
  registry* reg = new_registry(NULL);
  add_basic_ops(reg);
  execute_statement(s, reg);
  
  free(code);
  code = malloc(sizeof(char)*(strlen("addone 5 . ")+1));
  strcpy(code, "addone 5 . ");
  f = fmemopen(code, sizeof(char)*strlen(code), "r");
  state = fresh_state(0);
  s = NULL;
  new_parse(f, &state, &s);
  fclose(f);
  execute_statement(s, reg);

  /*  print_registry(reg);*/
  data* d = get(reg, "ans", 0);
  printf("Value: %d\n", *((int*) d->data));


  free(code);
  return 0;
}
  
