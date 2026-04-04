#include "slobil.h"

void*
mp_realloc_func (void* data, size_t old_size, size_t new_size)
{
  return GC_realloc(data, new_size);
}

void
mp_free (void* data, size_t size)
{
  GC_free(data);
}


void
init_slobil(task** task0, struct parser_state* state)
{
#ifdef GARBAGE
  GC_INIT();
  mp_set_memory_functions(GC_malloc, mp_realloc_func, mp_free);
#endif
  srand((unsigned) time(NULL));

  *task0 = malloc(sizeof(task));
  (*task0)->task = new_task(*task0);
  (*task0)->state = NULL;
  (*task0)->code = NULL;
  (*task0)->queued_instruction = NULL;
  (*task0)->pid = 0;
  (*task0)->task->reading = true;

  *state = fresh_state(0);
  
}

int
slobil_send_stream (FILE* f, struct parser_state* state, task* task)
{
  return interact(f, state, task->task->current_parse_object);
}

int
slobil_send_string (char* str, struct parser_state* state, task* task)
{
  FILE* f;
  int complete;
  
  f = fmemopen(str, strlen(str), "r");
  complete = interact(f, state, task->task->current_parse_object);
  fclose(f);

  return complete;
}

void
clean_up_slobil (task* task0, struct parser_state* state)
{
  free_task(task0);
  free_state(state);
}
