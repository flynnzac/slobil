/*
  SLOBIL
  Copyright 2023 Zach Flynn

  Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

  1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

  2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

  3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS “AS IS” AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   
*/


#include "slobil.h"

task_vars*
new_task (task* t0)
{
  task_vars* t = malloc(sizeof(task_vars));
  object* reg = new_object(NULL, SLOBIL_HASH_SIZE, t0);

  is_exit(0, t);
  t->current_parse_object = reg;

  t->slobil_stop_error_threshold = 1;
  t->slobil_error = 0;
  t->slobil_ll = NULL;
  t->slobil_ll_cnt = 0;
  t->last_ans = NULL;
  t->source_code = NULL;
  t->slobil_options = new_object(NULL, SLOBIL_HASH_SIZE, t0);
  
  t->slobil_slot_ans.name = "ans";
  t->slobil_slot_ans.key = hash_str("ans");
  
  t->reading = false;
  t0->task = t;

  add_basic_ops(reg);

  return t;
  
}

char*
append_to_source_code (char* source_code, const char* new)
{
  if (source_code == NULL)
    {
      source_code = malloc(sizeof(char)*(strlen(new)+1));
      strcpy(source_code, new);
    }
  else
    {
      source_code = realloc(source_code, sizeof(char)*
                            (strlen(source_code)+strlen(new)+1));
      strcat(source_code, new);
    }

  return source_code;
}

int
input_code (task_vars* task, char* code, bool save_code,
            int echo,
            struct parser_state* state)
{
  add_history(code);
  code = append_nl(code);
  FILE* f;
  if (save_code)
    {
      task->source_code = append_to_source_code(task->source_code,
                                                code);
    }

  f = fmemopen(code, sizeof(char)*strlen(code), "r");
  task->reading = false;
  int complete = interact(f, state, task->current_parse_object);
  task->reading = true;
  fclose(f);

#ifdef GARBAGE
#undef free
#endif
  free(code);
#ifdef GARBAGE
#define free(x)
#endif

  state->print_out = echo;

  return complete;
  
}



void
run_task_socket (task_vars* task, int port, bool save_code,
                 struct parser_state* state, int echo)
{
  int sock;
  struct sockaddr_in addr;

#ifdef SO_REUSEPORT
  sock = socket(AF_INET, SOCK_STREAM, 0);

  int opt = 1;
  addr.sin_family = AF_INET;
  addr.sin_addr.s_addr = INADDR_ANY;
  addr.sin_port = htons(port);

  setsockopt(sock, SOL_SOCKET, SO_REUSEADDR | SO_REUSEPORT,
             &opt, sizeof(opt));
  bind(sock, (struct sockaddr*) &addr, sizeof(addr));
#endif

  
  size_t sz_addr = sizeof(addr);
  char* code = NULL;
  
  while (!is_exit(-1, task)) 
    {
      /* remove gc garbage collection because later code assumes readline
         string which is stdlib malloc'd */

      task->reading = false;

#ifdef GARBAGE
#undef realloc
#undef malloc
#endif

      listen(sock, 3);
      size_t sz_addr = sizeof(addr);
      int new_sock = accept(sock, (struct sockaddr*) &addr,
                            (unsigned int*) &sz_addr);
      code = malloc(sizeof(char)*1024);
      size_t i = 0;
      size_t cur_size = 1024;
      char c[2];
      read(new_sock,&c, 1);
          
      while (c[0] != EOF)
        {
          if (i >= cur_size)
            {
              code = realloc(code, sizeof(char)*(cur_size+1024));
              cur_size += 1024;
            }
          code[i] = c[0];
          i++;
          read(new_sock,&c, 1);
        }

      if (i >= cur_size)
        {
          code = realloc(code, sizeof(char)*(cur_size+1));
        }

      code[i] = '\0';
#ifdef GARBAGE
#define realloc(x,y) GC_REALLOC(x,y)
#define malloc(x) GC_MALLOC(x)
#endif

      int complete = input_code(task, code, save_code,
                                echo, state);
    }

}


void
run_task_readline (task_vars* task, bool save_code,
                   struct parser_state* state,
                   int echo)
{
  int complete = 1;
  char* code;
  char* prompt = "... ";
  FILE* f;
  while (!is_exit(-1, task))
    {
      if (complete)
        code = readline(prompt);
      else
        code = readline("");

      if (code == NULL)
        is_exit(1, task);
      else
        complete = input_code(task, code, save_code, echo, state);
    }
}

void
run_task (data* t)
{
  instruction* inst = (((task*) t->data)->code);
  inst->being_called = true;
  execute_code(inst->stmt, ((task*) t->data)->state);

  inst->being_called = false;
}

void*
run_task_thread (void* input)
{
  data* arg1 = (data*) input;
  task* t = (task*) arg1->data;
  pthread_mutex_lock(&t->lock);
  ((task*) arg1->data)->pid = 1;
  pthread_mutex_unlock(&t->lock);
  run_task(arg1);
  
  pthread_mutex_lock(&t->lock);
  ((task*) arg1->data)->pid = -1;
  ((task*) arg1->data)->thread = NULL;
  pthread_mutex_unlock(&t->lock);
  pthread_exit(NULL);
}

int
end_task (task_vars* t)
{
  if (t->source_code != NULL)
    free(t->source_code);

  if (t->slobil_ll != NULL)
    {
      int i;
      for (i=0; i < t->slobil_ll_cnt; i++)
        {
          dlclose(t->slobil_ll[i]);
        }

      free(t->slobil_ll);
    }

  free_object(t->current_parse_object);
  free_object(t->slobil_options);

  if (is_exit(-1, t)==0)
    return 0;
  else
    return is_exit(-1,t)-1;
  

}
