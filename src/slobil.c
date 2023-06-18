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
#include <unistd.h>
#include <signal.h>

/* static variables */

static task* task0;

int
dummy_event ()
{
  return 0;
}

void
interrupt_handler (int status)
{
  signal(SIGINT, interrupt_handler);
  if (task0->task->reading)
    {
      rl_replace_line("",0);
      rl_done = 1;
    }
  else
    {
      is_error(2, task0->task);
    }
}

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

int
main (int argc, char** argv)
{
#ifdef GARBAGE
  GC_INIT();
  mp_set_memory_functions(GC_malloc, mp_realloc_func, mp_free);
#endif

  srand((unsigned) time(NULL));
  rl_event_hook = dummy_event;

  task0 = malloc(sizeof(task));
  task0->task = new_task(task0);
  task0->state = NULL;
  task0->code = NULL;
  task0->queued_instruction = NULL;
  task0->pid = 0;

  task0->task->reading = true;

  FILE* f;
  int complete = 1;
  struct parser_state state = fresh_state(0);

  int k;
  int echo = 1;
  char* script = NULL;
  int save_code = 1;
  bool listen_socket = false;
  int port = 0;
  bool early_stop = false;
  while ((k = getopt(argc, argv, "l:s:nmudvc:p:b:")) != -1)
    {
      switch (k)
        {
        case 'b':
          f = fopen(optarg, "r");
          complete = interact(f, &state,
                              task0->task->current_parse_object);
          fclose(f);
          char* newname = malloc(sizeof(char)*(strlen(optarg)+strlen(".dslob")+1));
          for (int idx = 0; idx < strlen(optarg); idx++)
            {
              if (optarg[idx]=='.')
                {
                  newname[idx] = '\0';
                  break;
                }
              else
                {
                  newname[idx] = optarg[idx];
                }
              if (idx == (strlen(optarg)-1))
                {
                  newname[idx+1] = '\0';
                }
            }
          strcat(newname, ".dslob");
          save_outer(task0->task->current_parse_object, newname);
          early_stop = true;
          break;
        case 'c':
          state.in_comment = 1;
          early_stop = true;
          script = malloc(sizeof(char)*(strlen(optarg)+1));
          strcpy(script, optarg);
          save_code = 0;
          break;
        case 'n':
          early_stop = true;
          break;
        case 'l':
          f = fopen(optarg, "r");
          complete = interact(f, &state,
                              task0->task->current_parse_object);
          fclose(f);
          break;
        case 's':
          early_stop = true;
          script = malloc(sizeof(char)*(strlen(optarg)+1));
          strcpy(script, optarg);
          save_code = 0;
          break;
        case 'm':
          echo = 0;
          break;
        case 'd':
          save_code = 0;
          break;
        case 'p':
          listen_socket = true;
          port = atoi(optarg);
          if (port <= 0)
            {
              fprintf(stderr, "Invalid port: %s\n", optarg);
              is_exit(2, task0->task);
            }
          break;
        case 'v':
          printf("%s\n", PACKAGE_STRING);
          is_exit(1, task0->task);
          break;
        case 'u':
          early_stop = true;
          script = "Make.arb";
          save_code = 0;
          break;
        default:
          fprintf(stderr, "Option not recognized.\n");
          is_exit(2, task0->task);
          break;
        }
    }


  /* Read extra arguments as an object definition */
  for (; optind < (argc-1); optind = optind + 2)
    {
      if (argv[optind][0] != '/')
        {
          fprintf(stderr, "Extra arguments must be in slot-value form.");
          is_exit(2, task0->task);
          break;
        }
      else
        {
          /* For now, treat all as string.*/
          data* tmp;
          uint32_t* u32_str = slobil_u8_to_u32(argv[optind+1],
                                               strlen(argv[optind+1]));
          assign_str(&tmp, u32_str, 0);
          set(task0->task->current_parse_object,
              tmp,
              argv[optind]+1,
              1);
        }
    }

  if (script != NULL)
    {
      f = fopen(script, "r");
      complete = interact(f, &state, task0->task->current_parse_object);
      fclose(f);
      free(script);
    }

  state.print_out = echo;
#ifndef SO_REUSEPORT
  if (listen_socket)
    printf("SO_REUSEPORT not defined, unable to listen on socket, starting prompt.\n");

  listen_socket = false;
#endif
  if (!listen_socket)
    {

      signal(SIGINT, interrupt_handler);
    }


  if (!early_stop)
    {
      if (listen_socket)
        {
          run_task_socket(task0->task, port, save_code,
                          &state, echo);
        }
      else
        {
          run_task_readline(task0->task, save_code,
                            &state, echo);
        }
    }

  int retcode = end_task(task0->task);
  free_state(&state);
  
  return retcode;

}

  

  
