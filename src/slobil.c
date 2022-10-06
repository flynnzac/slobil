/* 

   SLOBIL is a Object Based Environment and Language
   Copyright 2021 Zach Flynn <zlflynn@gmail.com>


   This file is part of SLOBIL.

   SLOBIL is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   SLOBIL is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with SLOBIL (in COPYING file).  If not, see <https://www.gnu.org/licenses/>.
   
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

int
main (int argc, char** argv)
{
#ifdef GARBAGE
  GC_INIT();
#endif
#ifdef GARBAGE
  mp_set_memory_functions(GC_malloc, GC_realloc, GC_free);
#else
  mp_set_memory_functions(malloc, realloc, free);
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

  char* code = NULL;
  char* prompt = "... ";
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
          char* newname = malloc(sizeof(char)*(strlen(optarg)+strlen(".dslobil")+1));
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
          strcat(newname, ".dslobil");
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

  

  
