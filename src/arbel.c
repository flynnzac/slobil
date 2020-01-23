/* 
   ARBEL is a REGISTER BASED ENVIRONMENT AND LANGUAGE
   Copyright 2019 Zach Flynn <zlflynn@gmail.com>

   This file is part of ARBEL.

   ARBEL is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   ARBLE is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with ARBEL (in COPYING file).  If not, see <https://www.gnu.org/licenses/>.
   
*/

#include "arbel.h"
#include <unistd.h>
#include <signal.h>

int
dummy_event ()
{
  return 0;
}

void
interrupt_handler (int status)
{
  is_error(2);
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
main (int argc, char** argv)
{
  #ifdef GARBAGE
  GC_INIT();
  #endif
  source_code = NULL;
  rl_event_hook = dummy_event;
  signal(SIGINT, interrupt_handler);
  registry* reg = new_registry(NULL, ARBEL_HASH_SIZE);
  add_basic_ops(reg);
  is_exit(0);
  current_parse_registry = reg;

  arbel_stop_error_threshold = 1;
  arbel_print_error_messages = true;
  
  arbel_hash_ans = hash_str("ans");
  arbel_hash_0 = hash_str("#0");
  arbel_hash_1 = hash_str("#1");
  arbel_hash_2 = hash_str("#2");
  arbel_hash_3 = hash_str("#3");
  arbel_hash_4 = hash_str("#4");
  arbel_hash_data = hash_str("data");
  arbel_hash_class = hash_str("--of");

  arbel_error = 0;

  arbel_ll = NULL;
  arbel_ll_cnt = 0;

  last_ans = NULL;

  char* code = NULL;
  char* prompt = "... ";
  FILE* f;
  int complete = 1;
  struct parser_state state = fresh_state(0);

  int k;
  int echo = 1;
  char* script = NULL;
  int save_code = 1;
  while ((k = getopt(argc, argv, "l:s:nmvc:")) != -1)
    {
      switch (k)
        {
	case 'c':
	  state.in_comment = 1;
	  is_exit(1);
	  script = malloc(sizeof(char)*(strlen(optarg)+1));
          strcpy(script, optarg);
          save_code = 0;
	  break;
        case 'n':
          is_exit(1);
          break;
        case 'l':
          f = fopen(optarg, "r");
          complete = interact(f, &state, current_parse_registry);
          fclose(f);
          break;
        case 's':
          is_exit(1);
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
	case 'v':
	  printf("%s\n", PACKAGE_STRING);
	  is_exit(1);
	  break;
        default:
          fprintf(stderr, "Option not recognized.");
          is_exit(1);
          break;
        }
    }

  if (script != NULL)
    {
      f = fopen(script, "r");
      complete = interact(f, &state, current_parse_registry);
      fclose(f);
      free(script);
    }

  state.print_out = echo;

  while (!is_exit(-1))
    {
      if (complete)
        {
          code = readline(prompt);
        }
      else
        {
          code = readline("");
        }
      add_history(code);
      code = append_nl(code);

      if (save_code)
        {
          source_code = append_to_source_code(source_code, code);
        }
      
      f = fmemopen(code, sizeof(char)*strlen(code), "r");
      complete = interact(f, &state, current_parse_registry);
      fclose(f);
      #ifdef GARBAGE
      #undef free
      #endif
      free(code);
      #ifdef GARBAGE
      #define free(x)
      #endif
      
      
      state.print_out = echo;
    }

  free_state(&state);
  free(source_code);
  free_registry(reg);

  if (arbel_ll != NULL)
    {
      int i;
      for (i=0; i < arbel_ll_cnt; i++)
        {
          dlclose(arbel_ll[i]);
        }

      free(arbel_ll);
    }

  return is_exit(-1)-1;
  
}

  

  
