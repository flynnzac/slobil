/* 
   ARBEL is a REGISTER BASED ENVIRONMENT AND LANGUAGE
   Copyright 2019 Zach Flynn

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

int
parse (FILE* f, registry* reg, registry** arg_reg,
       struct parser_state* state)
{
  /* to do:
     implement [ code ] = greedy evaluation
     { code } = execute on get
  */
  char c;
  data* d;
  data* d_new;
  char* name = NULL;
  int complete = 0;
  char* code;
  size_t n_digits;

  struct parser_state sentence_parser;
  registry* sentence_registry = NULL;
  FILE* sentence_stream;

  while (((c = fgetc(f)) != EOF) && c != '\0')
    {
      if (state->in_comment && (c != '\n'))
        continue;
      if (state->in_comment && (c == '\n'))
        {
          state->in_comment = 0;
          free_registry(*arg_reg);
          *arg_reg = NULL;
          complete = 1;
          continue;
        }
      if (is_whitespace(c) && !state->in_instr && !state->in_quote)
        {
          if (strlen(state->buffer) != 0 && !state->in_quote)
            {
	      
              if (*arg_reg == NULL)
                *arg_reg = new_registry(reg);
              
              state->buffer[state->i] = '\0';
              if (state->after_instr)
                {
                  switch (state->open_paren)
                    {
                    case '(':
                      assign_instr(&d, state->buffer);
                      name = argument_name(state->arg_n);
                      set(*arg_reg, d, name);
                      free(name); 
                      state->arg_n++;
                      break;
                    case '{':
                      assign_active(&d, state->buffer);
                      name = argument_name(state->arg_n);
                      set(*arg_reg, d, name);
                      free(name);
                      state->arg_n++;
                      break;
                    case '[':
                      code = malloc(sizeof(char)*
                                    (strlen(state->buffer)+1));
                      strcpy(code, state->buffer);
                      sentence_parser = fresh_state(0);
                      sentence_stream = fmemopen(code,
                                                 sizeof(char)*
                                                 (strlen(code)+1),
                                                 "r");

                      if (parse(sentence_stream, reg, &sentence_registry,
                                &sentence_parser))
                        {
                          d = get(reg, "ans",0);
                          if (d == NULL)
                            {
                              do_error("Code in []'s did not return an answer.");
                            }
                          else
                            {
                              d = copy_data(d);
                              name = argument_name(state->arg_n);
                              set(*arg_reg, d, name);
                              free(name);
                              state->arg_n++;
                            }
                          fclose(sentence_stream);
                        }
                      else
                        {
                          do_error("Incomplete sentence in []'s.  Must complete sentence in square brackets.");
                          fclose(sentence_stream);
                        }
                      free(code);
                      if (sentence_registry != NULL)
                        {
                          free_registry(sentence_registry);
                          sentence_registry = NULL;
                        }
                      break;
                    }

                  state->open_paren = '\0';
                  state->after_instr = 0;
                }
              else if (state->after_quote)
                {
                  state->after_quote = 0;
                  assign_str(&d, state->buffer);
                  name = argument_name(state->arg_n);
                  set(*arg_reg, d, name);
                  free(name);
                  state->arg_n++;
                }
              else if (is_integer(state->buffer))
                {
                  int entry = atoi(state->buffer);
                  assign_int(&d, entry);
                  name = argument_name(state->arg_n);
                  set(*arg_reg, d, name);
                  free(name);
                  state->arg_n++;
                }
              else if (is_decimal(state->buffer) &&
                       strcmp(state->buffer, ".")!=0)
                {
                  double entry = atof(state->buffer);
                  assign_dec(&d, entry);
                  name = argument_name(state->arg_n);
                  set(*arg_reg, d, name);
                  free(name);
                  state->arg_n++;
                }
              else if (is_register(state->buffer))
                {
                  str_shift_left(state->buffer);
                  assign_regstr(&d, state->buffer);
                  name = argument_name(state->arg_n);
                  set(*arg_reg, d, name);
                  free(name);
                  state->arg_n++;
                }
              else if (strcmp(state->buffer,".")==0)
                {
                  compute (*arg_reg);
                  if (!is_error(-1))
                    {
                      if (reg->up == NULL && is_retval(-1) &&
                          state->print_out)
                        {
                          d = get(reg, "ans", 0);
                          print_data (d, 0);
                          is_retval(0);
                        }
                      complete = 1;
                      if (reg->up == NULL && state->print_out)
                        {
                          printf("OK.\n");
                        }
                      state->arg_n = 0;
                      free_registry(*arg_reg);
                      *arg_reg = NULL;
                    }
                  else
                    {
                      break;
                    }

                }
              else if (strcmp(state->buffer, "rem")==0)
                {
                  state->in_comment = 1;
                  state->arg_n = 0;
                  free_registry(*arg_reg);
                  *arg_reg = NULL;
                }
              else if (is_reference(state->buffer))
                {
                  str_shift_left(state->buffer);
                  assign_ref(&d, reg, state->buffer);
                  name = argument_name(state->arg_n);
                  set(*arg_reg, d, name);
                  free(name);
                  state->arg_n++;
                }
              else 
                {
                  d = get(reg, state->buffer, 1);

                  if (d != NULL)
                    {
                      name = argument_name(state->arg_n);
                      d_new = copy_data(d);
                      set(*arg_reg, d_new, name);
                      free(name);
                      state->arg_n++;
                    }
                  else
                    {
                      char* msg = malloc(sizeof(char)*
                                         strlen("Value not found.") +
                                         strlen(state->buffer)+
                                         strlen(" ``") +1);
                      sprintf(msg, "Value `%s` not found.",
                              state->buffer);
                      do_error(msg);
                      free(msg);
                      break;
                    }
                }
              state->buffer[0] = '\0';
              state->i = 0;
            }
        }
      else if (c == '(')
        {
          if (state->in_instr == 0)
            {
              state->open_paren = '(';
            }
          else
            {
              state->buffer[state->i] = c;
              state->i++;
            }

          state->in_instr++;

        }
      else if (c == ')')
        {
          state->in_instr--;
          if (state->in_instr == 0)
            {
              state->after_instr = 1;
              if (state->open_paren != '(')
                {
                  do_error("Parenthesis do not match.");
                  break;
                }
            }
          else
            {
              state->buffer[state->i] = c;
              state->i++;
            }
        }
      else if (c == '{')
        {
          if (state->in_instr == 0)
            {
              state->open_paren = '{';
            }
          else
            {
              state->buffer[state->i] = c;
              state->i++;
            }
          
          state->in_instr++;
        }
      else if (c == '}')
        {
          state->in_instr--;
          if (state->in_instr == 0)
            {
              state->after_instr = 1;
              if (state->open_paren != '{')
                {
                  do_error("Parenthesis do not match.");
                  break;
                }
            }
          else
            {
              state->buffer[state->i] = c;
              state->i++;
            }

        }
      else if (c == '[')
        {
          if (state->in_instr == 0)
            {
              state->open_paren = '[';
            }
          else
            {
              state->buffer[state->i] = c;
              state->i++;
            }
          state->in_instr++;
        }
      else if (c == ']')
        {
          state->in_instr--;
          if (state->in_instr == 0)
            {
              state->after_instr = 1;
              if (state->open_paren != '[')
                {
                  do_error("Parenthesis do not match.");
                  break;
                }
            }
          else
            {
              state->buffer[state->i] = c;
              state->i++;
            }
        }
      else if (c == '"' && !state->in_instr)
        {
          if (state->in_quote)
            {
              state->in_quote = 0;
              state->after_quote = 1;
            }
          else
            {
              state->in_quote = 1;
            }
        }
      else
        {
          state->buffer[state->i] = c;
          state->i++;
          complete = 0;
        }
    }

  if (is_error(-1))
    {
      state->arg_n = 0;
      state->i = 0;
      state->buffer[0] = '\0';
      state->in_instr = 0;
      state->after_instr = 0;
      is_error(0);
      complete = 1;
      if ((*arg_reg) != NULL)
        {
          free_registry(*arg_reg);
          *arg_reg = NULL;
        }

    }

  return complete;
}

