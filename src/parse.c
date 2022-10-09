/* 
   SLOBIL is a Object Based Environment and Language

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

struct parser_state
fresh_state (int print) 
{
  struct parser_state state;
  state.buffer = malloc(sizeof(char)*1024);
  state.buffer[0] = '\0';
  state.buffer_sz = 1024;
  state.arg_n = 0;
  state.i = 0;
  state.in_instr = 0;
  state.after_instr = 0;
  state.in_quote = 0;
  state.after_quote = 0;
  state.open_paren = '\0';
  state.print_out = print;
  state.in_comment = 0;
  state.cur_elem = NULL;
  state.cur_stmt = NULL;

  return state;
}

void
free_state (struct parser_state* state)
{
  if (state->buffer != NULL)
    free(state->buffer);
}

void
clear_state_buffer (struct parser_state* state)
{
  state->buffer[0] = '\0';
  state->i = 0;
}

void
add_to_state_buffer (struct parser_state* state, const char c, bool incr)
{
  if (state->i >= state->buffer_sz)
    {
      state->buffer = realloc(state->buffer,
                              sizeof(char)*(state->buffer_sz +1024));
      state->buffer_sz += 1024;
    }
  state->buffer[state->i] = c;
  
  if (incr) state->i++;
}


element*
add_literal_argument (element** head, element* e, data* d)
{
  if (*head == NULL)
    {
      *head = append_literal_element(NULL, d);
      return *head;
    }
  else
    {
      e = append_literal_element(e, d);
      return e;
    }

}

element*
add_lookup_argument (element** head, element* e, char* d)
{
  int count = 0;
  int* is_slot;
  char** name = split_by_colon(d, &count, &is_slot);
  unsigned long* hash_name = malloc(sizeof(unsigned long)*(count));
  int i;
  for (i=0; i < count; i++)
    {
      hash_name[i] = hash_str(name[i]);
    }
    
  if (*head == NULL)
    {
      *head = append_argument_element(NULL, name, hash_name, count,
                                      is_slot);
      return *head;
    }
  else
    {
      e = append_argument_element(e, name, hash_name, count, is_slot);
      return e;
    }

}

element*
add_statement_argument (element** head, element* e, statement* s)
{
  if (*head == NULL)
    {
      *head = append_statement_element(NULL, s);
      return *head;
    }
  else
    {
      e = append_statement_element(e, s);
      return e;
    }

}

element*
parse_stmt (FILE* f, parser_state* state, int* complete, task_vars* task)
{
  char c;
  data* d = NULL;
  int sub_complete = 0;
  element* head = state->cur_elem;
  element* e = head;
  statement* sub_stmt = NULL;
  FILE* f_sub = NULL;
  parser_state sub_state;
  char* str;
  uint32_t* str32;
  
  /* Go to end of current element list. */
  while (e != NULL)
    {
      if (e->right == NULL) break;
      e = e->right;
    }

  while (!(*complete) && (((c = fgetc(f)) != EOF) && c != '\0'))
    {
      /* Handle comment */
      if (state->in_comment && (c != '\n'))
        continue;
      if (state->in_comment && (c == '\n'))
        {
          *complete = 1;
          continue;
        }

      /* Once we reach whitespace, it is time to interpret what we have in the buffer. */
      if (is_whitespace(c) && !state->in_instr && !state->in_quote)
        {
          add_to_state_buffer(state, '\0', false);
          if ((strlen(state->buffer) != 0 || state->after_quote) && !state->in_quote)
            {
              if (state->after_instr)
                {
                  /* We have a sub expression of some kind. */
                  str = malloc(sizeof(char)*(strlen(state->buffer)+2));
                  strcpy(str, state->buffer);

                  /* Stream the sub expression into parse so we can make it a sequence of
                     statements itself. */
                  f_sub = fmemopen(str,
                                   sizeof(char)*strlen(str), "r");
                  sub_state = fresh_state(0);
                  sub_stmt = NULL;
                  sub_complete = parse(f_sub, &sub_state, &sub_stmt, task);
                  fclose(f_sub);

                  if (!sub_complete)
                    {
                      char* ending = malloc(sizeof(char)*(strlen(" . ")+1));
                      strcpy(ending, " . ");
                      f_sub = fmemopen(ending,
                                       sizeof(char)*strlen(ending),
                                       "r");
                      sub_complete = parse(f_sub, &sub_state, &sub_stmt, task);
                      fclose(f_sub);
                      free(ending);
                    }

                  free_state(&sub_state);
                  free(str);
                  if (sub_complete & sub_stmt != NULL)
                    {
                      /* We have a complete substatement.  React accordingly. */

                      switch (state->open_paren)
                        {
                        case '(':
                          assign_instr(&d, NULL, state->buffer);
                          ((instruction*) d->data)->stmt = sub_stmt;
                          e = add_literal_argument(&head, e, d);
                          break;
                        case '{':
                          assign_expression(&d, NULL);
                          ((instruction*) d->data)->stmt = sub_stmt;
                          e = add_literal_argument(&head, e, d);
                          break;
                        case '[':
                          e = add_statement_argument(&head, e, sub_stmt);
                          break;
                        }
                      state->open_paren = '\0';
                      state->after_instr = 0;
                    }
                  else
                    {
                      do_error("Incomplete instruction in parenthesis.",
                               task);
                      state->open_paren = '\0';
                      state->after_instr = 0;
                      if (sub_stmt != NULL)
                        free_statement(sub_stmt);
                      break;
                    }
                }
              else if (state->after_quote)
                {
                  /* Handle quoted text.  Create string data from the text. */
                  state->after_quote = 0;
                  if (strlen(state->buffer)==0)
                    {
                      str32 = malloc(sizeof(uint32_t));
                      *str32 = (uint32_t) 0;
                      assign_str(&d, str32, 0);
                    }
                  else
                    {
                      str = escape_str(state->buffer);
                      str32 = slobil_u8_to_u32(str, strlen(str));
                      assign_str(&d, str32, 0);
                    }
                  e = add_literal_argument(&head, e, d);
                }
              else if (is_integer(state->buffer))
                {
                  /* Create Integer data if a literal integer. */
                  mpz_t mz;
                  mpz_init(mz);
                  mpz_set_str(mz, state->buffer, 10);
                  assign_int(&d, mz);
                  e = add_literal_argument(&head, e, d);
                  mpz_clear(mz);
                }
              else if (is_real(state->buffer) &&
                       strcmp(state->buffer, ".")!=0)
                {
                  /* Create Real data if a literal real. */
                  assign_real(&d, atof(state->buffer));
                  e = add_literal_argument(&head, e, d);
                }
              else if (is_slot(state->buffer))
                {
                  /* Create Slot data if a literal slot. */
                  assign_slot(&d, state->buffer+1,
                                hash_str(state->buffer+1));
                  e = add_literal_argument(&head, e, d);
                }
              else if (is_nothing(state->buffer))
                {
                  /* Assign Nothing if the word "Nothing" is in the buffer. */
                  assign_nothing(&d);
                  e = add_literal_argument(&head, e, d);
                }
              else if (is_boolean(state->buffer))
                {
                  /* Create Boolean data if a literal boolean. */ 
                  if (strcmp(state->buffer, "True")==0)
                    assign_boolean(&d, true);
                  else
                    assign_boolean(&d, false);
                  e = add_literal_argument(&head, e, d);
                }
              else if (strcmp(state->buffer,".")==0)
                {
                  /* Terminate statement. */
                  *complete = 1;
                  continue;
                }
              else if (strcmp(state->buffer, "'")==0)
                {
                  /* Start comment. */
                  state->in_comment = 1;
                  state->arg_n = 0;
                }
              else 
                {
                  /* If not a literal, it must be a reference to a value located at some slot. */
                  e = add_lookup_argument(&head, e, state->buffer);
                }
              clear_state_buffer(state);
            }
          else if (is_whitespace(c))
            {
              clear_state_buffer(state);
            }

        }
      else if (c == '(' && !state->in_quote)
        {
          /* Start capturing instruction. */
          if (state->in_instr == 0)
            {
              state->open_paren = '(';
            }
          else
            {
              add_to_state_buffer(state, c,true);
            }

          state->in_instr++;

        }
      else if (c == ')' && !state->in_quote)
        {
          /* End capturing instruction. */
          state->in_instr--;
          if (state->in_instr == 0)
            {
              state->after_instr = 1;
              if (state->open_paren != '(')
                {
                  do_error("Parenthesis do not match.", task);
                  break;
                }
            }
          else
            {
              add_to_state_buffer(state,c,true);
            }
        }
      else if (c == '{' && !state->in_quote)
        {
          if (state->in_instr == 0)
            {
              state->open_paren = '{';
            }
          else
            {
              add_to_state_buffer(state,c,true);
            }
          
          state->in_instr++;
        }
      else if (c == '}' && !state->in_quote)
        {
          state->in_instr--;
          if (state->in_instr == 0)
            {
              state->after_instr = 1;
              if (state->open_paren != '{')
                {
                  do_error("Parenthesis do not match.", task);
                  break;
                }
            }
          else
            {
              add_to_state_buffer(state,c,true);
            }

        }
      else if (c == '[' && !state->in_quote)
        {
          if (state->in_instr == 0)
            {
              state->open_paren = '[';
            }
          else
            {
              add_to_state_buffer(state,c,true);
            }
          state->in_instr++;
        }
      else if (c == ']' && !state->in_quote)
        {
          state->in_instr--;
          if (state->in_instr == 0)
            {
              state->after_instr = 1;
              if (state->open_paren != '[')
                {
                  do_error("Parenthesis do not match.", task);
                  break;
                }
            }
          else
            {
              add_to_state_buffer(state, c, true);
            }
        }
      else if (c == '"')
        {
          if (state->in_quote)
            {
              state->in_quote = 0;
              if (!state->in_instr)
                state->after_quote = 1;
            }
          else
            {
              state->in_quote = 1;
            }

          if (state->in_instr)
            add_to_state_buffer(state,c,true);
              
        }
      else
        {
          add_to_state_buffer(state,c,true);
          *complete = 0;
        }

      if (is_error(-1, task))
        {
          break;
        }
    }

  if (is_error(-1, task))
    {
      clear_state_buffer(state);

      state->arg_n = 0;
      state->in_instr = 0;
      state->after_instr = 0;
      
      is_error(0, task);
      *complete = 1;
    }

  return head;
 
}

int
parse (FILE* f, parser_state* state, statement** s, task_vars* task)
{
  int complete = 0;
  statement* stmt = NULL;
  int stop_reading = 1;
  do
    {
      complete = 0;
      state->cur_elem = parse_stmt(f, state, &complete, task);

      if (complete && !state->in_comment)
        {
          stmt = append_statement(stmt, state->cur_elem);

          if (state != NULL) free_state(state);
          *state = fresh_state(state->print_out);
          if (*s == NULL)
            {
              *s = stmt;
            }
        }
      else if (state->in_comment)
        {
          if (state != NULL) free_state(state);
          *state = fresh_state(state->print_out);
        }
      else if (state->cur_elem != NULL)
        {
          stop_reading = 0;
        }
    }
  while (complete==1);

  return (stop_reading);
    
}

int
interact (FILE* f, parser_state* state, object* reg)
{
  statement* s = NULL;
  int complete = parse(f, state, &s, reg->task->task);
  data* d;
  if (complete)
    {
      execute_code(s, reg);
      if (!is_error(-1, reg->task->task))
        {
          if (reg->up == NULL && state->print_out)
            {
              d = get(reg, reg->task->task->slobil_hash_ans, 0);
              if (d != NULL && d != (reg->task->task->last_ans))
                {
                  data* opt = get(reg->task->task->slobil_options,
                                  hash_str("print-ans"),
                                  0);
                  bool print_out = false;
                  if (opt != NULL && opt->type == Boolean)
                    {
                      print_out = *((bool*) opt->data);
                    }
                  if (print_out)
                    {
                      print_data(d, (PRINT_ANSWER | PRINT_QUOTES | PRINT_NEWLINE));
                    }

                  reg->task->task->last_ans = d;
                }
            }

        }

      if (state != NULL) free_state(state);
      *state = fresh_state(state->print_out);
      
    }

  if (s != NULL)
    free_statement(s);

  if (is_error(-1, reg->task->task))
    {
      is_error(0, reg->task->task);
      complete = 1;
    }

  return complete;
}
