#include "arbel.h"

void
clear_state_buffer (struct parser_state* state)
{
  state->buffer[0] = '\0';
  state->i = 0;
}

void
add_to_state_buffer (struct parser_state* state, const char c)
{
  state->buffer[state->i] = c;
  state->i++;
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
  if (*head == NULL)
    {
      *head = append_argument_element(NULL, d);
      return *head;
    }
  else
    {
      e = append_argument_element(e, d);
      return e;
    }

}

element*
add_statement_argument (element** head, element* e, statement* s)
{
  s = copy_statement(s);
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
parse_stmt (FILE* f, parser_state* state, int* complete)
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

  while (e != NULL)
    {
      if (e->right == NULL) break;
      e = e->right;
    }

  while (!(*complete) && (((c = fgetc(f)) != EOF) && c != '\0'))
    {
      if (state->in_comment && (c != '\n'))
        continue;
      if (state->in_comment && (c == '\n'))
        {
          *complete = 1;
          continue;
        }
      if (is_whitespace(c) && !state->in_instr && !state->in_quote)
        {
          state->buffer[state->i] = '\0';
          if (strlen(state->buffer) != 0 && !state->in_quote)
            {
              if (state->after_instr)
                {
                  str = malloc(sizeof(char)*(strlen(state->buffer)+1));
                  strcpy(str, state->buffer);
                  f_sub = fmemopen(str,
                                   sizeof(char)*strlen(str), "r");
                  sub_state = fresh_state(0);
                  sub_stmt = NULL;
                  sub_complete = parse(f_sub, &sub_state, &sub_stmt);
                  fclose(f_sub);
                  free(str);
                  if (sub_complete)
                    {
                      switch (state->open_paren)
                        {
                        case '(':
                          assign_instr(&d, sub_stmt, state->buffer);
                          e = add_literal_argument(&head, e, d);
                          break;
                        case '{':
                          assign_active(&d, sub_stmt);
                          e = add_literal_argument(&head, e, d);
                          break;
                        case '[':
                          e = add_statement_argument(&head, e, sub_stmt);
                          break;
                        }
                      state->open_paren = '\0';
                      state->after_instr = 0;
                      free_statement(sub_stmt);
                    }
                  else
                    {
                      do_error("Incomplete instruction in parenthesis.");
                      state->open_paren = '\0';
                      state->after_instr = 0;
                      if (sub_stmt != NULL)
                        free_statement(sub_stmt);
                      break;
                    }
                }
              else if (state->after_quote)
                {
                  state->after_quote = 0;
		  str = escape_str(state->buffer);
                  assign_str(&d, str,1);
                  e = add_literal_argument(&head, e, d);
                }
              else if (is_integer(state->buffer))
                {
                  int entry = atoi(state->buffer);
                  assign_int(&d, entry);
                  e = add_literal_argument(&head, e, d);
                }
              else if (is_decimal(state->buffer) &&
                       strcmp(state->buffer, ".")!=0)
                {
                  double entry = atof(state->buffer);
                  assign_dec(&d, entry);
                  e = add_literal_argument(&head, e, d);
                }
              else if (is_register(state->buffer))
                {
                  str_shift_left(state->buffer);
                  assign_regstr(&d, state->buffer);
                  e = add_literal_argument(&head, e, d);
                }
              else if (strcmp(state->buffer,".")==0)
                {
                  *complete = 1;
                  continue;
                }
              else if (strcmp(state->buffer, "c")==0)
                {
                  state->in_comment = 1;
                  state->arg_n = 0;
                }
              else if (is_reference(state->buffer))
                {
                  int i;
                  str = malloc(sizeof(char)*(strlen(state->buffer)));
                  for (i=0; i < (strlen(state->buffer)-1); i++)
                    {
                      str[i] = state->buffer[i+1];
                    }
                  str[strlen(state->buffer)-1] = '\0';
                  assign_ref(&d, NULL, str);
                  free(str);
                  e = add_literal_argument(&head, e, d);
                }
              else 
                {
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
          if (state->in_instr == 0)
            {
              state->open_paren = '(';
            }
          else
            {
              add_to_state_buffer(state, c);
            }

          state->in_instr++;

        }
      else if (c == ')' && !state->in_quote)
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
              add_to_state_buffer(state,c);
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
              add_to_state_buffer(state,c);
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
                  do_error("Parenthesis do not match.");
                  break;
                }
            }
          else
            {
              add_to_state_buffer(state,c);
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
              add_to_state_buffer(state,c);
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
          add_to_state_buffer(state,c);
          *complete = 0;
        }

      if (is_error(-1))
        {
          break;

        }
    }

  if (is_error(-1))
    {
      clear_state_buffer(state);

      state->arg_n = 0;
      state->in_instr = 0;
      state->after_instr = 0;
      
      is_error(0);
      *complete = 1;
    }

  return head;
 
}

int
parse (FILE* f, parser_state* state, statement** s)
{
  int complete = 0;
  statement* stmt = NULL;
  int stop_reading = 1;
  do
    {
      complete = 0;
      state->cur_elem = parse_stmt(f, state, &complete);

      if (complete && !state->in_comment)
        {
          stmt = append_statement(stmt, state->cur_elem);
          *state = fresh_state(state->print_out);
          if (*s == NULL)
            {
              *s = stmt;
            }
        }
      else if (state->in_comment)
        {
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
interact (FILE* f, parser_state* state, registry* reg)
{
  statement* s = NULL;
  int complete = parse(f, state, &s);
  data* d;
  if (complete)
    {
      execute_code(s, reg);
      if (!is_error(-1))
        {
          if (reg->up == NULL && is_retval(-1) &&
              state->print_out)
            {
              d = get(reg, arbel_hash_ans, 0);
              print_data(d,0);
              is_retval(0);
            }

          if (reg->up == NULL && state->print_out)
            {
              printf("OK.\n");
            }

        }

      *state = fresh_state(state->print_out);
      
    }

  if (s != NULL)
    free_statement(s);

  if (is_error(-1))
    {
      is_error(0);
      complete = 1;
    }

  return complete;
}
