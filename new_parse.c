int
new_parse (FILE* f, registry* reg,
           parser_state* state, statement** s)
{
  char c;
  data* d;
  int complete = 0;
  statement* stmt = NULL;
  element* head = NULL;

  do
    {
      complete = 0;
      head = parse_stmt(f, state, &complete);
      if (stmt == NULL)
        {
          *s = append_statement(NULL, head);
          stmt = *s;
        }
      else
        {
          stmt = append_statement(stmt, head);
        }
    }
  while (complete == 1);

  return complete;
    
}

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
add_argument (element** head, element* e, data* d)
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
parse_stmt (FILE* f, parser_state* state, int* complete)
{
  char c;
  data* d;
  data* d_new;
  int complete = 0;
  element* head = NULL;
  element* e;
  statement* sub_stmt;
  

  while ((((c = fgetc(f)) != EOF) && c != '\0') && !(*complete))
    {
      if (state->in_comment && (c != '\n'))
        continue;
      if (state->in_comment && (c == '\n'))
        {
          state->in_comment = 0;
          *complete = 1;
          continue;
        }
      if (is_whitespace(c) && !state->in_instr && !state->in_quote)
        {
          if (strlen(state->buffer) != 0 && !state->in_quote)
            {
              state->buffer[state->i] = '\0';
              if (state->after_instr)
                {
                  switch (state->open_paren)
                    {
                    case '(':
                      assign_instr(&d, state->buffer);
                      add_argument(d, state, arg_reg);
                      break;
                    case '{':
                      assign_active(&d, state->buffer);
                      add_argument(d, state, arg_reg);
                      break;
                    case '[':
                      int sub_complete = new_parse(,&sub_stmt);
                      add_argument(d, state, arg_reg);
                      break;
                    }

                  state->open_paren = '\0';
                  state->after_instr = 0;
                }
              else if (state->after_quote)
                {
                  state->after_quote = 0;
                  assign_str(&d, state->buffer);
                  e = add_argument(&head, e, d);
                }
              else if (is_integer(state->buffer))
                {
                  int entry = atoi(state->buffer);
                  assign_int(&d, entry);
                  e = add_argument(&head, e, d);
                }
              else if (is_decimal(state->buffer) &&
                       strcmp(state->buffer, ".")!=0)
                {
                  double entry = atof(state->buffer);
                  assign_dec(&d, entry);
                  e = add_argument(&head, e, d);
                }
              else if (is_register(state->buffer))
                {
                  str_shift_left(state->buffer);
                  assign_regstr(&d, state->buffer);
                  e = add_argument(&head, e, d);
                }
              else if (strcmp(state->buffer,".")==0)
                {
                  *complete = 1;
                  continue;
                }
              else if (strcmp(state->buffer, "rem")==0)
                {
                  state->in_comment = 1;
                  state->arg_n = 0;
                }
              else if (is_reference(state->buffer))
                {
                  str_shift_left(state->buffer);
                  assign_ref(&d, reg, state->buffer);
                  e = add_argument(&head, e, d);
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
      else if (c == '(')
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
              add_to_state_buffer(state,c);
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
              add_to_state_buffer(state,c);
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
              add_to_state_buffer(state,c);
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
              add_to_state_buffer(state,c);
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
          add_to_state_buffer(state,c);
          complete = 0;
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
      complete = 1;
      if ((*arg_reg) != NULL)
        {
          free_registry(*arg_reg);
          *arg_reg = NULL;
        }
    }

 
}
