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
           
