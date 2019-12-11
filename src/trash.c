void
trash (void* junk, enum trash_type type, size_t sz)
{
  trash_heap* heap = malloc(sizeof(trash_heap));
  heap->garbage = junk;
  heap->right = NULL;
  heap->type = type;
  heap->sz = sz;
  arbel_trash_size += sz;

  trash_heap* head = arbel_trash->trash;

  if (head == NULL)
    {
      arbel_trash->trash = heap;
    }
  else
    {
      while (head->right != NULL)
	{
	  head = head->right;
	}
      head->right = heap;
    }
      
}

void
empty_trash_if (size_t if_size)
{
  if (arbel_trash_size < if_size)
    return;

  trash_heap* tmp;
  while (tr != NULL)
    {
      switch (tr->type)
	{
	case TRASH_C:
	  free(tr->garbage);
	  break;
	case TRASH_DATA:
	  free_data((data*) tr->garbage);
	  break;
	case TRASH_REGISTRY:
	  free_registry((registry*) tr->garbage);
	  break;
	}
      arbel_trash_size -= tr->sz;
      tmp = tr->right;
      free(tr);
      tr = tmp;
    }
}


size_t
total_size_of_trash (trash_heap* tr)
{
  size_t total = 0;
  while (tr != NULL)
    {
      total += tr->sz;
      tr = tr->right;
    }

  return total;
}
