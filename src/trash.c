void
trash (void* junk, size_t sz, trash_heap* trash)
{
  trash_heap* heap = malloc(sizeof(trash_heap));
  heap->garbage = junk;
  heap->right = NULL;

  trash_heap* head = trash->trash;

  if (head == NULL)
    {
      trash->trash = heap;
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
trash_registry (registry* reg)
{
  registry_trash_heap* heap = malloc(sizeof(registry_trash_heap));
  heap->garbage = reg;
  heap->right = NULL;
  registry_trash_heap* head = arbel_trash.registry_trash;
  if (head == NULL)
    {
      arbel.registry_trash = heap;
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
trash_c (void* object)
{
  
}

