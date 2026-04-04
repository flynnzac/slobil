#include "slobil.h"

void*
json_create_structure(int nesting, int is_object)
{
  object* obj = new_object(NULL, new_hash_size(1), NULL);
  data* d;
  assign_object(&d, obj, false, NULL);
  return d;
}

void*
json_create_data(int type, const char* value, uint32_t length)
{

  data* ret;
  uint32_t* str32;
  mpz_t mz;

  switch (type)
    {
    case JSON_STRING:
      str32 = slobil_u8_to_u32(value, strlen(value));
      assign_str(&ret, str32, 0);
      break;
    case JSON_INT:
      mpz_init(mz);
      mpz_set_str(mz, value, 10);
      assign_int(&ret, mz);
      mpz_clear(mz);
      break;
    case JSON_FLOAT:
      assign_real(&ret, atof(value));
      break;
    case JSON_NULL:
      assign_nothing(&ret);
      break;
    case JSON_TRUE:
      assign_boolean(&ret, true);
      break;
    case JSON_FALSE:
      assign_boolean(&ret, false);
      break;
    }

  return ret;
}

int
json_append(void* structure, char* key, uint32_t key_length, void* elem)
{
  if (key != NULL)
    {
      data* obj = (data*) structure;
      set(((object*) obj->data), elem, key, 1);
    }
  else
    {
      data* list = (data*) structure;
      object* list_obj = ((object*) list->data);
      slot sl;
      int i = 1;
      data* d;
      sl.name = vector_name("t", i);
      sl.key = hash_str(sl.name);

      while ((d = get(list_obj, &sl, 0)) != NULL)
        {
          i++;
          free(sl.name);
          sl.name = vector_name("t", i);
          sl.key = hash_str(sl.name);
        }
      free(sl.name);

      sl.name = vector_name("t", i);

      set(list_obj, elem, sl.name, 1);
    }
  return 0;
}

