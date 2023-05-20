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

/**
 * @file registry.c
 * @brief Functions to manipulate objects.
 */

/**
 * Standard way of creating a new content.
 *
 * @return pointer to the newly allocated content.
 */
content*
new_content ()
{
  content* c = malloc(sizeof(content));
  c->right = NULL;
  c->left = NULL;
  c->value = NULL;
  c->name = NULL;
  c->key = 0;
  c->do_not_free_data = 0;
  return c;
}

/**
 * Standard way of creating a new object.
 *
 * @param up the starting object to be upstream from the current object (for scoping rules).
 * @param hash_size the hash size for the table.
 * @param t the task the object will belong to.
 * @return the newly allocated object
 */
object*
new_object (object* up, size_t hash_size, task* t)
{
  object* r = malloc(sizeof(object));
  r->up = up;
  r->hash_size = hash_size;
  r->objects = malloc(sizeof(content*)*hash_size);
  r->elements = 0;
  r->task = t;
  r->being_modified = false;
  r->inherit = NULL;
  
  for (int i = 0; i < hash_size; i++)
    {
      r->objects[i] = NULL;
    }

  return r;
}

/**
 * Test if content is initial element of linked list.
 *
 * @param c pointer to content to test.
 * @return true if the content is initial element of linked list and false otherwise.
 */
bool
is_init_content (content* c)
{
  return (c->right == NULL) &&
    (c->left==NULL) &&
    (c->value == NULL) &&
    (c->key == 0) &&
    (c->name == NULL);
}


/**
 * Sets slot in an object to a data value.
 *
 * @param obj the object to set the slot of.
 * @param d the data value to set the slot to.
 * @param name the name of the slot to set
 * @param rehash_flag if 0, do not rehash the object no matter its current size.
 * @returns The content that it set.  This is a convenience return, the object is modified by the call.
 */
content*
set (object* obj, data* d, char* name, int rehash_flag)
{
  slot new_slot = make_slot(name);
  content* c = del(obj, &new_slot,-1,false);

  if (c == NULL)
    {
      if (obj->objects[new_slot.key % obj->hash_size] == NULL)
        {
          obj->objects[new_slot.key % obj->hash_size] = new_content();
        }
      c = obj->objects[new_slot.key % obj->hash_size];
      c = head(c);
      
      content* new_c = new_content();
      new_c->left = c;
      new_c->right = NULL;
      new_c->value = d;
      c->right = new_c;
      new_c->name = malloc(sizeof(char)*(strlen(name)+1));
      strcpy(new_c->name, name);
      new_c->key = new_slot.key;
      
      obj->elements++;
      
      if (rehash_flag && (obj->elements > (SLOBIL_LOAD_FACTOR*(obj->hash_size))))
        {
          slot sl = make_slot("auto-rehash");
          data* check_hash = get(obj->task->task->slobil_options,
                                 &sl, 0);
          bool check = true;

          if (!(check_hash == NULL || check_hash->type != Boolean))
            {
              check = *((bool*) check_hash->data);
            }
          if (check)
            rehash(obj);
        }

      return new_c;
    }
  else
    {
      c->do_not_free_data = 0;
      c->value = d;
      return c;
    }

  return NULL;

}

/**
 * Gets data from slot in a given object.
 *
 * @param obj the object to fetch from.
 * @param hash_name the hashed name of the slot
 * @param recursive if 0, just check the object itself, if 1, then check the objects containing the object as well.
 * @return a pointer to the data value from the slot
 */
data*
get (object* obj, slot* sl, int recursive)
{
  if (obj == NULL)
    return NULL;

  if (strcmp(sl->name, "_")==0)
    {
      data* d;
      assign_object(&d, obj, false, obj->task);
      return d;
    }
  
  content* c = obj->objects[sl->key % obj->hash_size];
  if (c == NULL || is_init_content(c))
    {
      if (obj->inherit != NULL)
        {
          data* inherit =  get(obj->inherit, sl, 0);
          if (inherit != NULL)
            return inherit;
        }
      
      if (recursive)
        {
          return get(obj->up, sl, recursive);
        }
      else
        {
          return NULL;
        }
    }
  
  c = c->right;

  while (c != NULL)
    {
      if ((strcmp(c->name, sl->name) == 0) && c->value != NULL)
        return c->value;
      c = c->right;
    }

  if (obj->inherit != NULL)
    {
      data* inherit =  get(obj->inherit, sl, 0);
      if (inherit != NULL)
        return inherit;
    }

  if ((obj->up != NULL) && recursive)
    {
      return get(obj->up, sl, recursive);
    }
  else
    {
      return NULL;
    }
}

/**
 * Gets data from slot in a given object.  If it is an Expression, evaluate it.
 *
 * @param obj the object to fetch from.
 * @param hash_name the hashed name of the slot
 * @param recursive if 0, just check the object itself, if 1, then check the objects containing the object as well.
 * @return a pointer to the data value from the slot
 */

data*
lookup (object* obj, slot* sl, int recursive)
{
  data* d = get(obj, sl, recursive);

  if (d == NULL)
    return NULL;

  d = resolve(d, obj);

  return d;
}

/**
 * Move data from one slot to another in an object.
 *
 * @param obj the object.
 * @param old the current slot for the data.
 * @param new the new slot for the data.
 * @return a pointer to the content that was set
 */
content*
mov (object* obj, slot* old, slot* new)
{
  unsigned long old_element = old->key % obj->hash_size;

  content* cur = obj->objects[old_element];
  if (cur == NULL)
    return NULL;

  cur = cur->right;
  while (cur != NULL)
    {
      if (strcmp(cur->name, old->name)==0)
        {
          del(obj, new, 1, false);
          data* d = cur->value;
          int do_not_free_data = cur->do_not_free_data;
          del(obj, old, 0, false);
          content* c = set(obj, d, new->name, 0);
          c->do_not_free_data = do_not_free_data;
          return c;
        }
      cur = cur->right;
    }
  return NULL;
}

/**
 * Delete data from a slot.
 *
 * @param obj the object.
 * @param hash_name the hashed name of the slot to delete.
 * @param del_data if 1, delete the underlying data, if 0 do not delete the data just remove it from the object, if -1, delete the data but do not remove the corresponding content and return the content (this is an optimization) .
 * @param hard_free if true, deallocate the memory immediately, otherwise garbage collect
 * @return a pointer to the content where the data was deleted if del_data is less than 0, otherwise returns NULL.
 */
content*
del (object* obj, slot* sl, int del_data, bool hard_free)
{
  content* cur = obj->objects[sl->key % obj->hash_size];
  
  if (cur == NULL)
    return NULL;
  
  if (is_init_content(cur))
    return NULL;
  
  cur = tail(cur);

  while (cur != NULL)
    {
      if (strcmp(cur->name, sl->name)==0)
        {
          if (del_data >= 0)
            obj->elements--;
          
          if (cur->right != NULL && del_data >= 0)
            {
              cur->right->left = cur->left;
            }
          if (cur->left != NULL && del_data >= 0)
            {
              cur->left->right = cur->right;
            }

          if (del_data && cur->value != NULL && (!cur->do_not_free_data))
            {
#ifdef GARBAGE
              if (hard_free)
                {
#undef free_data
                  free_data(cur->value);
#define free_data(x)
                }
#endif
              free_data(cur->value);
              cur->value = NULL;
            }

          if (cur->name != NULL && del_data >= 0)
            {
              free(cur->name);
              cur->name = NULL;
            }


          if (del_data < 0)
            {
              return cur;
            }
          else
            {
              free(cur);
              
              if (is_init_content(obj->objects[sl->key % obj->hash_size]))
                {
                  free(obj->objects[sl->key % obj->hash_size]);
                  obj->objects[sl->key % obj->hash_size] = NULL;
                }
                
              return NULL;
            }

        }
      cur = cur->right;
    }

  return NULL;
}

/**
 * Mark a data value that should not be freed when the object containing it is freed.
 *
 * @param obj the object
 * @param hash_name the hashed slot to mark not to free
 */
void
mark_do_not_free (object* obj, unsigned long hash_name)
{

  content* c = obj->objects[hash_name % obj->hash_size];
  if (c==NULL || is_init_content(c))
    return;

  c = c->right;
  while (c != NULL)
    {
      if (c->key == hash_name)
        {
          c->do_not_free_data = 1;
          return;
        }
      c = c->right;
    }

}

/**
 * Function to get data from the colon notation.  
 *
 * @param obj top-level object to get data from.
 * @param hash_name array of hashes for each slot.
 * @param levels number of colon levels
 * @param is_slot array of 1 and 0's giving whether the object is a literal slot as opposed to a variable to look up.
 * @param name array of strings for each slot.
 * @return pointer to data value at final slot location
 */
data*
get_by_levels (object* reg, unsigned long* hash, int levels, int* is_slot, char** name)
{
  slot sl;
  sl.name = name[0];
  sl.key = hash[0];
  data* d = get(reg, &sl, 1);
  if (d == NULL)
    {
      char* msg = malloc(sizeof(char)*
                         (strlen("Value at slot / not found.")
                          + strlen(name[0]) + 1));
      sprintf(msg, "Value at slot /%s not found.", name[0]);
      do_error(msg, reg->task->task);
      free(msg);
    }
  else if (d->type != Object && levels > 1)
    {
      do_error("Cannot get slots in non-object.", reg->task->task);
    }
  else
    {
      for (int i=1; i < levels; i++)
        {
          if (d == NULL)
            {
              do_error("Slot not found in object.",
                       reg->task->task);
              return NULL;
            }

          if (d->type != Object)
            {
              do_error("Cannot get slots in non-object.",
                       reg->task->task);
              return NULL;
            }

          sl.name = name[i];
          sl.key = hash[i];
          if (is_slot[i])
            {
              d = get((object*) d->data, &sl, 0);
            }
          else
            {
              data* d1 = get(reg, &sl, 1);
              if (d1 == NULL || d1->type != Slot)
                {
                  do_error("Cannot use `:` with non-slot.",
                           reg->task->task);
                  return NULL;
                }
              else
                {
                  d = get((object*) d->data,
                          (slot*) d1->data,
                          0);
                }
            }
        }

      if (d == NULL)
        {
          do_error("Slot not found in object.",
                   reg->task->task);
          return NULL;
        }
		  
    }

  return d;
}


/**
 * Find the right-most content in a linked list.
 *
 * @param c pointer to an element of a content linked list
 * @return pointer to the right-most content in the list
 */
content*
head (content* c)
{
  if (c == NULL)
    return NULL;

  while (c->right != NULL)
    {
      c = c->right;
    }

  return c;
}

/**
 * Go right n times in a content linked-list.
 *
 * @param c pointer to a content element.
 * @param n the number of times to go right.
 * @return pointer to the content n elements after c in the list.
 */
content*
right_n (content* c, size_t n)
{
  if (c == NULL)
    return NULL;

  size_t i = 0;
  while (c->right != NULL && i <= n)
    {
      c = c->right;
      i++;
    }

  return c;
}

/**
 * Find the left-most content in a linked-list.
 *
 * @param c pointer to an element of a content linked list.
 * @return the left most content of a linked list.
 */
content*
tail (content* c)
{
  if (c == NULL)
    return NULL;
  
  while (c->left != NULL)
    {
      c = c->left;
    }

  return c->right;
}

/**
 * Indicates whether hash_size should be updated.
 *
 * @param elements number of elements.
 * @param hash_size the hash_size
 * @return true if elements exceeds hash_size x SLOBIL_LOAD_FACTOR
 */
bool
update_hash_size (size_t elements, size_t hash_size)
{
  return elements > (hash_size*SLOBIL_LOAD_FACTOR);
}
    
  

/**
 * Calculate new hash_size given number of elements.
 *
 * @param elements number of elements.
 * @return new hash size
 */
size_t
new_hash_size (size_t elements)
{
  size_t hash_size = ceil((double) elements / SLOBIL_LOAD_FACTOR);
  size_t factor = (hash_size / SLOBIL_HASH_SIZE) + 1;
  return SLOBIL_HASH_SIZE*factor;
}

/**
 * Rehash an object.
 *
 * @param obj object to rehash.
 */

void
rehash (object* obj)
{

  if (!update_hash_size(obj->elements, obj->hash_size))
    return;

  size_t old_size = obj->hash_size;
  obj->hash_size = 2*obj->hash_size;
  obj->elements = 0;
  content** objects = obj->objects;

  obj->objects = malloc(sizeof(content*)*obj->hash_size);

  for (int i = 0; i < obj->hash_size; i++)
    {
      obj->objects[i] = NULL;
    }


  for (int i = 0; i < old_size; i++)
    {
      /* copy */
      content* cur = objects[i];
      if (cur == NULL)
        continue;

      cur = tail(objects[i]);
      while (cur != NULL)
        {
          set(obj, cur->value, cur->name, 0);
          if (cur->do_not_free_data)
            mark_do_not_free(obj, cur->key);
          cur = cur->right;
        }

      /* clean */

      cur = objects[i];
      if (is_init_content(cur))
        {
          free(cur);
          continue;
        }

      cur = tail(cur);
      if (cur != NULL)
        free(cur->left);

      content* tmp;

      while (cur != NULL)
        {
          free(cur->name);
          tmp = cur->right;
          free(cur);
          cur = tmp;
        }

    }

  free(objects);


}

/**
 * Get an iterator to an object.
 *
 * @param obj pointer to an object.
 * @return iterator to move through the elements of an object.
 */
object_iter
get_object_iter (object* obj)
{
  object_iter iter;
  iter.obj = obj;
  iter.bucket = 0;
  iter.cur = obj->objects[0];

  while ((iter.bucket < obj->hash_size) &&
         (obj->objects[iter.bucket]==NULL))
    {
      iter.bucket++;
    }
  
  if (iter.bucket < obj->hash_size)
    {
      iter.cur = tail(obj->objects[iter.bucket]);
      iter.done = false;
    }
  else
    {
      iter.done = true;
    }
  return iter;
}

/**
 * Get next element of an interator.
 *
 * @param iter pointer to the iterator.  Will be set to the next element on exit.
 */
void
object_next_iter (object_iter* iter)
{
  iter->cur = iter->cur->right;
  if (iter->cur == NULL)
    {
      
      iter->bucket++;
      while ((iter->bucket < iter->obj->hash_size) &&
             (iter->obj->objects[iter->bucket] == NULL))
        {
          iter->bucket++;
        }

      if (iter->bucket < iter->obj->hash_size)
        {
          iter->cur = tail(iter->obj->objects[iter->bucket]);
        }
      else
        {
          iter->done = true;
          return;
        }
    }
}
  
  



