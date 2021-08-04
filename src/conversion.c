#include "wob.h"
#include <errno.h>

uint32_t *
wob_u8_to_u32 (const uint8_t *s, size_t n)
{
  const uint8_t *s_end = s + n;
  /* Output string accumulator.  */
  uint32_t *result;
  size_t allocated;
  size_t length;

  result = NULL;
  allocated = 0;

  length = 0;

  while (s < s_end)
    {
      ucs4_t uc;
      int count;

      /* Fetch a Unicode character from the input string.  */
      count = u8_mbtoucr (&uc, s, s_end - s);
      if (count < 0)
        {
          if (!(result == NULL))
            free (result);
          errno = EILSEQ;
          return NULL;
        }
      s += count;

      /* Store it in the output string.  */
      if (length + 1 > allocated)
        {
          uint32_t *memory;

          allocated = (allocated > 0 ? 2 * allocated : 12);
          if (length + 1 > allocated)
            allocated = length + 1;
          if (result == NULL)
            memory = (uint32_t *) malloc (allocated * sizeof (uint32_t));
          else
            memory =
              (uint32_t *) realloc (result, allocated * sizeof (uint32_t));

          if (memory == NULL)
            {
              if (!(result == NULL))
                free (result);
              errno = ENOMEM;
              return NULL;
            }
          if (result == NULL && length > 0)
            memcpy ((char *) memory, (char *) result,
                    length * sizeof (uint32_t));
          result = memory;
        }
      result[length++] = uc;
    }

  if (length == 0)
    {
      if (result == NULL)
        {
          /* Return a non-NULL value.  NULL means error.  */
          result = (uint32_t *) malloc (1);
          if (result == NULL)
            {
              errno = ENOMEM;
              return NULL;
            }
        }
    }
  else if (result != NULL && length < allocated)
    {
      /* Shrink the allocated memory if possible.  */
      uint32_t *memory;

      memory = (uint32_t *) realloc (result, length * sizeof (uint32_t));
      if (memory != NULL)
        result = memory;
    }

  return result;
}

uint8_t *
wob_u32_to_u8 (const uint32_t *s, size_t n)
{
  const uint32_t *s_end = s + n;
  /* Output string accumulator.  */
  uint8_t *result;
  size_t allocated;
  size_t length;

  result = NULL;
  allocated = 0;

  length = 0;
  /* Invariants:
     result is either == resultbuf or == NULL or malloc-allocated.
     If length > 0, then result != NULL.  */

  while (s < s_end)
    {
      ucs4_t uc;
      int count;

      /* Fetch a Unicode character from the input string.  */
      count = u32_mbtoucr (&uc, s, s_end - s);
      if (count < 0)
        {
          if (!(result == NULL))
            free (result);
          errno = EILSEQ;
          return NULL;
        }
      s += count;

      /* Store it in the output string.  */
      if (length + 1 > allocated)
        {
          uint8_t *memory;

          allocated = (allocated > 0 ? 2 * allocated : 12);
          if (length + 1 > allocated)
            allocated = length + 1;
          if (result == NULL)
            memory = (uint8_t *) malloc (allocated * sizeof (uint8_t));
          else
            memory =
              (uint8_t *) realloc (result, allocated * sizeof (uint8_t));

          if (memory == NULL)
            {
              if (!(result == NULL))
                free (result);
              errno = ENOMEM;
              return NULL;
            }
          if (result == NULL && length > 0)
            memcpy ((char *) memory, (char *) result,
                    length * sizeof (uint8_t));
          result = memory;
        }
      result[length++] = uc;
    }

  if (length == 0)
    {
      if (result == NULL)
        {
          /* Return a non-NULL value.  NULL means error.  */
          result = (uint8_t *) malloc (1);
          if (result == NULL)
            {
              errno = ENOMEM;
              return NULL;
            }
        }
    }
  else if (result != NULL && length < allocated)
    {
      /* Shrink the allocated memory if possible.  */
      uint8_t *memory;

      memory = (uint8_t *) realloc (result, length * sizeof (uint8_t));
      if (memory != NULL)
        result = memory;
    }

  return result;
}
