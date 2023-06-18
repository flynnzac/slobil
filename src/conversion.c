/*
  SLOBIL
  Copyright 2023 Zach Flynn

  Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

  1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

  2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

  3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS “AS IS” AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
   
*/


#include <errno.h>
#include "slobil.h"

/**
 * @file conversion.c
 * @brief Functions for converting strings from u32 from/to u8 and for converting from/to host and little endian
 */

/**
 * Convert UTF-8 to UTF-32 strings, adapted from u8_to_u32 from libunistring
 *
 * @param s a UTF-8 null-terminated string
 * @param n the number of units
 * @return UTF-32 encoded version of s
 */
uint32_t *
slobil_u8_to_u32 (const uint8_t *s, size_t n)
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
            memcpy ((uint32_t *) memory, (uint32_t *) result,
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

/**
 * Convert UTF-32 string to UTF-8 string
 *
 * @param s a UTF-32 null-terminated string
 * @param n the length of the string
 * @return UTF-8 encoded version of s
 */
uint8_t *
slobil_u32_to_u8 (const uint32_t *s, size_t n)
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
            memcpy ((uint8_t *) memory, (uint8_t *) result,
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

/**
 * Converts a UTF-32 string to little endian from host.
 *
 * @param str UTF-32 encoded string in host bit order
 * @return UTF-32 string in little endian order
 */
uint32_t*
u32_str_to_le (const uint32_t* str)
{
  uint32_t* result = malloc(sizeof(uint32_t)*(u32_strlen(str)+1));

  for (size_t i=0; i < u32_strlen(str); i++)
    {
      result[i] = htole32(str[i]);
    }
  return result;
}

/**
 * Converts little endian string to host bit order.
 *
 * @param str a UTF-32 encoded string in little endian order
 * @return returns a UTF-32 encoded string in host bit order
 */
uint32_t*
u32_str_to_h (const uint32_t* str)
{
  uint32_t* result = malloc(sizeof(uint32_t)*(u32_strlen(str)+1));

  for (size_t i=0; i < u32_strlen(str); i++)
    {
      result[i] = le32toh(str[i]);
    }
  return result;
}

