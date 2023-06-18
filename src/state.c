/*
  SLOBIL
  Copyright 2023 Zach Flynn

  Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

  1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

  2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

  3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS “AS IS” AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
   
*/

#include "slobil.h"

int
is_error (int e, task_vars* t)
{
  if (e >= 0)
    {
      t->slobil_error = e;
    }
  return t->slobil_error;
}


void
do_error (const char* msg, task_vars* t)
{
  slot sl = make_slot("print-errors");
  data* d = get(t->slobil_options,
                &sl,
                0);
  bool print_error = true;
  if (!(d==NULL || d->type != Boolean))
    {
      print_error = *((bool*) d->data);
    }
  if (print_error)
    fprintf(stderr, "Error: %s\n", msg);
  
  (void) is_error(1, t);
}

int
is_exit (int e, task_vars* t)
{
  if (e >= 0)
    {
      t->do_exit = e;
    }
  return t->do_exit;
}

