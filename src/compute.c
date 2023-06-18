/* 
   SLOBIL
   Copyright 2023 Zach Flynn

   Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

   1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

   3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS “AS IS” AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   
*/

/**
 * @file compute.c
 * @brief functions for computing operations and instructions
 * @author Zach Flynn (flynnzac)
 */
#include "slobil.h"

/**
 * Returns data to a given slot.  Gives an error if trying to return from top-level.
 *
 * @param obj a pointer to an object where the slot should be set
 * @param data a pointer to the data to be returned
 * @param name the name of the slot to set, null-terminated string
 */
void
ret (object* obj, data* d, char* name)
{
  if (obj != NULL)
    {
      set(obj, d, name, 1);
    }
  else
    {
      do_error("Cannot return from top-level.", obj->task->task);
    }
}

/**
 * Sets the /ans slot in the given object to a certain value
 *
 * @param obj a pointer to the object where the /ans slot should be set
 * @param data the data to set the /ans slot to
 */
void
ret_ans (object* obj, data* d)
{
  if (d == NULL)
    return;
  
  ret(obj, d, "ans");
}

/**
 * Set /ans slot from object to object above it
 *
 * @param obj_tmp the temporary object to take from its /ans slot
 * @param obj the object to set the /ans slot in
 */
void
ret_ans_to_object (object* obj_tmp, object* obj)
{
  data* ans;
  if (!is_error(-1, obj->task->task))
    {
      ans = get(obj_tmp, &obj->task->task->slobil_slot_ans, 0);
      if (ans != NULL)
        {
          mark_do_not_free(obj_tmp, obj->task->task->slobil_slot_ans.key);
          ret_ans(obj, ans);
        }
    }
}

 

/**
 * Executes a statement in a given object
 *
 * @param instr The instruction data object which contains the statements to execute
 * @param obj the object to execute the statements in
 */
void
execute_0 (data* instr, object* obj)
{
  ((instruction*) instr->data)->being_called = true;
  execute_code(((instruction*) instr->data)->stmt, obj);
  ((instruction*) instr->data)->being_called = false;
}

/**
 * Calls an instruction in a given environment determined by the arguments
 *
 * @param a the argument object for the SLOBIL statement
 * @param obj the object to execute the instruction in
 * @param obj_ans the object to set the /ans slot of 
 * @param explicit if 1, the call was made explicitly as call instruction ... .  If 0,the call was made implicitly as instruction ...
 */
void
_op_call (arg a, object* obj, object* obj_ans, const int explicit)
{
  data* arg1 = resolve(a.arg_array[explicit], obj);
  if (arg1 == NULL || (arg1->type != Instruction && arg1->type != Operation))
    {
      do_error("First argument to `call` must be an instruction.", obj->task->task);
      return;
    }

  /* The temporary object to run the instruction in */
  object* obj_tmp = new_object(obj, SLOBIL_HASH_SIZE, obj->task);
  bool err_construct = object_from_args(a, obj_tmp, explicit+1, obj->task->task);
  if (err_construct)
    {

      free_object(obj_tmp);
      return;
    }

  execute_0(arg1, obj_tmp);

  ret_ans_to_object(obj_tmp, obj_ans);
  free_object(obj_tmp);
}

/**
 * Evaluates an operation, both built-in and user-defined
 *
 * @param op an op_wrapper object that can either contain an instruction masquerading as an operation or an actual C-level operation
 * @param obj the object to evaluate the operation in
 * @param a the argument list
 */
void
do_operation (op_wrapper* op, object* obj, arg a)
{
  if (op->instr == NULL)
    {
      op->op(a, obj);
    }
  else
    {
      object* obj_tmp = new_object(obj, SLOBIL_HASH_SIZE, obj->task);
      data* d;
      size_t len = (op->n_arg+1) < a.length ? (op->n_arg + 1) : a.length;
      for (int i=1; i < len; i++)
        {
          d = resolve(a.arg_array[i], obj);
          content* c = set(obj_tmp, d,
                           ((slot*) op->args[i-1]->data)->name,
                           1);
          c->do_not_free_data = 1;
        }

      execute_0(op->instr, obj_tmp);

      ret_ans_to_object(obj_tmp, obj);
      
      free_object(obj_tmp);
    }
}

/**
 * Computes an argument list within an object
 *
 * @param cmd pointer to the data object that represents the command element of the argument list (the first element)
 * @param obj the object to execute the argument list in
 * @param a the argument list
 */
void
compute (data* cmd, object* obj, arg a)
{
  if (cmd == NULL)
    {
      do_error("Cannot compute a statement that does not start with an operation or instruction.", obj->task->task);
      return;
    }

  switch (cmd->type)
    {
    case Operation:
      if (is_error(-1, obj->task->task))
        return;
      do_operation((op_wrapper*) cmd->data, obj, a);
      break;
    case Instruction:
      _op_call(a, obj, obj, 0);
      break;
    case Slot:
      auto_set(a, obj);
      break;
    case Object:
      method_call(a, obj);
      break;
    default:
      do_error("Tried to compute something that is not an operation or instruction.", obj->task->task);
      break;
    }

}


