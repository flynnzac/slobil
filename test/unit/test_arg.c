#include "wob.h"
#include <check.h>
#include <stdlib.h>

START_TEST(test_gen_arg)
{
  int length = 23;
  int def_free = 0;
  arg a = gen_arg(length, def_free);

  ck_assert_int_eq(a.length,length);

  for (int i=0; i < a.length; i++)
    {
      ck_assert_int_eq(a.free_data[i], def_free);
      ck_assert_int_ne(a.free_data[i], 1-def_free);
    }


}
END_TEST

START_TEST(test_resolve)
{
  data* real_d;
  data* expr_d;

  assign_real(&real_d, 1.2);
  char* code = malloc(sizeof(char)*(strlen("answer 34 . ")+1));
  strcpy(code, "answer 34 . ");
  
  FILE* f = fmemopen(code, sizeof(char)*strlen("answer 34 . "), "r");
  struct parser_state pstate = fresh_state(0);
  statement* s = NULL;

  task* task0;
  task0 = malloc(sizeof(task));
  task0->task = new_task(task0);
  task0->state = NULL;
  task0->code = NULL;
  task0->queued_instruction = NULL;
  task0->pid = 0;

  parse(f, &pstate, &s, task0->task);
  fclose(f);

  assign_expression(&expr_d, s);

  data* t1 = resolve(real_d, task0->task->current_parse_registry);
  ck_assert_double_eq(*((double*) t1->data), 1.2);
  ck_assert_double_ne(*((double*) t1->data), 1.0);

  data* t2 = resolve(expr_d, task0->task->current_parse_registry);
  int x = mpz_get_si(*((mpz_t*) t2->data));
  ck_assert_int_eq(x, 34);
  ck_assert_int_ne(x, 50);  

  
  
}
END_TEST

START_TEST(test_check_length)
{
  arg a = gen_arg(12, 0);
  task* task0;
  task0 = malloc(sizeof(task));
  task0->task = new_task(task0);
  task0->state = NULL;
  task0->code = NULL;
  task0->queued_instruction = NULL;
  task0->pid = 0;

  check_length(&a, 12, "null", task0->task);
  ck_assert_int_eq(task0->task->wob_error, 0);
  ck_assert_int_ne(task0->task->wob_error, 1);

  check_length(&a, 5, "null", task0->task);
  ck_assert_int_eq(task0->task->wob_error, 0);
  ck_assert_int_ne(task0->task->wob_error, 1);

  check_length(&a, 20, "null", task0->task);
  ck_assert_int_ne(task0->task->wob_error, 0);
  ck_assert_int_eq(task0->task->wob_error, 1);

  
  
}
END_TEST
  


static Suite*
wob_arg_suite ()
{
  Suite* s;
  TCase* tc_core;

  s = suite_create("WOB arg");
  tc_core = tcase_create("arg");

  tcase_add_test(tc_core, test_gen_arg);
  tcase_add_test(tc_core, test_resolve);
  tcase_add_test(tc_core, test_check_length);

  suite_add_tcase(s, tc_core);

  return s;
  
}

int
main (int argc, char** argv)
{

  Suite* s;
  SRunner* sr;

  s = wob_arg_suite();
  sr = srunner_create(s);

  srunner_run_all(sr, CK_NORMAL);

  int number_failed = srunner_ntests_failed(sr);

  srunner_free(sr);

  return (number_failed>0);
}
  
  
