#include "onbu.h"
#include <check.h>
#include <stdlib.h>

START_TEST(test_assign_real)
{
  data* d = NULL;
  assign_real(&d, 1.4);

  ck_assert_double_eq(*((double*) d->data), 1.4);
  ck_assert_double_ne(*((double*) d->data), 1.3);
  
  free_data(d);  
  
}
END_TEST
  
START_TEST(test_assign_int)
{
  data* d = NULL;

  mpz_t z;
  mpz_init_set_si(z, 54);
  assign_int(&d, z);

  ck_assert_int_eq(mpz_get_si(*((mpz_t*) d->data)), 54);
  ck_assert_int_ne(mpz_get_si(*((mpz_t*) d->data)), 23);
  
  free_data(d);  
  
}
END_TEST

START_TEST(test_assign_str)
{
  data* d = NULL;

  char* str = "Hello, world!";
  size_t length = 0;
  uint32_t* str32 = u8_to_u32(str, strlen(str)+1,
                              NULL, &length);

  uint32_t* str32_alt = u8_to_u32("Hello", strlen(str)+1,
                                  NULL, &length);
  assign_str(&d, str32, 1);

  ck_assert(u32_strcmp(str32, (uint32_t*) d->data)==0);
  ck_assert(u32_strcmp(str32_alt, (uint32_t*) d->data) != 0);

  free(str32_alt);
  free(str32);  
  free_data(d);  
  
}
END_TEST

START_TEST(test_assign_bool)
{
  data* d = NULL;

  assign_boolean(&d, true);
  ck_assert_int_eq(*((bool*) d->data), true);
  ck_assert_int_ne(*((bool*) d->data), false);
  
  free_data(d);  
}
END_TEST

START_TEST(test_assign_nothing)
{
  data* d = NULL;

  assign_nothing(&d);

  ck_assert_ptr_null(d->data);
  ck_assert_int_eq(d->type, Nothing);
  
  free_data(d);  
}
END_TEST

START_TEST(test_assign_regstr)
{
  data* d = NULL;

  assign_regstr(&d, "test", hash_str("test"));

  ck_assert_str_eq(((regstr*) d->data)->name, "test");
  ck_assert_str_ne(((regstr*) d->data)->name, "test1");
  ck_assert_uint_eq(((regstr*) d->data)->key, hash_str("test"));
  
  free_data(d);  
}
END_TEST

START_TEST(test_assign_file)
{
  data* d = NULL;

  FILE* f = fopen("Makefile", "r");
  assign_file(&d, f);

  ck_assert_ptr_eq(d->data, f);
  ck_assert_ptr_nonnull(d->data);

  fclose(f);
  free_data(d);  
}
END_TEST



static Suite*
wob_copy_suite ()
{
  Suite* s;
  TCase* tc_core;

  s = suite_create("WOB copy");
  tc_core = tcase_create("copy");

  tcase_add_test(tc_core, test_assign_real);
  tcase_add_test(tc_core, test_assign_int);
  tcase_add_test(tc_core, test_assign_str);
  tcase_add_test(tc_core, test_assign_bool);
  tcase_add_test(tc_core, test_assign_nothing);
  tcase_add_test(tc_core, test_assign_regstr);
  tcase_add_test(tc_core, test_assign_file);

  suite_add_tcase(s, tc_core);

  return s;
  
}

int
main (int argc, char** argv)
{

  Suite* s;
  SRunner* sr;

  s = wob_copy_suite();
  sr = srunner_create(s);

  srunner_run_all(sr, CK_NORMAL);

  int number_failed = srunner_ntests_failed(sr);

  srunner_free(sr);

  return (number_failed>0);
}
  
  
