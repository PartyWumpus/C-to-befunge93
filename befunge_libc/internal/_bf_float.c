#include<softfloat.h>

double _bf_double_unary_minus(double a) {
  return 0.0;
}
double _bf_double_bitwise_complement(double a) {
  return 0.0;
}
double _bf_double_boolean_negate(double a) {
  return 0.0;
}

double _bf_double_add(double a, double b) {
  // TODO: use unions
  float64_t x = { a };
  float64_t y = { b };
  return f64_add(x, y).v;
}
double _bf_double_sub(double a, double b) {
  return 0.0;
}
double _bf_double_multiply(double a, double b) {
  return 0.0;
}
double _bf_double_divide(double a, double b) {
  return 0.0;
}
double _bf_double_modulo(double a, double b) {
  return 0.0;
}
double _bf_double_is_less_than(double a, double b) {
  return 0.0;
}
double _bf_double_is_less_or_equal(double a, double b) {
  return 0.0;
}
double _bf_double_is_greater_than(double a, double b) {
  return 0.0;
}
double _bf_double_is_greater_or_equal(double a, double b) {
  return 0.0;
}
