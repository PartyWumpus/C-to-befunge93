#include<softfloat.h>

void exit(int status);

typedef union { uint64_t ui; double f; float64_t s; } i64_f64;

double _bf_double_mulAdd(double a, double b, double c) {
  i64_f64 x;
  x.f = a;
  i64_f64 y;
  y.f = b;
  i64_f64 z;
  z.f = c;

  i64_f64 o;
  o.ui = f64_mulAdd(x.s, y.s, z.s).v;
  return o.f;
}

// binary operations

double _bf_double_add(double a, double b) {
  i64_f64 x;
  x.f = a;
  i64_f64 y;
  y.f = b;

  i64_f64 o;
  o.ui = f64_add(x.s, y.s).v;
  return o.f;
}
double _bf_double_sub(double a, double b) {
  i64_f64 x;
  x.f = a;
  i64_f64 y;
  y.f = b;

  i64_f64 o;
  o.ui = f64_sub(x.s, y.s).v;
  return o.f;
}
double _bf_double_multiply(double a, double b) {
  i64_f64 x;
  x.f = a;
  i64_f64 y;
  y.f = b;

  i64_f64 o;
  o.ui = f64_mul(x.s, y.s).v;
  return o.f;
}
double _bf_double_divide(double a, double b) {
  i64_f64 x;
  x.f = a;
  i64_f64 y;
  y.f = b;

  i64_f64 o;
  o.ui = f64_div(x.s, y.s).v;
  return o.f;
}

double _bf_double_modulo(double a, double b) {
  i64_f64 x;
  x.f = a;
  i64_f64 y;
  y.f = b;

  i64_f64 o;
  o.ui = f64_rem(x.s, y.s).v;
  return o.f;
}

// comparisons

int _bf_double_is_equal(double a, double b) {
  i64_f64 x;
  x.f = a;
  i64_f64 y;
  y.f = b;

  return f64_eq(y.s, x.s);
}

int _bf_double_is_not_equal(double a, double b) {
  i64_f64 x;
  x.f = a;
  i64_f64 y;
  y.f = b;

  return !f64_eq(y.s, x.s);
}

int _bf_double_is_less_than(double a, double b) {
  i64_f64 x;
  x.f = a;
  i64_f64 y;
  y.f = b;

  return f64_lt(x.s, y.s);
}
double _bf_double_is_less_or_equal(double a, double b) {
  i64_f64 x;
  x.f = a;
  i64_f64 y;
  y.f = b;

  return f64_le(x.s, y.s);
}
double _bf_double_is_greater_than(double a, double b) {
  i64_f64 x;
  x.f = a;
  i64_f64 y;
  y.f = b;

  return f64_lt(y.s, x.s);
}
double _bf_double_is_greater_or_equal(double a, double b) {
  i64_f64 x;
  x.f = a;
  i64_f64 y;
  y.f = b;

  return f64_le(y.s, x.s);
}

// casts

long _bf_f64_to_i64(double a) {
  i64_f64 x;
  x.f = a;
  return f64_to_i64_r_minMag(x.s, false);
}

double _bf_i64_to_f64(long a) {
  i64_f64 o;
  o.ui = i64_to_f64(a).v;
  return o.f;
}

double _bf_ui64_to_f64(long a) {
  i64_f64 o;
  o.ui = ui64_to_f64(a).v;
  return o.f;
}
