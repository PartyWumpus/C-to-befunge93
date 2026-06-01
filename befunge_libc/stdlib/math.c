double fma(double x, double y, double z) {
  // from internal/_bf_float.c
  double _bf_double_mulAdd(double a, double b, double c);

  return _bf_double_mulAdd(x, y, z);
}

// TODO: handle NaN
double fmax(double a, double b) {
  if (a == 0.0 && b == 0.0) { // == 0.0 or -0.0
    return a + b;
  }

  if (a > b) {
    return a;
  } else {
    return b;
  }
}

double copysign(double a, double b) {
  typedef union { long i; double f; } i64_f64;
  i64_f64 x;
  x.f = a;
  i64_f64 y;
  y.f = b;

  long x_sign = x.i < 0;
  long y_sign = y.i < 0;

  if (x_sign != y_sign) {
    x.i += 9223372036854775808;
  }

  return x.f;
}
