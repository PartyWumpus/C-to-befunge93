// binary operations
double _bf_float_add(double a, double b);
double _bf_float_sub(double a, double b);
double _bf_float_multiply(double a, double b);
double _bf_float_divide(double a, double b);
double _bf_float_modulo(double a, double b);

// comparisons
double _bf_float_is_equal(double a, double b);
double _bf_float_is_not_equal(double a, double b);
double _bf_float_is_less_than(double a, double b);
double _bf_float_is_less_or_equal(double a, double b);
double _bf_float_is_greater_than(double a, double b);
double _bf_float_is_greater_or_equal(double a, double b);

// casts
long _bf_f64_to_i64(double a);
double _bf_i64_to_f64(long a);
