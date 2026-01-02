long _bf_bitshift_left(long value, long shift) {
  for (long i = 0; i < shift; i++) {
    value *= 2;
  }
  return value;
}

long _bf_bitshift_right(long value, long shift) {
  for (long i = 0; i < shift; i++) {
    value /= 2;
  }
  return value;
}
