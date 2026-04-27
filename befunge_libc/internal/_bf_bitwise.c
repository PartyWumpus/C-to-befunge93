long pow2[63] = {
    1L, 2L, 4L, 8L, 16L, 32L, 64L, 128L, 256L, 512L,
    1024L, 2048L, 4096L, 8192L, 16384L, 32768L, 65536L,
    131072L, 262144L, 524288L, 1048576L, 2097152L, 4194304L,
    8388608L, 16777216L, 33554432L, 67108864L, 134217728L,
    268435456L, 536870912L, 1073741824L, 2147483648L,
    4294967296L, 8589934592L, 17179869184L, 34359738368L,
    68719476736L, 137438953472L, 274877906944L, 549755813888L,
    1099511627776L, 2199023255552L, 4398046511104L, 8796093022208L,
    17592186044416L, 35184372088832L, 70368744177664L,
    140737488355328L, 281474976710656L, 562949953421312L,
    1125899906842624L, 2251799813685248L, 4503599627370496L,
    9007199254740992L, 18014398509481984L, 36028797018963968L,
    72057594037927936L, 144115188075855872L, 288230376151711744L,
    576460752303423488L, 1152921504606846976L, 2305843009213693952L,
    4611686018427387904L
};

long _bf_bitshift_left(long value, long shift) {
  for (long i = 0; i < shift; i++) {
    value *= 2;
  }
  return value;
  // TODO: do this (w/ bounds check stuff)
  //return value * pow2[shift];
}

long _bf_signed_bitshift_right(long value, long shift) {
  for (long i = 0; i < shift; i++) {
    value /= 2;
  }
  return value;
}

long _bf_unsigned_long_bitshift_right(long value, long shift) {
  if (value >= 0) {
    for (long i = 0; i < shift; i++) {
      value /= 2;
    }
    return value;
  }

  long low = value - (-9223372036854775807L - 1L);
  long result = pow2[63 - shift];

  for (long i = 0; i < shift; i++) {
    low /= 2;
  }

  return result + low;
}
