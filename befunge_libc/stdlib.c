// dumb stupid idiot malloc
void *malloc(int size) {
  static int position = '!';
  // 0b11 * 2**61
  void* out = 4611686018427387904 + position;
  position = position + size;
  return out;
}

void *calloc(int num, int size)
{
  int max = num * size;
  int *new = malloc(max);
  for (int i = 0; i < max; i++)
  {
      new[i] = 0;
  }
  return new;
}

void free(void *ptr) {
  return;
}
