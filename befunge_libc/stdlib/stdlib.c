void exit(int status) {
  // this should match the POST_INIT_PRELUDE
  asm("55+ , 20g . @" : : ["r20" (status)]);
}

int atoi(char *str) {
  int out = 0;
  int sign = 1;

  while (*str == ' ' || *str == '\t' || *str == '\n') {
    str++;
  }

  if (*str == '-') {
    sign = -1;
    str++;
  } else if (*str == '+') {
    str++;
  }

  while (*str >= '0' && *str <= '9') {
    out *= 10;
    out += (*str - '0');
    str++;
  }

  return sign * out;
}

int strlen(char *str) {
    char *p = str;
    while (*p != '\0')
        p++;
    return p - str;
}

void *memset(void *ptr, int c, int n) {
  unsigned char val = c;
  unsigned char *p = ptr;
  while (n--) {
    *(p++) = val;
  }
  return ptr;
}

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
