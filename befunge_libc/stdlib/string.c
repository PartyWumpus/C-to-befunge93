#include<stdio.h>

int strcmp(char *s1, char *s2) {
    while(*s1 && (*s1 == *s2)) {
        s1++;
        s2++;
    }
    return *s1 - *s2;
}

int memcmp(void *s1, void *s2, long n) {
  unsigned char *p1 = s1;
  unsigned char *p2 = s2;

  while(n--) {
    if (*p1 != *p2) {
      return *p1 - *p2;
    }
    p1++;
    p2++;
  }
  return 0;
}

int puts(char *str) {
  int i = 0;
  while(str[i]) {
    putchar(str[i]);
    i++;
  }

  putchar('\n');
  return 1; //to meet spec.
}
