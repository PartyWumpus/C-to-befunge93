#include<stdio.h>

int strcmp(char* s1, char* s2) {
    while(*s1 && (*s1 == *s2)) {
        s1++;
        s2++;
    }
    return *s1 - *s2;
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
