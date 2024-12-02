int main(void) {
  int i = 0;
  while (i < 10) {
    i += 1;
    print_int(fib(i));
  }
  return 200;
}

int print_int(int a) {
  __asm__(
      "v>"
      "3."
      "0g"
      ">^"
      : 
      : [r1] "+" (a)
      );
  return;
}

int fib(int a) {
  if (a == 0 || a == 1) {
    return 1;
  } 
  return fib(a-1) + fib(a-2);
}
