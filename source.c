int main(void) {
  int x = 7;
  return fib(x);
}

int fib(int a) {
  if (a < 1) {
    return 1;
  } else {
    return fib(a - 1) + fib(a - 2);
  }
}
