int main(void) {
 int b = 10;
 return fib(b);
}


int fib(int i) {
  if (i == 0 || i == 1) {
    return 1;
  } else {
    return fib(i - 1) + fib(i - 2);
  }
}
