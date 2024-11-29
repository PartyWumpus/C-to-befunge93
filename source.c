int main(void) {
  int x = 5;
  x = a();
  if (x > 1) {
    int x = 12;
    x = x + 1;
  }
  x = x + 1;
  return x;
}

int a(void) {
  return 5;
}
