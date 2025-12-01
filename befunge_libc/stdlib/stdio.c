int putchar(int _char) {
  asm("76g," : : ["r76" (_char)]);
}

int getchar(void) {
  int a;
  asm("~76p" : ["r76" (a)] :);
  return a;
}
