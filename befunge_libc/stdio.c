int putchar(int _char) {
  __asm__("89g," : : [r96] "=" (_char));
}

int getchar(void) {
  int a;
  __asm__("~89p" : [r96] "=" (a) :);
  return a;
}
