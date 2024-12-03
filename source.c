int main(void) {
 int a = 10;
 print_int(a, 2);
 return a;
}

int w(void) {
 int a = 10;
 __asm__("5+": [bstack] "" (a): [bstack] "" (a));
 return a;
}

int print_int(int a) {
  __asm__("." : : [bstack] "" (a));
  return;
}
