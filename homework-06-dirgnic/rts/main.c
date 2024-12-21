#include <stdio.h>

extern long myMain();

int main(int argc, char** argv) {
  printf("%ld\n", myMain());
  return 0;
}


