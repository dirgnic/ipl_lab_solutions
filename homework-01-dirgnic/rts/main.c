#include <stdio.h>
#include <stdbool.h>

extern bool myMain();

int main(int argc, char** argv) {
  printf("%s\n", myMain() ? "true" : "false");
  return 0;
}


