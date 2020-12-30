#include <stdio.h>
#include <stdlib.h>

int display(int x) {
  printf("%d", x);
  return x;
}

int newline() {
  printf("\n");
  return 0;
}

int read() {
  int x = -1;
  if (scanf("%d", &x) == 1) {
    return x;
  }
  fprintf(stderr, "Failed to read value.");
  exit(1);
}
