#include <stdio.h>
#include <stdlib.h>

void display(int x) { printf("%d", x); }

void newline() { printf("\n"); }

int read() {
  int x = -1;
  if (scanf("%d", &x) == 1) {
    return x;
  }
  fprintf(stderr, "Failed to read value.");
  exit(1);
}
