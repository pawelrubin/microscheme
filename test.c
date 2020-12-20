#include <stdio.h>

struct lambda_env_2 {
  int a;
  int b;
};

int bar(struct lambda_env_2 args, int a, int b) {
  return args.a + args.b + a + b;
}

/*
(define (foo x y)
  (define (bar a b)
    (+ x y a b)
    )
  (bar 21 37)
  )
*/

int foo(int x, int y) {
  struct lambda_env_2 env = {x, y};
  return bar(env, 21, 37);
}

int main() { printf("%d\n", foo(1, 2)); }
