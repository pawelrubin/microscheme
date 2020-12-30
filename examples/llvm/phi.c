#include <stdbool.h>

bool foo(bool r, bool y) { return y || r; }

int main() { foo(true, false); }