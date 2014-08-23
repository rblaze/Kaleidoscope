#include <stdio.h>

double putchard(double v) {
    putchar((char) v);
    fflush(stdout);
    return 0;
}
