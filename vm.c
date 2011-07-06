#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

#define arr(m) (m?(uint*)m:zero)
#define C w & 7
#define B (w >> 3) & 7
#define A (w >> 6) & 7

typedef unsigned int uint;

static uint * ulloc(uint size) {
    uint * r = (uint*)calloc((1 + size), 4);
    *r = size;
    return (r + 1);
}

int main (int argc, char ** argv) {
    static uint reg[8], ip, * zero;
    FILE * f = fopen(argv[1], "rb");
    if (!f) return -1;
    struct stat buf;
    if (stat(argv[1], &buf)) return -1;
    else zero = ulloc(buf.st_size >> 2);
    int a, n = 4, i = 0;
    while(EOF != (a = fgetc(f))) {
        if (!n--) { i++; n = 3; }
        zero[i] = (zero[i] << 8) | a;
    }
    fclose(f);
    for(;;) {
        uint w = zero[ip++];
        switch(w >> 28) {
        case 0: if (reg[C]) reg[A] = reg[B]; break;  /* move */
        case 1: reg[A] = arr(reg[B])[reg[C]]; break; /* array index */
        case 2: arr(reg[A])[reg[B]] = reg[C]; break; /* array amendment */
        case 3: reg[A] = reg[B] + reg[C]; break;     /* addition */
        case 4: reg[A] = reg[B] * reg[C]; break;     /* multiplication */
        case 5: reg[A] = reg[B] / reg[C]; break;     /* division */
        case 6: reg[A] = ~(reg[B] & reg[C]); break;  /* not-and */
        case 7: return 0;                            /* halt */
        case 8: reg[B] = (uint)ulloc(reg[C]); break; /* allocation */
        case 9: free(-1 + (uint*)reg[C]); break;     /* abandonment */
        case 10: putchar(reg[C]); break;             /* output */
        case 11: reg[C] = getchar(); break;          /* input */
        case 12:                                     /* load program */
            if (reg[B]) {
                free(zero - 1);
                int size = ((uint*)reg[B])[-1];
                zero = ulloc(size);
                memcpy(zero, (uint*)reg[B], size * 4);
            }
            ip = reg[C];
            break;
        case 13: reg[7 & (w >> 25)] = w & 0177777777; /* load immediate */
        }
    }
}
