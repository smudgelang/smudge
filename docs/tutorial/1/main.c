#include <stdio.h>
#include <stdlib.h>
#include "turnstyle.h"
#include "turnstyle_ext.h"

void SMUDGE_free(const void *thing)
{
    free(thing);
}

void SMUDGE_panic(void)
{
    printf("Game over!\n");
    exit(-1);
}

void SMUDGE_panic_print(const char *fmt, const char *a, const char *b)
{
    printf(fmt, a, b);
    exit(-1);
}

int main(void)
{
    turnstyle_coin(NULL);
    turnstyle_person(NULL);
    return 0;
}
