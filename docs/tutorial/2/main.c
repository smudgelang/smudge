#include <stdio.h>
#include <stdlib.h>
#include "turnstyle.h"
#include "turnstyle_ext.h"

void SMUDGE_panic(void)
{
    printf("Game over!\n");
    exit(-1);
}

void flashLEDs(const turnstyle_coin_t *coin)
{
    printf("Blinky blinky\n");
}

void soundOkay(const turnstyle_person_t *person)
{
    printf("Ding\n");
}

int main(void)
{
    turnstyle_coin(NULL);
    turnstyle_person(NULL);
    return 0;
}
