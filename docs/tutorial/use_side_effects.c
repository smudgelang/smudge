#include <stdio.h>
#include <stdlib.h>
#include "side_effects.h"
#include "side_effects_ext.h"

void turnstyle_Send_Message(turnstyle_Event_Wrapper e)
{
    turnstyle_Handle_Message(e);
}

void SMUDGE_free(const void *thing)
{
}

void SMUDGE_panic(void)
{
    exit(-1);
}

void SMUDGE_panic_print(const char *a1, const char *a2, const char *a3)
{
    fprintf(stderr, a1, a2, a3);
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
