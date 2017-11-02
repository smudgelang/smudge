// Copyright 2017 Bose Corporation.
// This software is released under the 3-Clause BSD License.
// The license can be viewed at https://github.com/BoseCorp/Smudge/blob/master/LICENSE

#include <stdio.h>
#include <stdlib.h>
#include "side_effects.h"
#include "side_effects_ext.h"

void turnstile_Send_Message(turnstile_Event_Wrapper e)
{
    turnstile_Handle_Message(e);
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

void flashLEDs(const turnstile_coin_t *coin)
{
    printf("Blinky blinky\n");
}

void soundOkay(const turnstile_person_t *person)
{
    printf("Ding\n");
}

int main(void)
{
    turnstile_coin(NULL);
    turnstile_person(NULL);
    return 0;
}
