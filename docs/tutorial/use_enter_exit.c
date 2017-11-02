// Copyright 2017 Bose Corporation.
// This software is released under the 3-Clause BSD License.
// The license can be viewed at https://github.com/BoseCorp/Smudge/blob/master/LICENSE

#include <stdio.h>
#include <stdlib.h>
#include "enter_exit.h"
#include "enter_exit_ext.h"

struct turnstile_person_t
{
    char *name;
};

void turnstile_Send_Message(turnstile_Event_Wrapper a1)
{
    turnstile_Handle_Message(a1);
}

void SMUDGE_free(const void *a1)
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
    printf("Welcome to the other side of the turnstile, %s.\n", person->name);
}

void lockedEnter(void)
{
    printf("Locking the turnstile.\n");
}

void lockedExit(void)
{
    printf("Leaving the locked state.\n");
}

void unlockedEnter(void)
{
    printf("Unlocking the turnstile.\n");
}

int main(void)
{
    turnstile_person_t nikky;

    nikky.name = "Nikola";
    turnstile_coin(NULL);
    turnstile_person(&nikky);
    return 0;
}
