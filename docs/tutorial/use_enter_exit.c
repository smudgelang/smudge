#include <stdio.h>
#include <stdlib.h>
#include "enter_exit.h"
#include "enter_exit_ext.h"

struct turnstyle_person_t
{
    char *name;
};

void turnstyle_Send_Message(turnstyle_Event_Wrapper a1)
{
    turnstyle_Handle_Message(a1);
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

void flashLEDs(const turnstyle_coin_t *coin)
{
    printf("Blinky blinky\n");
}

void soundOkay(const turnstyle_person_t *person)
{
    printf("Welcome to the other side of the turnstyle, %s.\n", person->name);
}

void lockedEnter(void)
{
    printf("Locking the turnstyle.\n");
}

void lockedExit(void)
{
    printf("Leaving the locked state.\n");
}

void unlockedEnter(void)
{
    printf("Unlocking the turnstyle.\n");
}

int main(void)
{
    turnstyle_person_t nikky;

    nikky.name = "Nikola";
    turnstyle_coin(NULL);
    turnstyle_person(&nikky);
    return 0;
}
