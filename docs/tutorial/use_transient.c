#include <stdio.h>
#include <stdlib.h>
#include "transient.h"
#include "transient_ext.h"

struct turnstile_person_t
{
    char *name;
};

extern void turnstile_Send_Message(turnstile_Event_Wrapper e)
{
    turnstile_Handle_Message(e);
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

void lightLEDs(void)
{
    printf("-------Powering Up-------\n");
}

void flashLEDs(const turnstile_coin_t *coin)
{
    printf("Blinky blinky\n");
}

void soundOkay(const turnstile_person_t *person)
{
    printf("Welcome to the other side of the turnstile, %s.\n", person->name);
}

void soundAlarm(const turnstile_person_t *person)
{
    printf("BEEP BEEP! %s TRIED TO GET THROUGH WITHOUT PAYING!\n", person->name);
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
    turnstile_person(&nikky);
    turnstile_coin(NULL);
    turnstile_person(&nikky);
    return 0;
}
