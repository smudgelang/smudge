#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <pthread.h>
#include "turnstyle.h"
#include "turnstyle_ext.h"

pthread_t thread;

struct turnstyle_person_t
{
    char *name;
};

struct coinInspector_validate_t
{
    unsigned int signature;
};

static void *sendValidCoin(void *unused)
{
    sleep(1);
    coinInspector_valid(NULL);
    return NULL;
}

extern void turnstyle_Send_Message(turnstyle_Event_Wrapper e)
{
    turnstyle_Handle_Message(e);
}

void SMUDGE_free(const void *thing)
{
    free((void *)thing);
}

void SMUDGE_panic(void)
{
    printf("Game over!\n");
    exit(-1);
}

void lightLEDs(void)
{
    printf("-------Powering Up-------\n");
}

void flashLEDs(const turnstyle_coin_t *coin)
{
    printf("Blinky blinky\n");
}

void soundOkay(const turnstyle_person_t *person)
{
    printf("Welcome to the other side of the turnstyle, %s.\n", person->name);
}

void soundAlarm(void)
{
    printf("BEEP BEEP! ALARM! ALARM!\n");
}

void lockedEnter(void)
{
    printf("Bar the gate!\n");
}

void lockedExit(void)
{
    printf("Leaving the locked state.\n");
}

void unlockedEnter(void)
{
    printf("Unlocking the turnstyle.\n");
}

void validateCoin(const coinInspector_validate_t *coin)
{
    printf("Looking at the coin in question.\n");
    pthread_create(&thread, NULL, sendValidCoin, NULL);
}

int main(void)
{
    turnstyle_person_t nikky;

    nikky.name = "Nikola";
    turnstyle_person(&nikky);
    turnstyle_coin(NULL);
    pthread_join(thread, NULL);
    turnstyle_person(&nikky);
    turnstyle_tilt(NULL);
    turnstyle_coin(NULL);
    turnstyle_tilt(NULL);
    return 0;
}
