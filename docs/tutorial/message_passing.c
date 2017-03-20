#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "queue.h"
#include "messages.h"
#include "messages_ext.h"

struct turnstyle_person_t
{
    const char *name;
};

static queue_t *q;

static void flushEventQueue(void)
{
    // This function could be running in a parallel thread
    // concurrently with the rest of the system. The important thing
    // is that it pops messages off the queue and sends them to
    // turnstyle_Handle_Message.
    bool success;
    turnstyle_Event_Wrapper *wrapper;

    while(size(q) > 0)
    {
        success = dequeue(q, (void **)&wrapper);
        if (!success)
        {
            fprintf(stderr, "Failed to dequeue element.\n");
            exit(-1);
        }
        // This actually sends the event into the state machine.
        turnstyle_Handle_Message(*wrapper);

        // This frees the event payload that's within the wrapper by
        // calling SMUDGE_free on it.
        turnstyle_Free_Message(*wrapper);

        // We still need to free the copy of the wrapper itself, since
        // it was malloc'd in turnstyle_Send_Message.
        free(wrapper);
    }
}

static turnstyle_person_t *newPerson(const char *name)
{
    turnstyle_person_t *person;

    person = malloc(sizeof(turnstyle_person_t));
    if (person == NULL)
    {
        fprintf(stderr, "Failed to allocate space for %s.\n", name);
        exit(-1);
    }
    person->name = name;
    return person;
}

void turnstyle_Send_Message(turnstyle_Event_Wrapper e)
{
    bool success;
    turnstyle_Event_Wrapper *wrapper;

    // The event wrapper is passed in on the stack, so we have to
    // allocate some memory that we can put in the message queue.
    wrapper = malloc(sizeof(turnstyle_Event_Wrapper));
    if (wrapper == NULL)
    {
        fprintf(stderr, "Failed to allocate wrapper memory.\n");
        exit(-1);
    }
    memcpy(wrapper, &e, sizeof(turnstyle_Event_Wrapper));

    // Put the event on the queue, to be popped off later and handled
    // in order.
    success = enqueue(q, wrapper);
    if (!success)
    {
        fprintf(stderr, "Failed to enqueue message.\n");
        exit(-1);
    }
}

void SMUDGE_free(const void *a1)
{
    free((void *)a1);
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

void flashLEDs(const turnstyle_coin_t *coin)
{
    printf("Blinky blinky\n");
}

void soundOkay(const turnstyle_person_t *person)
{
    printf("Welcome to the other side of the turnstyle, %s.\n", person->name);
}

void soundAlarm(const turnstyle_person_t *person)
{
    printf("BEEP BEEP! %s TRIED TO GET THROUGH WITHOUT PAYING!\n", person->name);
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
    turnstyle_person_t *nikky, *cheater;

    q = newq();
    if (q == NULL)
    {
        fprintf(stderr, "Failed to get a queue.\n");
        return -1;
    }
    nikky = newPerson("Nikola");
    cheater = newPerson("Thomas");

    turnstyle_person(cheater);
    turnstyle_coin(NULL);
    turnstyle_person(nikky);

    flushEventQueue();
    freeq(q);
    return 0;
}
