// Copyright 2017 Bose Corporation.
// This software is released under the 3-Clause BSD License.
// The license can be viewed at https://github.com/Bose/Smudge/blob/master/LICENSE

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "queue.h"
#include "side_effects.h"
#include "side_effects_ext.h"

struct turnstile_person_t
{
    const char *name;
};

static queue_t *q;

static void flushEventQueue(void)
{
    // This function could be running in a parallel thread
    // concurrently with the rest of the system. The important thing
    // is that it pops messages off the queue and sends them to
    // turnstile_Handle_Message.
    bool success;
    turnstile_Event_Wrapper *wrapper;

    while(size(q) > 0)
    {
        success = dequeue(q, (void **)&wrapper);
        if (!success)
        {
            fprintf(stderr, "Failed to dequeue element.\n");
            exit(-1);
        }
        // This actually sends the event into the state machine.
        turnstile_Handle_Message(*wrapper);

        // This frees the event payload that's within the wrapper by
        // calling SMUDGE_free on it.
        turnstile_Free_Message(*wrapper);

        // We still need to free the copy of the wrapper itself, since
        // it was malloc'd in turnstile_Send_Message.
        free(wrapper);
    }
}

static void initializeEventQueue(void)
{
    q = newq();
    if (q == NULL)
    {
        fprintf(stderr, "Failed to get a queue.\n");
        exit(-1);
    }
}

void turnstile_Send_Message(turnstile_Event_Wrapper e)
{
    bool success;
    turnstile_Event_Wrapper *wrapper;

    // The event wrapper is passed in on the stack, so we have to
    // allocate some memory that we can put in the message queue.
    wrapper = malloc(sizeof(turnstile_Event_Wrapper));
    if (wrapper == NULL)
    {
        fprintf(stderr, "Failed to allocate wrapper memory.\n");
        exit(-1);
    }
    memcpy(wrapper, &e, sizeof(turnstile_Event_Wrapper));

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

void sendEvents(void)
{
    turnstile_person_t nikky, cheater;

    nikky.name = "Nikola";
    cheater.name = "Thomas";

    turnstile_person(&cheater);
    turnstile_coin(NULL);
    turnstile_person(&nikky);
}

int main(void)
{

    initializeEventQueue();

    sendEvents();

    flushEventQueue();
    freeq(q);
    return 0;
}
