// Copyright 2017 Bose Corporation.
// This software is released under the 3-Clause BSD License.
// The license can be viewed at https://github.com/BoseCorp/Smudge/blob/master/LICENSE

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "queue.h"
#include "default_states.h"
#include "default_states_ext.h"

typedef enum
{
    TURNSTYLE,
    COIN_INSPECTOR
} sm_id_t;

struct turnstile_person_t
{
    const char *name;
};

struct turnstile_coin_t
{
    int diameter;
    bool ridges;
    const char *text;
};

typedef struct
{
    sm_id_t sm;
    union
    {
        turnstile_Event_Wrapper turnstile;
        coinInspector_Event_Wrapper coinInspector;
    } wrapper;
} system_message_t;

static queue_t *q;

static void flushEventQueue(void)
{
    // This function could be running in a parallel thread
    // concurrently with the rest of the system. The important thing
    // is that it pops messages off the queue and sends them to
    // turnstile_Handle_Message.
    bool success;
    system_message_t *msg;

    while(size(q) > 0)
    {
        success = dequeue(q, (void **)&msg);
        if (!success)
        {
            fprintf(stderr, "Failed to dequeue element.\n");
            exit(-1);
        }
        switch(msg->sm)
        {
        case TURNSTYLE:
            // This actually sends the event into the state machine.
            turnstile_Handle_Message(msg->wrapper.turnstile);

            // This frees the event payload that's within the wrapper by
            // calling SMUDGE_free on it.
            turnstile_Free_Message(msg->wrapper.turnstile);
            break;
        case COIN_INSPECTOR:
            // This could just as easily have been implemented as a
            // separate queue running in a different thread.
            coinInspector_Handle_Message(msg->wrapper.coinInspector);
            coinInspector_Free_Message(msg->wrapper.coinInspector);
            break;
        }
        // We still need to free the copy of the wrapper itself, since
        // it was malloc'd in turnstile_Send_Message.
        free(msg);
    }
}

static turnstile_person_t *newPerson(const char *name)
{
    turnstile_person_t *person;

    person = malloc(sizeof(turnstile_person_t));
    if (person == NULL)
    {
        fprintf(stderr, "Failed to allocate space for %s.\n", name);
        exit(-1);
    }
    person->name = name;
    return person;
}

void turnstile_Send_Message(turnstile_Event_Wrapper e)
{
    bool success;
    system_message_t *msg;
    turnstile_Event_Wrapper *wrapper;

    // The event wrapper is passed in on the stack, so we have to
    // allocate some memory that we can put in the message queue.
    msg = malloc(sizeof(system_message_t));
    if (msg == NULL)
    {
        fprintf(stderr, "Failed to allocate message memory.\n");
        exit(-1);
    }
    msg->sm = TURNSTYLE;
    wrapper = &msg->wrapper.turnstile;
    memcpy(wrapper, &e, sizeof(e));

    // Put the event on the queue, to be popped off later and handled
    // in order.
    success = enqueue(q, msg);
    if (!success)
    {
        fprintf(stderr, "Failed to enqueue message.\n");
        exit(-1);
    }
}

void coinInspector_Send_Message(coinInspector_Event_Wrapper e)
{
    bool success;
    system_message_t *msg;
    coinInspector_Event_Wrapper *wrapper;

    // The event wrapper is passed in on the stack, so we have to
    // allocate some memory that we can put in the message queue.
    msg = malloc(sizeof(system_message_t));
    if (msg == NULL)
    {
        fprintf(stderr, "Failed to allocate message memory.\n");
        exit(-1);
    }
    msg->sm = COIN_INSPECTOR;
    wrapper = &msg->wrapper.coinInspector;
    memcpy(wrapper, &e, sizeof(e));

    // Put the event on the queue, to be popped off later and handled
    // in order.
    success = enqueue(q, msg);
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

void SMUDGE_panic_print(const char *fmt, const char *st, const char *ev)
{
    fprintf(stderr, fmt, st, ev);
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

void soundAlarm(void)
{
    printf("BEEP BEEP! ALARM! ALARM!\n");
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

void validateCoin(const coinInspector_validate_t *coin)
{
    static bool tick = false;
    printf("Looking at the coin in question.\n");

    // Every other coin is good. Who needs real validation when we can
    // just accept 50% of them?
    if (tick)
    {
        coinInspector_invalid(NULL);
    }
    else
    {
        coinInspector_valid(NULL);
    }
    tick = !tick;
}

int main(void)
{
    turnstile_person_t *nikky;

    q = newq();
    if (q == NULL)
    {
        fprintf(stderr, "Failed to get a queue.\n");
        return -1;
    }

    nikky = newPerson("Nikola");
    turnstile_person(nikky);
    turnstile_coin(NULL); // Good
    flushEventQueue();
    nikky = newPerson("Nikola");
    turnstile_person(nikky);
    turnstile_coin(NULL); // No good
    flushEventQueue();
    turnstile_tilt(NULL);
    flushEventQueue();
    return 0;
}
