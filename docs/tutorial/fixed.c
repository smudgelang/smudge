// Copyright 2017 Bose Corporation.
// This software is released under the 3-Clause BSD License.
// The license can be viewed at https://github.com/BoseCorp/Smudge/blob/master/LICENSE

#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include "initial_ext.h"
#include "initial.h"

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

int main(void)
{
    printf("Current state: %s\n", turnstile_Current_state_name());
    printf("Sending coin event.\n");
    turnstile_coin(NULL);
    printf("Current state: %s\n", turnstile_Current_state_name());
    printf("Sending person event.\n");
    turnstile_person(NULL);
    printf("Current state: %s\n", turnstile_Current_state_name());
    return 0;
}
