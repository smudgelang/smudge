// Copyright 2017 Bose Corporation.
// This software is released under the 3-Clause BSD License.
// The license can be viewed at https://github.com/BoseCorp/Smudge/blob/master/LICENSE

#include <stddef.h>
#include <stdio.h>
#include "initial.h"

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
