#include <stddef.h>
#include <stdio.h>
#include "initial.h"

int main(void)
{
    printf("Current state: %s\n", turnstyle_Current_state_name());
    printf("Sending coin event.\n");
    turnstyle_coin(NULL);
    printf("Current state: %s\n", turnstyle_Current_state_name());
    printf("Sending person event.\n");
    turnstyle_person(NULL);
    printf("Current state: %s\n", turnstyle_Current_state_name());
    return 0;
}
