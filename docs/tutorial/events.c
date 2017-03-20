#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include "initial_ext.h"
#include "initial.h"

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
