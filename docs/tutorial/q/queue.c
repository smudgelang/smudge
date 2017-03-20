#include <stdbool.h>
#include <string.h>
#include <stdlib.h>
#include "queue.h"

struct queue_s
{
    queue_t *next;
    int val;
    bool empty;
};

static queue_t *tail(queue_t *head)
{
    queue_t *cursor;
    for (cursor = head; cursor->next != NULL; cursor = cursor->next)
        ;
    return cursor;
}

static bool in(queue_t *head, int val)
{
    queue_t *cursor;

    if (head->empty)
        return false;
    for (cursor = head; cursor != NULL; cursor = cursor->next)
    {
        if (cursor->val == val)
            return true;
    }
    return false;
}

queue_t *newq(void)
{
    queue_t *node;

    node = malloc(sizeof(queue_t));
    if (node == NULL)
        return NULL;
    memset(node, 0, sizeof(*node));
    node->empty = true;
    return node;
}

void freeq(queue_t *head)
{
    queue_t *freeNext, *freeMe;

    freeMe = head;
    while (freeMe != NULL)
    {
        freeNext = head->next;
        free(freeMe);
        freeMe = freeNext;
    }
    return;
}

bool enqueue(queue_t *head, int value)
{
    queue_t *next;
    queue_t *parent;

    next = NULL;
    parent = tail(head);
    if (parent->empty == false)
    {
        next = newq();
        if (next == NULL)
            return false;
        parent->next = next;
    }
    else
    {
        next = parent;
    }
    next->empty = false;
    next->val = value;
    return true;
}

bool dequeue(queue_t *head, int *value)
{
    queue_t *oldNext;

    if (head->empty)
        return false;
    *value = head->val;
    if (head->next == NULL)
    {
        head->empty = true;
        return true;
    }

    oldNext = head->next;
    memcpy(head, oldNext, sizeof(queue_t));
    free(oldNext);
    return true;
}

size_t size(const queue_t *head)
{
    size_t count;
    const queue_t *cursor;

    count = 0;
    for (cursor = head; cursor != NULL; cursor = cursor->next)
    {
        if (!cursor->empty)
            count++;
    }
    return count;
}
