// Copyright 2017 Bose Corporation.
// This software is released under the 3-Clause BSD License.
// The license can be viewed at https://github.com/smudgelang/smudge/blob/master/LICENSE

#ifndef __QUEUE_H__
#define __QUEUE_H__

#include <stdbool.h>

typedef struct queue_s queue_t;

// Return a new queue.
queue_t *newq(void);

// Free the memory allocated for a queue.
void freeq(queue_t *head);

// Insert a new value into the queue, return true on success, false on failure.
bool enqueue(queue_t *head, void *value);

// Pop an element off the queue. Return true on success, false on failure.
bool dequeue(queue_t *head, void **value);

// Return the length of the queue.
size_t size(const queue_t *head);

#endif
