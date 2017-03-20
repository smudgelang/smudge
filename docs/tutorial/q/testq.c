#include <stdbool.h>
#include <stdio.h>
#include "queue.h"

int main(void)
{
   queue_t *q;
   size_t s;
   int i, n;
   bool st;

   q = newq();
   if (q == NULL)
   {
      printf("Failed to allocate a queue.\n");
      return -10;
   }
   for (i = 0; i < 1000; i++)
   {
      s = size(q);
      if (s != (size_t)i)
      {
	 printf("Size mismatch: expected %d, got %zu.\n", i, s);
	 return -3;
      }
      st = enqueue(q, i);
      if (!st)
      {
	 printf("Failed to enqueue %d\n", i);
	 return -1;
      }
   }

   for (i = 0; i < 1000; i++)
   {
      st = dequeue(q, &n);
      if (!st)
      {
	 printf("Failed to dequeue %d\n", i);
	 return -2;
      }
      if (n != i)
      {
          printf("dequeued %d, expected %d.\n", n, i);
          return -4;
      }
   }

   s = size(q);
   if (s != 0)
   {
       printf("Size mismatch, expecting 0 got %zu", s);
       return -5;
   }

   for (i = 0; i < 1000; i++)
   {
       s = size(q);
       if (s != (size_t)i)
       {
           printf("Size mismatch: expected %d, got %zu.\n", i, s);
           return -3;
       }
       st = enqueue(q, i);
       if (!st)
       {
           printf("Failed to enqueue %d\n", i);
           return -1;
       }
   }

   if (!check(q))
   {
       printf("Failed cycle check.\n");
       return -100;
   }

   for (i = 0; i < 1000; i++)
   {
       st = dequeue(q, &n);
       if (!st)
       {
           printf("Failed to dequeue %d\n", i);
           return -2;
       }
       if (n != i)
       {
           printf("dequeued %d, expected %d.\n", n, i);
           return -4;
       }
   }

   s = size(q);
   if (s != 0)
   {
       printf("Size mismatch, expecting 0 got %zu", s);
       return -5;
   }


   freeq(q);
   q = newq();
   if (!enqueue(q, 1))
   {
       printf("Failed to enqueue 1.\n");
       return -101;
   }
   freeq(q);
   q = NULL;
   return 0;
}
