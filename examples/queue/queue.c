/*
 * Bounded queues implemented as circular arrays
 * NB: this implementation is buggy!
 */

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

typedef struct _queue {
  size_t capacity;  // max number of elements
  int front;        // index of front
  int rear;         // index of rear 
  int *arr;         // elements
} *Queue;


/* Create a new queue
 */
Queue new(size_t size)
{
  assert(size >= 1);
  int *arr = malloc(size*sizeof(int));
  Queue q = malloc(sizeof(struct _queue));
  assert(arr != NULL && q != NULL);
  q->capacity = size;
  q->front = q->rear = 0;
  q->arr = arr;
  return q;
}

/* Delete a queue
 */
void delete(Queue q) {
  free(q -> arr);
  free(q);
}

int is_empty(Queue q)
{
  return (q->front == q->rear);
}

int is_full(Queue q) {
  return ((q->rear == q->front - 1) ||
	  (q->rear == q->capacity-1 && q->front == 0));
}

void enqueue(Queue q, int v)
{
  /* assert(!is_full(q)); */
  q->arr[q->rear] = v;
  q->rear = (q->rear+1)%q->capacity;
}

int dequeue(Queue q)
{
  /* assert(!is_empty(q)); */
  int val = q->arr[q->front];
  q->front = (q->front+1)%q->capacity;
  return val;
}



  
