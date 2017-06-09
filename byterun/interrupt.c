#include "caml/platform.h"
#include "caml/interrupt.h"
#include "caml/domain.h"
#include <stdio.h>

/* Sending interrupts between domains.

   To avoid deadlock, some rules are important:

   - Don't hold interruptor locks for long
   - Don't hold two interruptor locks at the same time
   - Continue to handle incoming interrupts even when waiting for a response */

struct interrupt {
  struct interruptor* sender;
  interrupt_handler handler;
  void* data;
  atomic_uintnat completed;
};

void caml_init_interruptor(struct interruptor* s, atomic_uintnat* interrupt_word)
{
  s->interrupt_word = interrupt_word;
  caml_plat_mutex_init(&s->lock);
  caml_plat_cond_init(&s->cond, &s->lock);
  s->received = s->acknowledged = 0;
}

/* must be called with s->lock held */
static void handle_incoming(struct interruptor* s)
{
  while (s->received != s->acknowledged) {
    struct interrupt* req = s->messages[s->acknowledged & (Interrupt_queue_len - 1)];
    struct interruptor* sender = req->sender;
    s->acknowledged++;
    caml_plat_unlock(&s->lock);

    req->handler(caml_domain_self(), req->data);
    atomic_store_rel(&req->completed, 1);
    /* req is now invalid, and may have been deallocated */

    /* lock sender->lock so that we don't broadcast between check and wait */
    caml_plat_lock(&sender->lock);
    caml_plat_broadcast(&sender->cond);
    caml_plat_unlock(&sender->lock);

    caml_plat_lock(&s->lock);
  }
}

void caml_handle_incoming_interrupts(struct interruptor* s)
{
  caml_plat_lock(&s->lock);
  handle_incoming(s);
  caml_plat_unlock(&s->lock);
}

static const uintnat INTERRUPT_MAGIC = (uintnat)(-1); //FIXME dup
void caml_send_interrupt(struct interruptor* self,
                         struct interruptor* target,
                         interrupt_handler handler,
                         void* data)
{
  uintnat pos;
  struct interrupt req;
  int i;

  req.sender = self;
  req.handler = handler;
  req.data = data;
  atomic_store_rel(&req.completed, 0);

  caml_plat_lock(&target->lock);
  Assert (target->received - target->acknowledged < Interrupt_queue_len);
  pos = target->received++;
  target->messages[pos & (Interrupt_queue_len - 1)] = &req;
  /* Signal the condition variable, in case the target is
     itself waiting for an interrupt to be processed elsewhere */
  caml_plat_broadcast(&target->cond); // OPT before/after unlock? elide?
  caml_plat_unlock(&target->lock);

  atomic_store_rel(target->interrupt_word, INTERRUPT_MAGIC);

  /* Often, interrupt handlers are fast, so spin for a bit before waiting */
  for (i=0; i<1000; i++) {
    if (atomic_load_acq(&req.completed)) {
      return;
    }
    cpu_relax();
  }

  caml_plat_lock(&self->lock);
  while (1) {
    handle_incoming(self);
    if (atomic_load_acq(&req.completed)) break;
    caml_plat_wait(&self->cond);
  }
  caml_plat_unlock(&self->lock);
}
