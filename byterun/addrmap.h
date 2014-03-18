#include "mlvalues.h"

struct addrmap_entry { uintnat key, value; };
struct addrmap {
  struct addrmap_entry* entries;
  uintnat size;
};

#define ADDRMAP_INIT {0,0}


uintnat caml_addrmap_lookup(struct addrmap* t, value v);

#define ADDRMAP_NOT_PRESENT ((uintnat)(-1))


uintnat* caml_addrmap_insert_pos(struct addrmap* t, value v);


void caml_addrmap_clear(struct addrmap* t);
