
typedef enum memory_order {
  memory_order_relaxed, memory_order_consume, memory_order_acquire,
  memory_order_release, memory_order_acq_rel, memory_order_seq_cst
} memory_order;

void fi1a(int *i) {
  int v;
  __atomic_load(i, &v, memory_order_seq_cst);
}
