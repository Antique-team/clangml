@interface I0 { 
@public
  unsigned y: 1;
} 
@end

int f0(I0 *a) {
  return a->y;
}
