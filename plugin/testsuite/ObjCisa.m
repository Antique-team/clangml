typedef struct objc_class *Class;

typedef struct objc_object {
  Class isa;
} *id;

@interface Foo {
@public
  id isa;
}
+(id)method;
@end

id Test2() {
  return [Foo method]->isa;
}
