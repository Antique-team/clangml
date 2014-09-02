
// @interface NSNumber
// + (NSNumber *)numberWithInt:(int)value;
// @end

// @protocol NSCopying @end
// typedef unsigned long NSUInteger;

// @interface NSDictionary
// + (id)dictionaryWithObjects:(const id [])objects
//                     forKeys:(const id <NSCopying> [])keys
//                       count:(NSUInteger)cnt;
// - (Class)isa;
// @end

// @interface NSString<NSCopying>
// @end

// void f() {
//   NSDictionary *dict1 = @{ @"toto":@111 };
//   Class isa = [dict1 isa];
// }

typedef struct objc_class *Class;

typedef struct objc_object {
    Class isa;
} *id;

// rdar 7609722
@interface Foo { 
@public 
  id isa; 
} 
+(id)method;
@end

id Test2() {
    if([Foo method]->isa)
      return (*[Foo method]).isa;
    return [Foo method]->isa;
}
