@interface NSNumber
+ (NSNumber *)numberWithInt:(int)value;
@end

@protocol NSCopying @end
typedef unsigned long NSUInteger;

@interface NSDictionary
+ (id)dictionaryWithObjects:(const id [])objects
                    forKeys:(const id <NSCopying> [])keys
                      count:(NSUInteger)cnt;
@end

@interface NSString<NSCopying>
@end

void f() {
  NSDictionary *dict1 = @{ @"toto":@111 };
  NSDictionary *dict2 = @{ @"toto":@111, @"titi":@222 };
}
