@interface NSObject
@end

@interface NSArray : NSObject
+ (id)arrayWithObjects:(const id [])objects count:(int)cnt;
@end

int main() {
  NSArray * l = @[ @"1" ];
  NSArray * m = @[ @"1", @"2" ];
}
