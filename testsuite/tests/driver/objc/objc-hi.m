#import <Foundation/Foundation.h>
#import <stdio.h> 

@interface HelloWorld : NSObject {
  // no instance variables
}

// methods
- (void)sayHello;

@end

@implementation HelloWorld

- (void)sayHello
{
  printf("Hello world\n");
}

@end

#import <Foundation/Foundation.h>

int main (int argc, const char * argv[]) {
  NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];

  // my stuff
  HelloWorld *hw = [[HelloWorld alloc] init];
  [hw autorelease];
  
  [hw sayHello];
      
  [pool release];
  return 0;
}
