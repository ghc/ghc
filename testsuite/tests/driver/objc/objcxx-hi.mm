#import <Foundation/Foundation.h>
#import <iostream>

@interface HelloWorld : NSObject {
  // no instance variables
}

// methods
- (void)sayHello;

@end

@implementation HelloWorld

- (void)sayHello
{
  std::cout << "Hello world" << std::endl;
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
