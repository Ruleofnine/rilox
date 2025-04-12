// Setup variables
var a = 0;
var b = 1;
var c = 2;
var d = 3;

// Basic increment tests
++a;
print a; // expect 1

a++;
print a; // expect 2

++b;
++b;
print b; // expect 3

b++;
print b; // expect 4

// Assignment chaining
a = b = c = d;
print a; // expect 3
print b; // expect 3
print c; // expect 3

// Mutation with assignment
a += 2;
print a; // expect 5

b -= 1;
print b; // expect 2

// Comma expression chaining
print (a = 1, b = 2, c = 3, d = 4, a + b + c + d); // expect 10

// More complex comma with mutation
print (a++, ++b, c += d, a + b + c); // expect a=2, b=3, c=7 → print 2 + 3 + 7 = 12

// Nested comma expression in grouping
print ( (a = 5, b = 6, a + b) + (c = 7, d = 8, c + d) ); // expect (5 + 6) + (7 + 8) = 11 + 15 = 26

// Mutation chains
++a;
++a;
++a;
print a; // expect 8

// Postfix mutation chains
a++;
a++;
a++;
print a; // expect 11

// Assignment inside comma chains
print (a = (b = (c = (d = 9, d + 1), c + 1), b + 1), a); // d=9, c=10, b=11, a=12 → print 12

// Arithmetic with assignment
a = b = c = d = 2;
print a + b + c + d; // expect 8

// Arithmetic with mutation
a += b += c += d;
print a; // c=4, b=6, a=8

// Grouped comma expressions
print ( (a = 1, b = 2, c = 3, d = 4, a + b) + (a = 5, b = 6, c = 7, d = 8, c + d) ); // (1+2)+(7+8)=3+15=18
print true ? true : fales;
// Redundant commas (valid, keeps final value)
print (a = 1, 2, 3, 4, 5); // expect 5
// Postfix + prefix + assignment combos
++a; //2
a++; //3
++b; //7
b++; //8
a += b;//3+8
print a; // depends on previous a/b values → a=2+3=5

print false ? invalid_variable : true;
var a;
print a;
if (false) {
  print true ; print true;
  print true ; print true;
  print true ; print true;
  print true ; print true;
} else {
  print a;
  print a;
  print a;
  print a;
}
