module tmp

import StdEnv

// Today is: 21/09/2023

f :: Int -> Int
f x = x*2

//Start = f 42

f2 :: Int -> Int
f2 x = x*3

g :: Int -> Int
g x = x / 2

gr :: Real -> Real
gr x = x / 2.0



h :: Int Int -> Int
h a b = a + b

//Start = h 10 5  /// = 15

h3 :: Int Int Int -> Int
h3 x y z = x + 2*y*z

//Start = h3 1 2 3  /// = 13

h4 :: Real Real Real -> Int
h4 x y z = toInt(x + 2.0*y*z)

//Start = h4 1.2 1.2 1.2 /// = 4

b :: Bool -> Bool
b x = not x

//Start = b True /// = False

b2 :: Bool -> Bool
b2 x = not (not x)

//Start = b2 True

s :: String String String -> String
s a b c = a +++ b +++ c

//Start = s "Hello" ", my name is " "Kim" /// = "Hello, my name is Kim"

c :: Char -> Char
c x = x

//Start = c '$'  /// = '$'

//Start = abs -4 // Built-in func

div :: Int -> Int
div x = x/2

divr :: Real -> Real
divr x = x/2.0
//Start = (div 5, divr 5.0, div 10, divr 10.0) // (2, 2.5, 5, 5)

divb :: Int Int -> Int
divb x y = x/y

//Start = divb 5 3 /// = 1

divbr :: Int Int -> Real
divbr x y = (toReal x)/(toReal y)

//Start = divbr 5 3 // = 1.6667

divbr2 :: Int Int -> Real
divbr2 x y = toReal(x/y)

//Start divbr2 5 3 // = 1 (because of too late converting)

isEven :: Int -> Bool
isEven a = (a rem 2 == 0)  // rem is short-hand for Reminder

//Start = isEven 23 // == False

//Too long way to solve, not recommend way!
isEvenWr :: Int -> Bool
isEvenWr a
|(a rem 2 == 0) = True
| otherwise = False

//Start = isEvenWr 23

// Second way for checking even-odd value
isEven2 :: Int -> Bool
isEven2 a = (a/2)*2 == a

//Start = isEven2 23

// ** NOTE THAT ** isEven(camel case) is also a built-in func

grandma :: Real Real Real Real Real -> Real
grandma a o p s b = a*500.0 + o*800.0 + p*150.5 + s*4000.0 + b*700.0

//Start = grandma 5.0 7.0 10.0 2.0 1.0 // = 18305

// x^2 + 2x + 1. with x = 5, what is the value ?
eq :: Int -> Int
eq x = x^2 + 2*x + 1 // ^ is caret

//Start = eq 5 // = 36

// n-th power of x
qa :: Int Int -> Int
qa x n = x^n

//Start = qa 4 4 // = 256

quad :: Real Real Real Real -> Real
quad a b c x = a*x*x + b*x + c

//Start = quad 1.0 2.0 1.0 3.0 // = 16

ME = 8848
K2 = 8611

dist :: Int -> Int
dist x = (ME - K2) + x

//Start = dist 1000 // = 1237

eq4 :: Int -> Int
eq4 x = x^4 + 3*x^3 + 4*x^2 + x - 10

//Start = eq4 10 // = 13400

ap :: Real Real Real -> Real
ap x a n = x*(a + 1.0)^n

Start = ap 200.0 0.1 6.0 // = 354.3122