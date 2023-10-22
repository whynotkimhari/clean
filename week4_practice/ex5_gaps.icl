module ex5_gaps
import StdEnv

// 1. Multiply the digits of a number e.g. for 123 is 6.

prodDigits :: Int -> Int
prodDigits x 
| x < 10 = x
= (x rem 10) * prodDigits (x/10) 


//Start = prodDigits 503 // 0
//Start = prodDigits 54213 // 120



// 2. Write a function that takes two arguments, say n and x, and computes n*x by recursion.

prec :: Int Int -> Int
prec x n
| n == 0 = 0
= x + prec x (n - 1)


//Start = prec 10 2 // 20



// 3. Sum numbers from 12..N in a recursive function, where N is positive.

fn :: Int -> Int
fn n
| n <= 11 = abort " N can not be zero or negative or less then 11"
| n == 12 = 12
= n + fn (n-1)


//Start = fn -10 //  N can not be zero or negative or less then 11
//Start = fn 14  // 39



// 4. Compute factorial n recursively, where n! = n*(n-1)!.

factor :: Int -> Int
factor w
| w == 0 = 1
| otherwise = w * factor (w-1)


//Start = factor 5 // 120



// 5. sumsq n returns 1+1 + 2+2 + ... + n+n - with a pattern for 0

sumsq :: Int -> Int
sumsq 0 = 0
sumsq n = 2 * n + sumsq (n-1)


//Start = sumsq 3 // 12
  


// 6. Compute for a given positive x the sum of 5x(2y+1), for i from 1 to x, and a given y.

f :: Int Int -> Int
f 0 a = 0
f x a = 5*x*(2*a+1) + f (x-1) a


//Start = f 5 10 // 1075



// 7. Compute the sum 1+ 1*2 + 1*2*3+ 1*2*3*4+ 1*2*3*4*5+ ...+1*2*3*...*n 
// where n is a positive number.

sump :: Int -> Int
sump 0 = 0
sump n = prod [1..n] + sump (n-1) 


//Start = sump 5 // 153



// 8. Cut a list in 4 parts quarter, middle, third quarter. 
// E.g. cut [1..10] -> [[1,2], [3,4,5], [6,7], [8,9,10]]

cut :: [Int] -> [[Int]]
cut x = [ take z x, drop z (take y x), take u w, drop u w ]
where y = ((length x) / 2)
      z = y/2
      w = drop y x
      u = ((length w) / 2)


//Start = cut [1..10] // [[1,2],[3,4,5],[6,7],[8,9,10]]
//Start = cut [1..11] // [[1,2],[3,4,5],[6,7,8],[9,10,11]]
//Start = cut []
//Start = cut [21]
//Start = cut [1..21] // [[1,2,3,4,5],[6,7,8,9,10],[11,12,13,14,15],[16,17,18,19,20,21]]




// 9. Extract the third element of a non-empty list. 

m2 :: [Int] -> Int
m2 [] = abort "your list is empty"
m2 x = x !! 2


//Start = m2 [1..5] 
//Start = m2 [1..4] 
//Start = m2 [1]
//Start = m2 []



// 10. Triple every element of a list

f1 :: [Int] -> [Int]
f1 [] = []
f1 [x : xs] = [ x * 3 : f1 xs]


//Start = f1 [1,5,3,1,6]  // [3,15,9,3,18]



// 11. Compute the square of positives and change the sign of negatives.

f2 :: [Int] -> [Int]
f2 [] = []
f2 [u:us]
| u > 0 = [u*u : f2 us]
= [~u : f2 us]


//Start = f2 [1, 2, 0, -2, 3, -4] // [1,4,0,2,9,4]



// 12. write a function that keeps the integers of a list up to the length of the second 
// if not found all list must returned.

f3 :: [Int] [Int] -> [Int]
f3 [] list = []
f3 [x:xs] list
| x == y = []
= [ x : f3 xs list]
where y = length list


//Start = f3 [1, 2, -2, 3, 5, 0, -4] [1..8] // [1,2,-2,3,5,0,-4]
//Start = f3 [1, 2, -2, 3, 5, 0, -4] [1..5] // [1,2,-2,3]