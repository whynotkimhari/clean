module PT2gr5
import StdEnv

/* Your name and neptun code : BUI NGUYEN KIM HAI - QMIBHU       */

/*1. Given n and x, write a function to compute the sum
n*(x-1) + (n-1)*(x-2) + ... + 2*(x-3) + 1*(x-2)

*/

sums :: Int Int -> Int
sums 0 _ = 0
sums 1 x = x - 1
sums n x = n * (x - 1) + sums (n - 1) (x - 1)

//Start = sums 3 3 // 8
// 3*2 + 2*1 + 1*0 

//Start = sums 5 10 //115
// 5*9 + 4*8 + 3*7 + 2*6 + 1*5 

//Start = sums 2 0 // -4

//Start = sums 10 10 // 330

//Start = sums 5 1 // -20

//Start = sums 0 0 // 0

Start = (sums 0 0, sums 5 1, sums 10 10, sums 2 0, sums 5 10, sums 3 3)