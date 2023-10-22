module PT3gr5

import StdEnv

// Write your Neptun code here:         QMIBHU
// NAME: BUI NGUYEN KIM HAI             


/* Given 2-D list of real numbers, sum all the numbers where the decimal part is only zero.
Example [[1.0, 3.0, 1.4], [3.53, 53.42, 3.41]] => [4.0, 0.0]   */

pt3 :: [[Real]] -> [Real]
pt3 [] = []
pt3 [x : xs] = [toReal(sum (filter (\x = toReal(toInt(x * 10.0) rem 10) == 0.0) x)): pt3 xs]

//Start = toInt((1.2) * 10.0) rem 10

//Start = pt3 [] //[]
//Start = pt3 [[1.2, 1.0, 1.0], [3.53, 53.42, 53.21, 78.0, 73.0, 47.4]] // [2.0, 151.0]
//Start = pt3 [[1.0],[1.8, 3.9], [4.0, 78.0, 67.9, 7.9, 6.0], [6.9]] // [1, 0, 88, 0]
