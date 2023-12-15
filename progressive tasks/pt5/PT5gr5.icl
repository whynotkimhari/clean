module PT5gr5
import StdEnv


/* 
WRITE NAME AND NEPTUN:	 BUI NGUYEN KIM HAI --- QMIBHU

You are given a list of lists and a number. 
If the given number is inside the interval 
of the sublist, then transform that sublist 
by joining the two numbers into one integer.

Example:
6 is not in the interval [1,2] and [1,4] so remove these sublists
6 is inside (10,5) so join -> 105 
6 is inside 

pt54 [[1,2], [1,4], [10, 5], [3,8]] 6 = [105, 38] */


pt54 :: [[Int]] Int -> [Int]
pt54 lists num = [toInt(toString(hd list) +++ toString(last list)) \\ list <- lists | list <> [] &&( (num >= (hd list) && num <= (last list)) || (num >= (last list) && num <= (hd list)))]

//Start = pt54 [[1,2], [1,4], [10, 5], [3,8]] 6 //[105,38]
//Start = pt54 [[2,3], [4,5], [5,6], [6,7], [8,8]] 9 // []
//Start = pt54 [[1,0], [0,1]] 1 // [10, 1]
//Start = pt54 [[]] 0 // []
//Start = pt54 [] 10 // []