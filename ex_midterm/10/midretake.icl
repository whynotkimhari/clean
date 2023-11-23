module midretake

import StdEnv

/*-- Functional Programming mid-term retake, 2023. June 7.
-- This solution was submitted and prepared by
   WRITE NAME AND NEPTUN HERE!!
-- <name, neptun> for the Functional Programming course.

-- I declare that this solution is my own work.
-- I have not copied or used third-party solutions.
-- I have not passed my solution to my classmates, 
   neither made it public.
-- The most serious consequence of a disciplinary fault 
   is dismissal from the University.
   
Each task is of 10 points. */

/* 1. Buy choco
You are sent to the store to buy exactly two chocolates.
In a list you have all the chocolate prices, and in an Int the amount 
of money you have. You want to minimize the cost and buy the 2 cheapest. 
If you do not have enough money to buy the two cheapest chocolates 
you just return your money. If you have, return the money you will have
after the purchase. 
   [1,2,2] 3  1+2=3 left money 0
   [3,2,2] 3  2+274 not enough money, return original amount 3 */

buyChoco :: [Int] Int -> Int
buyChoco list num 
| sum <= num = num - sum
= num
where sortedList = sort list
	  sum = sortedList !! 0 + sortedList !! 1

//Start = buyChoco [1,2,2] 3 // 0
//Start = buyChoco [3,2,2,8,10] 3 // 3 (You cannot buy two choco)


/* 2. Sum cargo
You are given a list of cargo (id, weight) tuples. 
Return a list of tuples (id, sum_of_weight)
after you sum the weight of cargos with the same id. 
   [(1,1),(4,5),(3,8),(3,1),(1,5),(1,7)] -> [(1,1+5+7),(4,5),(3,8+1)] */

cargo :: [(Int, Int)] -> [(Int, Int)]
cargo list = removeDup [(id, sum ( map (\(x,y) = y ) (filter (\(x,y) = x == id) list))) \\ (id,w) <- list]

//Start = cargo [(1,1),(4,5),(3,8),(3,1),(1,5),(1,7)] // [(1,13),(4,5),(3,9)]
//Start = cargo [(1,3),(2,2),(7,1),(2,2),(1,4)] // [(1,7),(2,4),(7,1)]


/* 3. Employee max working hour
You are given a list of task tuples of (id, finishTime) 
where id is the employee id who worked on this task
and a finishTime which is the time the employee finished his task. 
The ith task starts right after the (i-1)th task. 
Return the first id of the employee that worked on the task during 
the longest time.
   [(0,3), (2,5), (0,6), (1,8), (2,9)]
   employee 0 worked 3-0=3 and 6-5=1 hours
   employee 1 worked 8-6=2 hours
   employee 2 worked 5-3=2 and 9-8=1 hours 
   the max working hour is 3, and the employee id is 0 */
f :: [Int] -> [Int]
f [x] = []
f [x,y:xs] = [y-x: f [y:xs]]

longestWork :: [(Int,Int)] -> Int
longestWork list = snd (maxList (removeDup [(maxList (map (\(x,y) = y) (filter (\(x,y) = x == id) new_list)), id) \\ (id,h) <- new_list]))
where listA = fst (unzip list)
	  listB = f ([0] ++ snd (unzip list))
	  new_list = zip (listA,listB)

//Start = longestWork [(0,3), (2,5), (0,6), (1,8), (2,9)] // 0
//Start = longestWork [(0,3), (2,5), (0,9), (1,15)] // 1


/* 4. Beauty number
Find the beauty number of the given number and a k number of digits.
Beauty number of a number is taking all possible continuous subparts of a number 
with k digits counting how many of them is a divisor of the given number.
Given the 2400 and 2 continuous digits:
   (24)00 -> 24 is a divisor of 2400
   2(40)0 -> 40 is a divisor of 2400
   24(00) -> 0 is not a divisor of 2400, so return 2 
Make sure you are not dividing by 0 and handle that case. */
numToList :: Int -> [Int]
numToList num = reverse (map (\x = x rem 10) (takeWhile ((<>)0) (iterate (\x = x/10) num)))

listToNum :: [Int] -> Int
listToNum [] = 0
listToNum [x:xs] = x*(10^(length xs)) + listToNum xs

getPart :: [Int] Int -> [Int]
getPart list k = [listToNum [list !! j \\ j <- [i..(i+k-1)]] \\ i <- [0..((length list) - k)]]

beautyNum :: Int Int -> Int
beautyNum num k = length (filter (\x = x <> 0 && num rem x == 0) (getPart list k))
where list = numToList num

//Start = beautyNum 2400 2 // 2
//Start = beautyNum 430043 2 // 2
//Start = beautyNum 112233 1 // 4


/* 5. Different digits 
Write a function that given an integer n and k, checks if the number n 
contains maximum k or less different digits. 
   k=2 334343 has 2 different digits, return True
       5566 has 2 different digits, return True
       120 has 3 different digits, return False 
       9893487 has 5 different digits return False */

diffDigit :: Int Int -> Bool
diffDigit num k = length (removeDup (numToList num)) <= k

//Start = diffDigit 121 2 // True
//Start = diffDigit 78938647 34 // True
//Start = diffDigit 343 1 // False
//Start = diffDigit 434234567 2 // False


/* 6. Triangular sum
Given a list of integers find the triangular sum of the list by
summing the neighbouring numbers until there is only one number left.
Assume the list is not empty.
e.g. 1 	2 	3 	4 	5
	  3   5   7   9
	    8   12  16
	       20 28
	        48       */
	        
reduce :: [Int] -> [Int]
reduce [x] = []
reduce [x,y:xs] = [x+y : reduce [y:xs]]
	         
triSum :: [Int] -> Int
triSum [x] = x
triSum list = triSum (reduce list)

//Start = triSum [1,2,3,4,5] // 48
//Start = triSum [45,3,68,2,4,89] // 869
//Start = triSum [8] // 8

  
/* 7. Tournament
You are organizing a football tournament and you have n teams,
calculate the number of matches until a winner is decided.
   If n is even, they play in pairs and the winners advance to the next round
   If n is odd, then the odd team automatically goes to the next round,
   the rest play with each other
Return the total number of matches.
8 -> 4 matches 4 winners, 2 matches 2 winners, 1 match with 1 winner - 4+2+1=7 matches
7 -> 3 matches 3+1 winners, 2 matches 2 winners, 1 match 1 winner - 3+2+1=6 matches */

countMatch :: Int -> Int
countMatch x
| x == 1 = 0
| x rem 2 == 0 = x / 2 + countMatch (x/2)
= x / 2 + 1 + countMatch (x/2)

//Start = countMatch 7 // 6
//Start = countMatch 8 // 7
//Start = countMatch 14 // 13


/* 8. Rewarded mice
There are two mice and n different types of cheese.
You are given two positive integer lists reward1 and reward2, and a non-negative integer k.
The best rewarded point for a cheese type is the max of the values of same position in 
the lists. Return the maximum points the mice can achieve if they eat exactly k types 
of the most rewarded cheese. 
e.g [1,10,3,4,0] [1,4,2,1,2] 4, max is [1,10,3,4,2] the best 4 points are 10+3+4+2=19 */

cheese :: [Int] [Int] Int -> Int
cheese listA listB num = sum (take num (reverse (sort [maxList [a,b] \\ a <- listA & b <- listB])))

//Start = cheese [1,10,3,4,0] [1,4,2,1,2] 4 // 19
//Start = cheese [1..5] [1..5] 4 // 14 
	    
	    
/* 9. Gifts
You are given an integer list of gifts denoting the number of gifts in various piles.
Return the number of gifts remaining after k seconds which is given as second argument.
Every second, you do the following:
	Choose the pile with the maximum number of gifts.
	Change the max value, leave instead the square root transformed to Int of max.
	Keep the rest of the gifts. 
	Repeat until k seconds elapsed. 
[25,64,9,4,100] 4s -> [25,64,9,4,10] 3s -> [25,8,9,4,10] 2s -> [5,8,9,4,10] 1s -> 
[5,8,9,4,3] 0s -> sum [5,8,9,4,3] -> 29 */

pickGift :: [Int] Int -> Int
pickGift list num 
| num == 0 = sum list
= pickGift (init sortedList ++ [new_tail]) (num-1) 
where sortedList = sort list
	  new_tail = toInt(sqrt (toReal (last sortedList)))

//Start = pickGift [25,64,9,4,100] 4 // 29
//Start = pickGift [1,1,1,1] 4 // 4
//Start = pickGift [23,45,6,2,4,6] 10 // 10	    


/* 10. Binary numbers
Write a function that takes a binary number and returns the decimal equivalent,
calculated by multiplying each digit by 2 raised to the power of its position.
   1011 -> 1 * 2^3 = 8
           0 * 2^2 = 0
           1 * 2^1 = 2
           1 * 2^0 = 1 -> 8+0+2+1=11  */

binToDec :: Int -> Int
binToDec num = sum [b*(2^p) \\ b <- list & p <- [start,(start - 1)..0]]
where list = numToList num
	  start = (length list) - 1

//Start = binToDec 1011 //11
//Start = binToDec 10110 //22
//Start = binToDec 0//0


/* 11. Circular sum
Write a function that takes two lists of integers and returns a list of integers
where each integer of list 1 is added to each integer of list 2 in a circular basis.
   [1,1,1,1,1,1] [1,2,3] -> [1+1,1+2,1+3,1+1,1+2,1+3] = [2,3,4,2,3,4]
   [0,0,0,0,0,0,0] [4,6] -> [0+4,0+6,0+4,0+6,0+4,0+6,0+4] = [4,6,4,6,4,6,4]  */

rotateSum :: [Int] [Int] -> [Int]
rotateSum _ [] = []
rotateSum [] _ = []
rotateSum listA listB = [x+y \\ x <- listA & y <- flatten (repeatn n listB)]
where n = (length listA) / (length listB)

//Start = rotateSum [1,1,1,1,1,1] [1,2,3] //[2,3,4,2,3,4]
//Start = rotateSum [0,0,0,0,0,0,0] [4,6] //[4,6,4,6,4,6,4]
//Start = rotateSum [1,2,3] [4,5,6] //[5,7,9]
//Start = rotateSum [1,2,3] [] //[]


/* 12. Bags
Given a list of 3-element tuples where 
   the 1st element represents the key of the tuple, 
   the 2nd element represents the value of the tuple,
   the 3rd element represents the multiplicity (frequency) of the value.
Given a second list, it has to be "added" to the first list like:
   A = [("Test1",576,2),("Test2",304,2),("Test3",789,3)] and 
   B = [576,304,304,834,576,789,834,576,304,789]
Processing the list B, we add to A 576 3 times, 304 3 times,
789 is added 2 times, 834 appears 2 times in B, but does not exist in A, 
so 834 will be ignored.
Result: [(("Test1",576,5),("Test2",304,5),("Test3",789,5)] */

bag :: [(String, a, Int)] [a] -> [(String, a, Int)] | Eq a
bag listA listB = [(name,val,cnt + length (filter ((==)val) listB ) ) \\ (name,val,cnt) <- listA]

//Start = bag [("Test1",576, 2),("Test2",304, 2),("Test3", 789, 3)] [576,304,304,834,576,789,834,576,304,789]
// [("Test1",576,5),("Test2",304,5),("Test3",789,5)]
//Start = bag [("Test1",576, 2),("Test2",304, 2),("Test3", 789, 3)] [842,583,390,982]
// [("Test1",576,2),("Test2",304,2),("Test3",789,3)]
//Start = bag [("Test1",576, 2)] [576] // [("Test1",576, 3)]
//Start = bag [] [842,583,390,982] // []
//Start = bag [("Test1",576, 2),("Test2",304, 2)] [] // [("Test1",576,2),("Test2",304,2)