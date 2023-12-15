module mid
import StdEnv

/*---------------------------------------------------------------
-- Functional Programming midterm
-- This solution was submitted and prepared by
-- < NAME NEPTUN > for the midterm reretake programming assignment of
-- the Functional Programming course.
-- I declare that this solution is my own work.
-- I have not copied or used third-party solutions.
-- I have not passed my solution to my classmates, neither made it public.
*/


/* 1. Remove 0
Write a function that removes the zeros from a list. */

remove0 :: [Int] -> [Int]
remove0 list = filter ((<>)0) list

//Start = remove0 [0,0,0,0,1,2,3,0,4,0,0,0,5,0,0,0,0,6,7,8,0] // 1,2,3,4,5,6,7,8]

/* 2. Change to 0
Write a  function that changes even numbers to 0 in a list.*/

f :: Int -> Int
f num
| isEven num = 0
= num

change0 :: [Int] -> [Int]
change0 list = map f list

//Start = change0 [1..10] // [1,0,3,0,5,0,7,0,9,0]
//Start = change0 [1,6,4,8,9,10,11,13,16] // [1,0,0,0,9,0,11,13,0]

/* 3. Sum of dubles
Write a function to change to double all elements and sum them up. */

sumdouble :: [Int] -> Int
sumdouble list = 2 * (sum list)

//Start = sumdouble [1..10] // 110

//Use the below lists for the following tasks

Names = ["Louis","Peter","Brian","Stewie"]
Ages = [30,35,5,2]
Relation = ["Wife","Husband","Dog","Baby"]
salary = [1000,1500,1,1]
expense = [1200,200,100,1000]

/* 4. Data processing
Summarize the above data such that the function takes these 5 lists 
and creates a list of tuples, one tuple should contain all info about one person.
input - above 5 lists
output - [("Louis",30,"Wife",1000,1200),("Peter",35,"Husband",1500,200),("Brian",5,"Dog",0,100),("Stewie",2,"Baby",0,1000)] */

summary :: [String] [Int] [String] [Int] [Int] -> [(String,Int,String,Int,Int)]
summary names ages rela sal exp = [(n,a,r,s,e) \\ n <- names & a <- ages & r <- rela & s <- sal & e <- exp]

summaryTuple = summary Names Ages Relation salary expense

//Start = summaryTuple
// [("Louis",30,"Wife",1000,1200),("Peter",35,"Husband",1500,200),("Brian",5,"Dog",1,100),("Stewie",2,"Baby",1,1000)]

/* 5. High expense
Take the previous list of tuples and write a function
to find the person with highest highest expenses.
output - "Louis" */

highExp :: [(String,Int,String,Int,Int)] -> String
highExp list = snd (maxList (map (\(n,a,r,s,e) = (e,n)) list))

//Start = highExp summaryTuple // "Louis"

/* 6. High ratio
Write a function that takes the previous list of tuples and returns the 
relation of the person whose expense to salary ratio is the maximum. 
output = "Baby" */

maxRatio :: [(String,Int,String,Int,Int)] -> String
maxRatio list = snd (maxList (map (\(n,a,r,s,e) = ((toReal e) / (toReal s),r)) list))
//Start = maxRatio summaryTuple // "Baby"


/* 7. Oldest
Create a function that takes the list of tuple and returns all details 
of who is the oldest in the family, 1 dog year is 8 human years. */

update :: (String,Int,String,Int,Int) -> (String,Int,String,Int,Int)
update (n,a,r,s,e)
| r == "Dog" = (n,a*8,r,s,e)
= (n,a,r,s,e)

rule :: (String,Int,String,Int,Int) (String,Int,String,Int,Int) -> Bool
rule (n1,a1,r1,s1,e1) (n2,a2,r2,s2,e2) = (a1 > a2)

Oldest :: [(String,Int,String,Int,Int)] -> (String,Int,String,Int,Int)
Oldest list = hd (sortBy rule (map update list))

//Start = Oldest summaryTuple // ("Brian",5,"Dog",1,100)

/* 8. Divisible halfs
Write a function that takes a number and checks if its second half is divisible by the first half
E.g. - 224448 first half -> 224 , second half-> 448, 448 is divisible by 224 so output is True
For simplicity assume the number to be of even length.*/

numToList :: Int -> [Int]
numToList num = reverse ( map (\x=x rem 10) ( takeWhile ((<>)0) (iterate (\x=x/10) num) ) )

listToNum :: [Int] -> Int
listToNum [] = 0
listToNum [x:xs] = x*(10^(length xs)) + listToNum xs

secHalfDiv :: Int -> Bool
secHalfDiv num = (listToNum (drop n list)) rem (listToNum (take n list)) == 0 
where list = numToList num
	  n = (length list) / 2
	  
//Start = secHalfDiv 224448 // True
//Start = secHalfDiv 224447 // False

/* 9. Occurences modified
Write a function that takes a list of integers modifies it in the following way
if there is more than one occurence of a number it is replaced by the total sum.
E.g. - input [1,4,2,3,2,3,3,3]
occurence of 1 -> 1 so it stays 1
occurence of 2 -> 2 so it becomes 2+2=4
occurence of 3 -> 4 so it becomes 3+3+3+3 = 12
occurence of 4 -> 1 so it stays 4
output - [1,4,4,12,4,12,12,12] */

	

//Start = occModify [1,4,2,3,2,3,3,3] // [1,4,4,12,4,12,12,12]
//Start = occModify [1,2,1,1,1,1,2,2,2,2,3,1,2,3,5] // [6,12,6,6,6,6,12,12,12,12,6,6,12,6,5]


/* 10. L matrix
Write a function that takes a square matrix (list of lists) of Integer 
and returns if it is an L Matrix: all elements in first column and 
last row are 1 and rest are 0. Example:
 1 0 0 0
 1 0 0 0  is L matrix and returns true
 1 0 0 0
 1 1 1 1
 
 1 0 0
 0 0 0   is not L matrix because not all elements of first column are 1
 1 1 1   */
 
check :: [Int] Int -> Bool
check row i
| (length row) - 1 == i = row == repeatn (length row) 1
= ([1] ++ repeatn (length row - 1) 0) == row

LorNot :: [[Int]] -> Bool
LorNot matrix = and [check row i \\ row <- matrix & i <- [0..]]
where n = length matrix

//Start = LorNot [[1,0,0],[1,0,0],[1,1,1]] // True
//Start = LorNot [[1,0,0],[0,0,0],[1,1,1]] // False
//Start = LorNot [[1,0,0,0],[1,0,0,0],[1,0,0,0],[1,1,1,0]] // False






