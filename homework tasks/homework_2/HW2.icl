module HW2
import StdEnv


//Name:    BUI NGUYEN KIM HAI            Neptun Code: QMIBHU

/*
Question 1
In maths, a happy number is a number which will eventually reach 1 when replaced by the sum of square
of each digit. On the contrary, unhappy numbers will eventually reach 4 when they undergo the same process.
For exmaple, 
	13 is a happy number, because 13 -> 1^2 + 3^2 = 10 -> 1^2 + 0^2 = 1
	
	11 is not a happy number, because 11 -> 1^2 + 1^2 = 2 -> 2^2 = 4 
	
Tip: You need a helper function which will compute the sum of the square of each digit.
*/

sumDigit :: Int -> Int
sumDigit 0 = 0
sumDigit x = y^2 + sumDigit(x / 10)
where y = x rem 10

isHappy :: Int -> Bool
isHappy num 
| num <= 0 = False
| num == 1 = True
| num == 4 = False
= isHappy (sumDigit num)

//Start = isHappy 0 //False
//Start = isHappy 1 //True
//Start = isHappy 4 //False
//Start = isHappy 12 //False
//Start = isHappy 13 //True
//Start = isHappy 68 //True

//Start = (isHappy 0, isHappy 1, isHappy 4, isHappy 12, isHappy 13, isHappy 68, isHappy 10, isHappy 7)

/*
Question 2
Given a number n, go through all numbers between 1 and n, and append every HappyNumber that you come accross
to a string. Output the string. 
*/

HappyNumbers :: Int -> String
HappyNumbers x
| x < 0 = ""
| (isHappy x) = toString x +++ " " +++ HappyNumbers(x - 1)
= HappyNumbers(x - 1)

//Start = HappyNumbers 100 // "100 97 94 91 86 82 79 70 68 49 44 32 31 28 23 19 13 10 7 1 "
//Start = HappyNumbers 0 // ""
Start = HappyNumbers 3
