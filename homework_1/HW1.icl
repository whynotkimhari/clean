module HW1

import StdEnv


//Please write your neptun code here: QMIBHU

/*
Don't copy the others' work, otherwise, you won't get point for this homework.
Changing the function and variable does not help. 
	
Your submission should not have any errors when running the code.

It is possible to get partial points for not working code, but please make sure you comment out the errors.

You should not delete anything from the given code, given test cases should stay the same, but you can add 
your tests as well. Don't change the given function signatures, however, you can add as many functions as 
you wish, just make sure to name them appropriately (if function squares the number, call it 'square',
'second_power', etc. and not 'f' or 'g'). The same goes for variable names. 

Make sure that you comment all 'Start'-s before submitting the code.
*/


/*
Task 1

Intro:
The binary number system is the base of all computing systems and operations. Hence, as Computer Scientists we 
should be informed about this number system!
Read about it here: https://www.cuemath.com/numbers/binary-to-decimal/
Task:
Define a function which will convert a 3 digit binary number to a decimal number.
The binary number will not be provided as a single integer, but rather 3 separate integers, each representing
one digit of the binary number.
As you should have already learned, binary digits can either be 0 or 1, so be sure to check that
*/

binToDec :: Int Int Int -> Int
binToDec num1 num2 num3
| (num1 == 0 || num1 == 1) && (num2 == 0 || num2 == 1) && (num3 == 0 || num3 == 1) = 4 * num1 + 2 * num2 + 1 * num3
= abort "One of the inputs is NOT a binary digit!"

//Start = binToDec 1 2 1 // "One of the inputs is NOT a binary digit!"
//Start = binToDec 9 1 1 // "One of the inputs is Not a binary digit!"
//Start = binToDec 1 2 7 // "One of the inputs is Not a binary digit"
//Start = binToDec 1 1 1 // 7
//Start = binToDec 0 1 0 // 2
//Start = binToDec 1 1 0 // 6
//Start = binToDec 1 0 0 // 4

// My own test case
//Start = binToDec 0 0 0 // 0
//Start = binToDec 0 0 1 // 1
//Start = binToDec 0 1 1 // 3
//Start = binToDec 1 0 1 // 5
//Start = binToDec 6 6 6 // -1 ("One of the inputs is NOT a binary digit!")
//Start = binToDec 0 1 3 // -1 ("One of the inputs is NOT a binary digit!")
//Start = binToDec 1 6 9 // -1 ("One of the inputs is NOT a binary digit!")
//Start = binToDec 0 0 2 // -1 ("One of the inputs is NOT a binary digit!")


/*
Task 2
Create a function that determines whether or not a given year is a leap year. 
Leap years are determined by the following rules:

Leap years are years divisible by four (like 1984 and 2004). 
However, years divisible by 100 are not leap years (such as 1800 and 1900) 
unless they are divisible by 400 (for example 800, 1600).
*/

leapYear :: Int -> Bool
leapYear year 
| (year rem 4 == 0 && year rem 100 <> 0) || year rem 400 == 0 = True
= False
//Start = leapYear 1996 // true
//Start = leapYear 1997 // false
//Start = leapYear 1900 //false
//Start = leapYear 1600 //true

// My own test case
//Start = leapYear 1800 // false
//Start = leapYear 2004 // true
//Start = leapYear 2023 //false: this year is not a leap year
//Start = leapYear 2024 //true: however; next year will be a leap year!!
