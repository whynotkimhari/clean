module PT1gr5
import StdEnv

/* Your name and neptun code :  BUI NGUYEN KIM HAI - QMIBHU      */

/*1- Write a function findHCategory that takes 
a Real representing the height of a person and 
returns a String representing the category of the 
person based on the given table:
-------------------------------------
|     BMI      |     Category      |
-------------------------------------
| 150.5 or less | small       |
-------------------------------------
| 150.6 - 180.9 | normal     |
-------------------------------------
| 190.0 - 200.9 | tall        |
-------------------------------------
| 201.0 or more | very tall           |
-------------------------------------
negativ values are invalid

*/

findHCategory :: Real -> String
findHCategory height 
| height < 0.0 = "invalid"
| height <= 150.5 = "small"
| height <= 180.9 = "normal"
| height <= 200.9 = "tall"
= "very tall"
//Start = findHCategory -5.0 // "invalid"
//Start = findHCategory 180.0 // "normal"
//Start = findHCategory 200.5 // "tall"
//Start = findHCategory 270.0 // "very tall"
//Start = findHCategory 130.5 // "small"

Start = (findHCategory -5.0, findHCategory 180.0, findHCategory 200.5, findHCategory 270.0, findHCategory 130.5)
