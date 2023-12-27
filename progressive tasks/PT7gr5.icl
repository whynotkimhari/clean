module PT7gr5

import StdEnv

// BUI NGUYEN KIM HAI - QMIBHU

/*
* Write a function getAverage that takes array of students and returns array
* of tuples, where first element is the same student and second one is average
* of it's grades. Average should NOT be rounded to integers, it should be real.
*/

:: Student = {name :: String
			 ,id :: String
			 ,grades :: {Int}}

myAvg :: [Real] -> Real
myAvg [] = 0.0
myAvg list = avg list

getAverage :: {Student} -> {(Student, Real)}
getAverage arr = {(s, myAvg [toReal g \\ g <-: s.grades]) \\ s <-: arr}

student1 = {name="a",id="st1",grades={80,40,70}}
student2 = {name="b",id="st2",grades={120,30,80,40,70}}
student3 = {name="c",id="st3",grades={80,50,40,70}}
student4 = {name="d",id="st4",grades={}}




//Start = getAverage {student1} // {((Student "a" "st1" {80,40,70}),63.3333333333333)}
//Start = getAverage {student1, student2, student3, student4} // {((Student "a" "st1" {80,40,70}),63.3333333333333),((Student "b" "st2" {120,30,80,40,70}),68),((Student "c" "st3" {80,50,40,70}),60),((Student "d" "st4" {}),0)}
//Start = getAverage {} // {}
//Start = getAverage {student4} // {((Student "d" "st4" {}),0)}