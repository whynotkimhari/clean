module endTerm1_todo
import StdEnv


//-------------------------------------------------------------------------------
/*
	1. Create a `toInt` instance for the Person record. An integer representation of a person
	is the sum of the length of its firstName, their age and height.
*/

::Person={firstName::String, age::Int, height::Int}
Rose::Person
Rose={firstName="Rose",age=23,height=172}
Jack::Person
Jack={firstName="Jack",age=25,height=193}
Emilia::Person
Emilia={firstName="Emilia",age=15,height=160}
Leo::Person
Leo={firstName="Leo",age=16,height=175}
Grace::Person
Grace={firstName="Grace",age=35,height=165}
Harry::Person
Harry={firstName="Harry",age=42,height=180}
Emilia2::Person
Emilia2={firstName="Emilia",age=15,height=180}

// TO DO instance

instance toInt Person
where 
	toInt p = length ([c \\ c <-: p.firstName]) + p.age + p.height

//Start = toInt Rose // 199
//Start = toInt Leo // 194
//Start = toInt Grace // 205


//-------------------------------------------------------------------------------
/*
	2. Create an instance of `isEven` for the Person record. A person is even if the sum of their
	age and height is even.
*/
// TO DO instance

instance isEven Person
where
	isEven p = (p.age + p.height) rem 2 == 0

//Start = isEven Rose // False
//Start = isEven Harry // True


//-------------------------------------------------------------------------------

// 3. Given a list of continents, give back the names of the continents that have 
// at least one country whose capital has prime number of 'i' in it.

::Country={name::String,capital::String}
Macedonia::Country
Macedonia={name="Macedonia",capital="Skopje"}
Hungary::Country
Hungary={name="Hungary",capital="Budapest"}
Spain::Country
Spain={name="Spain",capital="Madrid"}
Brazil::Country
Brazil={name="Brazil",capital="Brasilia"}
Chile::Country
Chile={name="Chile",capital="Santiago"}
Argentina::Country
Argentina={name="Argentina",capital="Buenos Aires"}
China::Country
China={name="China",capital="Beijing"}
India::Country
India={name="India",capital="New Delhi"}
::Continent={contName::String,countries::{Country}}
Europe::Continent
Europe = {contName="Europe",countries={Macedonia,Hungary,Spain}}
Asia::Continent
Asia = {contName="Asia",countries={China,India}}
SouthAmerica::Continent
SouthAmerica ={contName="South America",countries={Argentina,Brazil,Chile}}

isPrime :: Int -> Bool
isPrime n = [i \\ i <- [1..n] | n rem i == 0] == [1,n]

isOK :: Continent -> Bool
isOK continent = or [isPrime (length [1 \\ c <-: country.capital | c == 'i']) \\ country <-: continent.countries]

continentsPrimeI :: [Continent] -> [String]
continentsPrimeI continents = [continent.contName \\ continent <- continents | isOK continent]

//Start = continentsPrimeI [Europe,Asia]//["Asia"]
//Start = continentsPrimeI [Europe]//[]
//Start = continentsPrimeI [Europe,SouthAmerica,Asia]//["South America","Asia"]


//-------------------------------------------------------------------------------
/* 4.
Me and my friends went to play football in the streets, and the game ended as tie, 
so we were discussing if we should go for penalties or not. Help me to decide that.
You will get in a list each one of my team member skill Level and name, and you will get 
the name of  the other team's goalkeeper and his/her level of skill.
If the skill of the player is greater or equal than the skill of the goalkeeper, 
then the penalty will count as scored.
The team would win this virtual game, if at least 3 or more penalties could 
be scored against the given goalkeeper.
*/

::APlayer = { name ::String, skillLevel :: Int}

shouldWePlay :: [APlayer] APlayer -> Bool
shouldWePlay players goalie = length [1 \\ p <- players | p.skillLevel >= goalie.skillLevel] >= 3

//Start = shouldWePlay [{name = "kareem", skillLevel = 4},{name = "Tarek", skillLevel = 3},{name = "Ali", skillLevel = 3},{name="Hussien", skillLevel=2},{name="Ziad", skillLevel=4}] {name="Gemy", skillLevel=4} 
// False

//Start = shouldWePlay [{name = "kareem", skillLevel = 5},{name = "Tarek", skillLevel = 4},{name = "Ali", skillLevel = 3},{name="Hussien", skillLevel=2},{name="Ziad", skillLevel=4}] {name="Gemy", skillLevel=4} 
// True


//-------------------------------------------------------------------------------
// 5. Given an integer write it in full words. For example:
// 109 should be one-zero-nine.
// 175 should be one-seven-five
// 100 should be one-zero-zero

numberList :: {String}
numberList = {"zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"}

numToList :: Int -> [Int]
numToList 0 = [0]
numToList num = reverse (map (\x=x rem 10) (takeWhile ((<>)0) (iterate (\x=x/10) num)))

toStr :: [Int] -> String
toStr list = foldr (+++) "" [numberList.[x] +++ "-" \\ x <- (init list)] +++ numberList.[last list]

full_words :: Int -> String
full_words num
| num < 0 = "minus-" +++ toStr list
= toStr list
where list = numToList (abs num)

//Start = full_words 0 // "zero"
//Start = full_words 175 // "one-seven-five"
//Start = full_words -5 // "minus-five"
//Start = full_words 100 // "one-zero-zero"
//Start = full_words 33333 // "three-three-three-three-three"


//-------------------------------------------------------------------------------
/*
    6. Given a predefined MaybeInt type, define a new operator !+!
    for accessing the nth element in the list, you can test it with showFifthElement function.
*/

:: MaybeInt = Just Int | Nothing

// DEFINITION OF OPERATOR !+! ..... YOUR CODE COMES HERE....
(!+!) infixr 6 :: [Int] Int -> MaybeInt
(!+!) list x
| length list > x = Just arr.[x]
= Nothing
where
	arr :: {Int}
	arr = {y \\ y <- list}

//Just for testing purposed. DO NOT MODIFY

showFifthElement :: [Int] -> String
showFifthElement xs
  = case xs !+! 4 of
      Nothing -> "There is no fifth element in this list"
      Just n  -> "The fifth element of the list is: " +++ toString n

//Start = showFifthElement [1,2..10] // "The fifth element of the list is: 5"
//Start = showFifthElement [0,0] // "There is no fifth element in this list"
//Start = showFifthElement [33, 41, 56, 12, 96, 1] // "The fifth element of the list is: 96"


//-------------------------------------------------------------------------------

// 7. You are given array of integers.
// Your function should return true if each value appears at least twice in the array, 
// and it should return false otherwise.

f7 :: {Int} -> Bool
f7 arr = and [length (filter ((==)x) list) >= 2 \\ x <- list]
where list = [x \\ x <-: arr]

//Start = f7 {1,2,3,1,3,2,2,2} // True
//Start = f7 {1,2,3,4,3,2,1} // False
//Start = f7 {1,1,1,3,3,4,3,2,4,2} // True


//-------------------------------------------------------------------------------
// 8.
// Write a filter function for colored rose tree.
// Colored rose Tree is a tree where each node has 
// some value, color and children nodes stored in list.
// Your filter function should take tree, color, a two 
// condition function and filtering type as an argument. Return a list of
// values stored in nodes which have given color and
// satisfy both of the given conditions if filter type is 'AND'
// or satisfy at least one of the given functions if filter type
// is "OR" (Condition function returns
// true for node's value).

:: NodeColor = Red | Green | Blue

:: FilterType = AND | OR

:: ColoredRoseTree a = Node a NodeColor [ColoredRoseTree a] | Leaf

// TODO
instance toString FilterType
where 
	toString AND = "AND"
	toString OR = "OR"

instance toString NodeColor
where 
	toString Red = "Red"
    toString Green = "Green"
    toString Blue = "Blue"
	  
instance == FilterType
where 
	(==) a b = toString a == toString b
  	
instance == NodeColor
where 
	(==) a b = toString a == toString b

getNodesWithColor :: (ColoredRoseTree a) NodeColor -> [a]
getNodesWithColor Leaf _ = []
getNodesWithColor (Node x nodeColor list) color 
| nodeColor == color = [x] ++ flatten [getNodesWithColor (Node y n l) color \\ (Node y n l) <- list]
= flatten [getNodesWithColor (Node y n l) color \\ (Node y n l) <- list]

filterColoredTree :: (ColoredRoseTree a) NodeColor FilterType (a -> Bool) (a -> Bool) -> [a]
filterColoredTree Leaf _ _ _ _ = []
filterColoredTree (Node x nodeColor list) color fType cond1 cond2 
| fType == AND = [node \\ node <- nodes | (cond1 node) && (cond2 node)]
= [node \\ node <- nodes | (cond1 node) || (cond2 node)]
where nodes = getNodesWithColor (Node x nodeColor list) color

tree1 = Node 1 Red [(Node 2 Blue [Node 4 Blue []]), Leaf, Leaf, (Node 3 Blue [Leaf,Leaf])]
tree2 = Node 1 Red [(Node 2 Blue [Node 4 Blue []]), Leaf, Leaf, (Node 3 Blue [Leaf,Node 7 Red [Node 9 Red [], Node 10 Red []]])]

//Start = filterColoredTree tree1 Blue OR isEven isOdd // [2,4,3]
//Start = filterColoredTree tree1 Blue AND isEven isOdd // []
//Start = filterColoredTree tree1 Blue AND isOdd isOdd // [3]
//Start = filterColoredTree tree2 Red OR (\x = True) isEven // [1,7,9,10]
//Start::[Int] // Uncomment this line too, to run next test
//Start = filterColoredTree Leaf Green OR isOdd isEven // []

