module HW10
import StdEnv


//Name:[BUI NGUYEN KIM HAI] NeptunCode:[QMIBHU]

:: Tree a = Node a (Tree a) (Tree a) | Leaf
tree1 = Node 2 
						( Node 3 (Node 8 Leaf Leaf) (Node 13 Leaf Leaf)) 
						( Node 5 (Node 21 Leaf Leaf) (Node 34 Leaf Leaf))
						
tree2 = Node 10 
						( Node 5 (Node 3 (Node 1 Leaf Leaf) Leaf) (Node 7 (Node 6 Leaf Leaf) Leaf)) 
						( Node 15 (Node 13 Leaf Leaf) (Node 18 Leaf Leaf))
						
tree3 = Node 10 
						( Node 5 (Node 3 Leaf Leaf) (Node 7 Leaf Leaf)) 
						( Node 15 Leaf (Node 18 Leaf Leaf))
						
tree4 = Node 7 (Node 13 Leaf Leaf) (Node 11 Leaf Leaf)


/*  1)Nodes within range

    Given a binary search tree and two integers low and high, 
    return a sorted list with the values of all nodes with a value in the inclusive range 
    [low, high].

    eg:
    
    Input: low = 7, high = 15
             10                   1st level
            /  \                
           5   15                 2nd level
          / \    \ 
         3   7   18               3rd level

       	

	Output: [7, 10, 15]
	Explanation: Nodes 7, 10, and 15 are in the range [7,15]
*/

nodesWithinRange :: (Tree Int) Int Int -> [Int]
nodesWithinRange Leaf _ _ = []
nodesWithinRange (Node x le ri) start end 
| start <= x && x <= end = sort ([x] ++ (nodesWithinRange le start end) ++ (nodesWithinRange ri start end))
= sort ((nodesWithinRange le start end) ++ (nodesWithinRange ri start end))

//Start = nodesWithinRange tree1 5 10 // [5,8]
//Start = nodesWithinRange tree3 7 15 // [7,10,15]
//Start = nodesWithinRange tree2 6 10 // [6,7,10]


/* Instance - Define (+) on Strings 
When adding a word from another, add all instances of 
the letters which are not present in the adding word to the end original word.


For example,
	"player" "field"-> "playerfid" 
	Explanation: we add all letters of field which are not present in player. In this case {'f','i','d'} are not
	present in player, so we append to the end of player fid.
*/

str2List :: String -> [Char]
str2List str = [c\\c<-:str]

instance + String
where
	(+) str1 str2 = {c\\c<-newList}
	where
		list1 = str2List str1
		list2 = str2List str2
		newList = list1 ++ [c\\c<-list2|not(isMember c list1)]


//Start = "word" + "word" // "word"
//Start = "runner" + "railway" // "runnerailway"
//Start = "player" + "field" // "playerfid"