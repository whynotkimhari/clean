module PT4gr5

import StdEnv

// write your name and neptun code here: BUI NGUYEN KIM HAI - QMIBHU

// Write a function which takes a list of lists
// and returns the biggest even sum of the sublists' sums
// example: [[1,2,3],[],[10,1],[2,2]] -> 6 because the sum
// of [1,2,3] is 6 and it is biggest even between the other sums
// 0, 11 and 4
// Note: sum of empty list is 0
// Note: if there is no even sums, return -1

max_sum :: [[Int]] -> Int
max_sum lists 
| new_list == [] = -1
= last new_list
where new_list = sort (filter isEven (map sum lists))
//Start = max_sum [[1,2,3],[],[10,1],[2,2]] //6
//Start = max_sum [[1,1], [9,9], [100,1]] //18
//Start = max_sum [[],[1]] //0
//Start = max_sum [[1],[3,4]] //-1
