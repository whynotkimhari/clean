module PT9gr5

import StdEnv 
// write NEPTUN AND NAME
// 	BUI NGUYEN KIM HAI -- QMIBHU

/* Define an + instance for Strings which returns String  
with vowels eliminated from both strings and glued into one. 
Note: y is also English vowel!
*/ 

//write the instance here
isVowel :: Char -> Bool
isVowel c = isMember c ['a','e','o','i','u','y','A','E','O','I','U','Y']
instance + String
where
	(+) a b = {c\\c<-z}
	where
		x = [c\\c<-:a|not (isVowel c)]
		y = [c\\c<-:b|not (isVowel c)]
		z = x ++ y

//instance 
 
Start = ["sarah" + "SARAH", "bOrys" + "Borys", "functional" + "CLEAN", "abcde" + "auco", "haPPy" + "pLaYz"]  
// ["srhSRH","brsBrs","fnctnlCLN","bcdc","hPPpLz"]