module HW8
import StdEnv

/*
	NAME : BUI NGUYEN KIM HAI
	NEPTUN ID : QMIBHU
*/

/*
The following lines are necessary for this homework. Do not modify them.
Skip down to the questions.
*/

:: Position = Forward | Midfield | Defense
:: Team = { teamname :: String, players :: {Player} }
:: Player = { name :: String,
			age :: Int,
			position :: Position,
			nationality :: String,
			goals :: {Int}
		   }
		   
p1 :: Player
p1 = {name = "p1", age = 21, position = Forward, nationality = "German", goals = {0,0,3,2,1,1,0,2} }
p2 :: Player 
p2 = {name = "p2", age = 29, position = Defense, nationality = "Dutch", goals = {0,1,0,0,1,1,0,2} }
p3 :: Player 
p3 = {name = "p3", age = 25, position = Midfield, nationality = "British", goals = {5,1,5,0,0,1,0,2} }
p4 :: Player
p4 = {name = "p4", age = 27, position = Forward, nationality = "Italian", goals = {3,1,2,3,1,1,0,2} }
p5 :: Player
p5 = {name = "p5", age = 16, position = Defense, nationality = "Italian", goals = {0,2,0,1,0,0,1,0}	}	   
p6 :: Player
p6 = {name = "p6", age = 22, position = Midfield, nationality = "Dutch", goals = {6,0,0,0,0,2,3,0} }  
		   
t1 :: Team
t1 = {teamname = "t1", players={p2,p3,p4} } // 27 avg age
t2 :: Team 
t2 = {teamname = "t2", players={p1,p3,p4} } // 24 avg age
t3 :: Team 
t3 = {teamname = "t3", players={p1,p2} } // 25 
t4 :: Team
t4 = {teamname = "t4", players={p1,p4,p6} } // 23
t5 :: Team 
t5 = {teamname = "t5", players={p1,p5} } // 18
t6 :: Team
t6 = {teamname = "t6", players={p2,p5,p6} } // 22


/* Task 1 - Calculate the team with the lowest average age 
Given a list of team, calculate the team with the lowest average age. The average age
of a team can be defined as the average of its players' ages.
For example, 
		[t3,t5,t6]
		average age of t3 = 21 + 29 / 2 = 25
		average age of t5 = 21 + 25 / 2 = 23
		average age of t6 = 29 + 16 + 22 / 3 = 22
		Hence, the function should output t6
*/ 

get_avg :: Team -> (Int,String)
get_avg team = (avg ages, team.teamname)
where ages = [player.age \\ player <-: team.players]

calc_lowest_avg_team :: [Team] -> String
calc_lowest_avg_team teams = snd ( minList [get_avg team \\ team <- teams] )

//Start = calc_lowest_avg_team [t1,t2,t3] // "t2"
//Start = calc_lowest_avg_team [t1,t2,t3,t4,t5,t6] // "t5"
//Start = calc_lowest_avg_team [t1,t3,t6] // "t6"
//Start = calc_lowest_avg_team [t2,t4] // "t4"

/* 2 - Calculate the team with the most goals for a specific position.
Given a list of teams, you should calculate the total goals scored by the players
in the given position for each team. Afterwards, return those teams which have the
maximum amount of goals for the given position.
For example,
	Given [t5, t6] Forward
	First you check players of t5: p1 and p5. 
		p1 is Forward, hence you calculate goals scored by p1. 
		p2 is not Forward, hence you do not calculate goals scored by p5.
	Total number of goals scored by Forwards in this case, is equal to the goals scored by p1.
	Second you check players of t6: p2, p5, and p6
		p2 is not Forward
		p5 is not Forward
		p6 is not Forward
	Hence, Total number of goals scored by Forwards in this case is equal to 0.
	Function should return ["t5"]
*/

id :: Position -> Int
id Forward = 0
id Midfield = 1
id Defense = 2

get_sum :: Team Position -> (Int,String)
get_sum team pos = (total, team.teamname)
where 
	total = sum [goal \\ player <-: team.players, goal <-: player.goals | id player.position == id pos]
	
max_goals :: [Team] Position -> [String]
max_goals teams pos = qualifiedList
where 
	qualifiedList = [name \\(goal,name) <- list | goal == target]
	list = [get_sum team pos \\ team <- teams]
	target = fst (maxList list)

//Start = max_goals [t5,t6] Forward // ["t5"]
//Start = max_goals [t1,t3] Midfield // ["t1"]
//Start = max_goals [t1,t2,t3,t4,t5,t6] Forward // ["t2","t4"]
//Start = max_goals [t1,t2,t3,t4,t5,t6] Defense // ["t6"]
//Start = max_goals [t1,t2,t3,t4,t5,t6] Midfield // ["t1","t2"]

