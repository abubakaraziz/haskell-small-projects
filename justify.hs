-- ========================================================================================================================== --

--READ ME !!
--MUHAMMAD ABU BAKAR AZIZ 20100117
--I have tested the code on the all the given examples and it is working fine. The code fails for very large width e.g 30 etc
--my code works properly for width >10 and less than 30 for the last part.
--Moreover, I also used nubs code from online to filter out duplicates.
--For JustifyText function, use putStr(head(justifyText lineCost enHyp 15 text1)).
--                                                          ASSIGNMENT 1
--
--      A common type of text alignment in print media is "justification", where the spaces between words, are stretched or
--      compressed to align both the left and right ends of each line of text. In this problem we'll be implementing a text
--      justification function for a monospaced terminal output (i.e. fixed width font where every letter has the same width).
--
--      Alignment is achieved by inserting blanks and hyphenating the words. For example, given a text:
--
--              "He who controls the past controls the future. He who controls the present controls the past."
--
--      we want to be able to align it like this (to a width of say, 15 columns):
--
--              He who controls
--              the  past cont-
--              rols  the futu-
--              re. He  who co-
--              ntrols the pre-
--              sent   controls
--              the past.
--


-- ========================================================================================================================== --


import Data.List
import Data.Char

text1 = "He who controls the past controls the future. He who controls the present controls the past."
text2 = "A creative man is motivated by the desire to achieve, not by the desire to beat others."


-- ========================================================================================================================== --







-- ========================================================= PART 1 ========================================================= --


--
-- Define a function that splits a list of words into two lists, such that the first list does not exceed a given line width.
-- The function should take an integer and a list of words as input, and return a pair of lists.
-- Make sure that spaces between words are counted in the line width.
--
-- Example:
--    	   ==>   (["A", "creative"], ["man"])
--    splitLine ["A", "creative", "man"] 11   ==>   (["A", "creative"], ["man"])
--    splitLine ["A", "creative", "man"] 10   ==>   (["A", "creative"], ["man"])
--    splitLine ["A", "creative", "man"] 9    ==>   (["A"], ["creative", "man"])
--


splitLine :: [String] -> Int -> ([String], [String])
-- Function definition here
splitLine [] _ =([],[])
splitLine listofstrings width
    |tail_list==[] &&  length_head<=width =(listofstrings,[])
	|length_head<=width= ([head listofstrings] ++ fst (splitLine tail_list (width-length_head-1)),snd (splitLine tail_list (width-length_head-1)))
	|otherwise= ([],listofstrings)
	where 
		length_head=length(head listofstrings)
		tail_list=tail listofstrings







-- ========================================================= PART 2 ========================================================= --


--
-- To be able to align the lines nicely. we have to be able to hyphenate long words. Although there are rules for hyphenation
-- for each language, we will take a simpler approach here and assume that there is a list of words and their proper hyphenation.
-- For example:

enHyp = [("creative", ["cr","ea","ti","ve"]), ("controls", ["co","nt","ro","ls"]), ("achieve", ["ach","ie","ve"]), ("future", ["fu","tu","re"]), ("present", ["pre","se","nt"]), ("motivated", ["mot","iv","at","ed"]), ("desire", ["de","si","re"]), ("others", ["ot","he","rs"])]


--
-- Define a function that splits a list of words into two lists in different ways. The first list should not exceed a given
-- line width, and may include a hyphenated part of a word at the end. You can use the splitLine function and then attempt
-- to breakup the next word with a given list of hyphenation rules. Include a breakup option in the output only if the line
-- width constraint is satisfied.
-- The function should take a hyphenation map, an integer line width and a list of words as input. Return pairs of lists as
-- in part 1.
--
-- Example:
--  lineBreaks enHyp 12 ["He", "who", "controls."]   ==>  	
--
-- Make sure that words from the list are hyphenated even when they have a trailing punctuation (e.g. "controls.")
--
-- You might find 'map', 'find', 'isAlpha' and 'filter' useful.
--


lookupfind::String->[(String, [String])]->[String]
lookupfind  key []=[]
lookupfind  [] (x:xs)=[]
lookupfind  key (x:xs) --This functions finds value in a tuple of specific key
			|key==value=snd x
			|xs==[]=[]				
			|otherwise=lookupfind key xs 
			where 
				value=fst x
--function defination 	
space_counter::[String]->Int
space_counter []=0
space_counter (x:[])=0
space_counter (x:xs)=1+ space_counter xs

-- Function definition here

length_find :: ([String], [String])->Int
length_find ([""],[""])=0
length_find liststring=length (concat(fst liststring)) 

--test code: second_value_tupe ([],["A","creative","man"])
second_value_tupe::([String], [String])->[String]  -- getting second value of tuples only as list of strings
second_value_tupe x=snd x

--adding_two_strings::(String->String)

--test code: lineBreaks enHyp 12 ["He", "who", "controls."]
--test code: lineBreaks enHyp 12 ["A", "creative", "achieve"] 12
remove_punc::String->String --removing '.' from the string
remove_punc ""=""
remove_punc (x:xs)
				|x=='.'= ""++ remove_punc xs 
				|otherwise=[x] ++ remove_punc xs 

--removing_n_characters 2 "controls." 
remov_n_char::Int->String->String
remov_n_char n []=[]
remov_n_char n (x:xs) 	|xs==[]=[]
						|n==0=x:xs
						|otherwise= remov_n_char (n-1) xs


adding_hypo::String->String->String
adding_hypo previous_word hyp=previous_word++hyp
						
 --test-case-0 8 "creative" "" ["He", "who"] ["cr","ea","ti","ve"]
add_string::Int->Int->String->String->[String]->[String]->[[String]]
add_string counter width_left key_word previous_word listofstrings  listofhyps
						|listofhyps==[]=[]  
						|otherwise= [current_list]++ recursive_call					
							where
								string_length=length (head listofhyps)
								current_hyp=head listofhyps
								previous_hypo=adding_hypo previous_word	current_hyp
								current_list=listofstrings++[previous_hypo ++ "-"]
								recursive_call=add_string (counter+string_length) width_left key_word previous_hypo listofstrings  (tail listofhyps)
		




get_tail_of_list::[String]->String
get_tail_of_list []=[]
get_tail_of_list (x:xs)
				|xs==[]=x
				|otherwise=get_tail_of_list xs

--[["He","who","co-"],["He","who","cont-"],["He","who","contro-"],["He","who","controls-"]]
add_val_to_list::[String]->String->([String],[String])
add_val_to_list listofwords key_word=(listofwords,[remov_n_char ((length tailstring)-1) key_word])
							where
								tailstring=get_tail_of_list listofwords

-- get_second_tup_val::[["hello", "how"]] "creative" 20
-- get_second_tup_val::[["He","who","co-"],["He","who","cont-"],["He","who","contro-"],["He","who","controls-"]] "controls" 20
get_second_tup_val::[[String]]->String->Int->[([String],[String])]
get_second_tup_val [] key_word width_left=[]
get_second_tup_val (x:xs) key_word width_left
								|key_word==[]=[]
								|length(get_tail_of_list x )<(width_left) =[add_val_to_list x key_word] ++ get_second_tup_val xs key_word width_left
								|otherwise=[]							

--test code: lineBreaks enHyp 12 ["He", "who", "controls."]

lineBreaks :: [(String, [String])] -> Int -> [String] -> [([String], [String])]

--lineBreaks enHyp 12 ["He", "who", "controls."]   ==>   [(["He","who"], ["controls."]),, (["He","who","co-"], ["ntrols."]), (["He","who","cont-"], ["rols."])]

lineBreaks enHyp width liststring					
					|snd x==[]=[x]
					|otherwise= [x]++final_list --complete_list
								where 
									x=splitLine liststring width
 									leng=length_find(splitLine liststring width) --calculate length of strings in first value tuple e.g 5 (Hewho)(["He","who"],["controls"])
 									listofwords=second_value_tupe x --give second value tuple input second_value_tupe (splitLine ["He", "who","controls", "nice"] 12) gives ["controls","nice"]
 									value=lookupfind (remove_punc(head listofwords)) enHyp --find value of the the head in listofwords e.g (["controls"]) and table corresponding to "controls" like co-
 									width_left=width-leng-space_counter (fst x)
 									list_word_not_hyp=add_string 0 width_left (remove_punc(head listofwords)) "" (fst x) value
 									complete_list=get_second_tup_val list_word_not_hyp (head listofwords) width_left
 									final_list=map (\(first,second)->(first,second++tail listofwords)) complete_list


-- ========================================================= PART 3 ========================================================= --


--
-- Define a function that inserts a given number of blanks (spaces) into a list of strings and outputs a list of all possible
-- insertions. Only insert blanks between strings and not at the beginning or end of the list (if there are less than two
-- strings in the list then return nothing). Remove duplicate lists from the output.
-- The function should take the number of blanks and the the list of strings as input and return a lists of strings.
--
-- Example:
--    blankInsertions 2 ["A", "creative", "man"]   ==>   [["A", " ", " ", "creative", "man"], ["A", " ", "creative", " ", "man"], ["A", "creative", " ", " ", "man"]]
--
-- Use let/in/where to make the code readable
--

add_single_blank::[String]->[String]
add_single_blank []=[]
add_single_blank (x:[])=[x]
add_single_blank (x:xs)=[x]++[" "]++xs
						

												

insert_blank:: [String]->[[String]]
insert_blank []=[]
insert_blank (x:[])=[]
insert_blank (x:xs)=[[x]++[" "]++xs] ++  map (x :) (insert_blank xs)  
									
apply_insert_rest::[[String]]->[[String]]
apply_insert_rest []=[]
apply_insert_rest (x:xs)=insert_blank x ++ apply_insert_rest xs

nubs ::(Eq a)=>[[a]]->[[a]]
nubs []=[]
nubs (x:xs)=x:nub (filter(\y->not (x==y))xs)

blankInsertions :: Int -> [String] -> [[String]]
blankInsertions _ []=[]
blankInsertions n list_of_words
				|n==0=[list_of_words]
				|n==1=insert_blank list_of_words
				|otherwise= nubs(apply_insert_rest((blankInsertions (n-1) list_of_words)))





-- ========================================================= PART 4 ========================================================= --


--
-- Define a function to score a list of strings based on four factors:
--
--    blankCost: The cost of introducing each blank in the list
--    blankProxCost: The cost of having blanks close to each other
--    blankUnevenCost: The cost of having blanks spread unevenly
--    hypCost: The cost of hyphenating the last word in the list
--
-- The cost of a list of strings is computed simply as the weighted sum of the individual costs. The blankProxCost weight equals
-- the length of the list minus the average distance between blanks (0 if there are no blanks). The blankUnevenCost weight is
-- the variance of the distances between blanks.
--
-- The function should take a list of strings and return the line cost as a double
--
-- Example:
--    lineCost ["He", " ", " ", "who", "controls"]
--        ==>   blankCost * 2.0 + blankProxCost * (5 - average(1, 0, 2)) + blankUnevenCost * variance(1, 0, 2) + hypCost * 0.0
--        ==>   blankCost * 2.0 + blankProxCost * 4.0 + blankUnevenCost * 0.666...
--
-- Use let/in/where to make the code readable
--


---- Do not modify these in the submission ----
blankCost = 1.0
blankProxCost = 1.0
blankUnevenCost = 1.0
hypCost = 1.0
-----------------------------------------------
lengthlist_strings::[String]->Double
lengthlist_strings []=0
lengthlist_strings (x:xs)=1+lengthlist_strings xs

lengthlist_num::[double]->Double
lengthlist_num []=0
lengthlist_num (x:xs)=1+lengthlist_num xs


sums::[Double]->Double
sums [] =0
sums (x:[]) =x
sums x= (foldr (+) 0 x)

hyp_counter_word:: String->Double
hyp_counter_word ""=0
hyp_counter_word (x:xs)
			|[x]=="-"=1+hyp_counter_word xs
			|otherwise=hyp_counter_word xs


hyp_counter_list::[String]->Double
hyp_counter_list []=0
hyp_counter_list (x:xs)=hyp_counter_word x+ hyp_counter_list xs

remove_n_list:: Double-> [String]->[String]	
remove_n_list _ []=[]
remove_n_list  0 x=x
remove_n_list  n (x:xs)=remove_n_list (n-1) xs


dist_betwe_blank ::[String]->Double
dist_betwe_blank []=0
dist_betwe_blank (x:xs)
				|x/=" "=1+dist_betwe_blank xs
				|otherwise=0
				

check_blank ::[String]->Double
check_blank []=(1-(2))
check_blank (x:xs)
			|x==" "=0
			|otherwise=check_blank xs

list_of_Blanks::[String]->[Double]
list_of_Blanks []=[]
list_of_Blanks word_list=[dist_betwe_blank word_list] ++ list_of_Blanks (remove_n_list (n+1) word_list)
						where
							n=dist_betwe_blank word_list


blank_counter::[String]->Double
blank_counter []=0
blank_counter (x:xs)
			|x==" "=1+blank_counter xs
			|otherwise=blank_counter xs


list_distanc_blanks::[String]->[Double]
list_distanc_blanks []=[]
list_distanc_blanks x = list_of_Blanks x

mean_var ::[Double]->[Double]
mean_var x=map (((sums x)/(lengthlist_num x))-) x

square::[Double]->[Double]
square x=map (^2) x


variance::[Double]->Double
variance x =(sums (square (mean_var x)))/lengthlist_num x
cost=blankCost * 2.0 + blankProxCost * 4.0 + blankUnevenCost * 0.666
line=["He", " ", " ", "who", "controls"]
lineCost :: [String] -> Double
-- Function definition here
--        ==>   blankCost * 2.0 + blankProxCost * (5 - average(1, 0, 2)) + blankUnevenCost * variance(1, 0, 2) + hypCost * 0.0
--        ==>   blankCost * 2.0 + blankProxCost * 4.0 + blankUnevenCost * 0.666...
lineCost x =(blankCost * totalblankcost) + (blankProxCost * totalBlankProx) + (blankUnevenCost *tot_variance ) + (hypCost * totalhypcost)
			where
				total_blanks=blank_counter x
				list_length=lengthlist_strings x
				distbtwblanks=list_distanc_blanks x
				len=lengthlist_num distbtwblanks
				average_caculate= (sums distbtwblanks)/(len) 
				totalblankcost=(blankCost*total_blanks)
				totalBlankProx=(list_length-average_caculate)
				totalhypcost=(hypCost*(hyp_counter_list x))
				tot_variance=variance distbtwblanks
				





-- ========================================================= PART 5 ========================================================= --


--
-- Define a function that returns the best line break in a list of words given a cost function, a hyphenation map and the maximum
-- line width (the best line break is the one that minimizes the line cost of the broken list).
-- The function should take a cost function, a hyphenation map, the maximum line width and the list of strings to split and return
-- a pair of lists of strings as in part 1.
--
-- Example:
--    bestLineBreak lineCost enHyp 12 ["He", "who", "controls"]   ==>   (["He","who", "cont-"], ["rols"])
--
-- Use let/in/where to make the code readable
--


calculate_length:: [String]->Int
calculate_length []=0
calculate_length (x:xs)=length x + calculate_length xs


implied_blanks_counter::[String]->Int
implied_blanks_counter []=0
implied_blanks_counter (x:[])=0
implied_blanks_counter (x:xs)=1+implied_blanks_counter xs


apply_blanks::[([String],[String])]->Int->[[String]]
apply_blanks [] _=[]
apply_blanks (x:xs) width 
			|(snd x)==[]=[fst x]
apply_blanks (x:xs) width=blankInsertions noblank (fst x) ++ apply_blanks xs width
							where
								noblank=width - (calculate_length (fst x))-(implied_blanks_counter (fst x))


apply_cost::[String]->(Double,[String])
apply_cost x=(lineCost x,x)

mini::[Double]->Double
mini []=0.0
mini (x:[])=x
mini(x:y:xs)
		|x<y=mini (x:xs)
		|otherwise=mini (y:xs)


remove_space::[String]->[String]
remove_space []=[]
remove_space(x:xs)
		|x==" "=remove_space xs
		|otherwise=[x]++remove_space xs 


return_bestline::[[String]]->Double->[String]
return_bestline [] _ =[]
return_bestline (x:xs) mini
			|lineCost x==mini=x
			|otherwise=return_bestline xs mini

find_finaline::[String]->[([String], [String])]-> [String]
find_finaline _ []=[]
find_finaline tofind (x:xs)
				|fst x==tofind=snd x
				|otherwise=find_finaline tofind xs
				


bestLineBreak :: ([String] -> Double) -> [(String, [String])] -> Int -> [String]->([String],[String])
bestLineBreak lineCost enHyp width lists= final_best_line
											where
												x=(lineBreaks enHyp width lists)
												right=map (lineCost) (apply_blanks (lineBreaks enHyp width lists) width) --returns [Double]
												length_of_each=map (length) (apply_blanks (lineBreaks enHyp width lists) width)
												blank_lists=(apply_blanks (lineBreaks enHyp width lists) width)
												minim=mini right
												bestline=return_bestline blank_lists minim
												tofind=remove_space bestline
												final_best_line=(bestline,find_finaline tofind x) 




-- Function definition here

-- Finally define a function that justifies a given text into a list of lines satisfying a given width constraint.
-- The function should take a cost function, hyphenation map, maximum line width, and a text string as input and return a list of
-- strings.
--
-- 'justifyText lineCost enHyp 15 text1' should give you the example at the start of the assignment.
--
-- You might find the words and unwords functions useful.
--
adding_backslash::[String]->[String]
adding_backslash []=[]
adding_backslash (x:[])=[x]++["\n"]
adding_backslash (x:xs)=[x]++adding_backslash xs

unword_string::[String]->[String]
unword_string []=[]
unword_string (x:[])=[x]
unword_string (x:xs)
			|x==" "=[x]++ unword_string xs
			|otherwise= [x]++[" "]++ unword_string xs


return_justify::[String] -> [(String, [String])]-> Int ->[String]
return_justify [] enHyph width =[]
return_justify listwords enHyp width=adding_backslash (unword_string(fst word_list)) ++ return_justify (snd word_list) enHyp width 
										where 
											word_list=(bestLineBreak lineCost enHyp width listwords)


--justifyText :: ([String] -> Double) -> [(String, [String])] -> Int -> String -> [String]
-- Function definition here
justifyText:: ([String] -> Double) -> [(String, [String])] -> Int -> String -> [String]
justifyText lineCost enHyp width text=  [(concat(return_justify word_list enHyp width))]
										where
											word_list=words text






