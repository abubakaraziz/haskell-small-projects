-- ---------------------------------------------------------------------
-- DNA Analysis 
-- CS300 Spring 2018
-- Due: 24 Feb 2018 @9pm
-- ------------------------------------Assignment 2------------------------------------
--20100117
--Muhammad Abu Bakar Aziz
--
-- >>> YOU ARE NOT ALLOWED TO IMPORT ANY LIBRARY
-- Functions available without import are okay
-- Making new helper functions is okay
--
-- ---------------------------------------------------------------------
--
-- DNA can be thought of as a sequence of nucleotides. Each nucleotide is 
-- adenine, cytosine, guanine, or thymine. These are abbreviated as A, C, 
-- G, and T.
--
type DNA = [Char]
type RNA = [Char]
type Codon = [Char]
type AminoAcid = Maybe String

-- ------------------------------------------------------------------------
-- 				PART 1
-- ------------------------------------------------------------------------				

-- We want to calculate how alike are two DNA strands. We will try to 
-- align the DNA strands. An aligned nucleotide gets 3 points, a misaligned
-- gets 2 points, and inserting a gap in one of the strands gets 1 point. 
-- Since we are not sure how the next two characters should be aligned, we
-- try three approaches and pick the one that gets us the maximum score.
-- 1) Align or misalign the next nucleotide from both strands
-- 2) Align the next nucleotide from first strand with a gap in the second     
-- 3) Align the next nucleotide from second strand with a gap in the first    
-- In all three cases, we calculate score of leftover strands from recursive 
-- call and add the appropriate penalty.                                    



score :: DNA -> DNA -> Int
score [] []=0
score (x:xs) []=0
score [] (y:ys)=0

score (x:xs)(y:ys)
	|x==y=maximum[1+a,1+b,3+c]  
	|otherwise=maximum [1+a,1+b,2+c]
	where
		a=score (x:xs) ys
		b=score xs (y:ys)
		c=score xs ys





-- -------------------------------------------------------------------------
--				PART 2
-- -------------------------------------------------------------------------
-- Write a function that takes a list of DNA strands and returns a DNA tree. 
-- For each DNA strand, make a separate node with zero score 
-- in the Int field. Then keep merging the trees. The merging policy is:
-- 	1) Merge two trees with highest score. Merging is done by making new
--	node with the smaller DNA (min), and the two trees as subtrees of this
--	tree
--	2) Goto step 1 :)
--

data DNATree = Node DNA Int DNATree DNATree | Nil deriving (Ord, Show, Eq)
type EACHNODE=([DNATree],Int)

merge_two::DNATree->DNATree->DNATree
merge_two left_tree Nil=left_tree
merge_two Nil right_tree=right_tree
merge_two (Node a score1 left1 right1) (Node b score2 left2 right2)
			|score1<score2=Node (min a b) (score a b) tree1 tree2
			|score1>=score2=Node (min a b) (score a b) tree2 tree1
				where
					tree1=(Node a score1 left1 right1) 
					tree2=(Node b score2 left2 right2)
					 
nubs ::(Eq a)=>[[a]]->[[a]]
nubs []=[]
nubs (x:xs)=x:nubs(filter(\y->not (x==y))xs)

subset_two::[DNATree]->[[DNATree]]
subset_two []=[]
subset_two (x:[])=[]
subset_two (x:y:z)=[[x]++[y]]++subset_two ([x]++z)++subset_two ([y]++z)


get_DNA::DNATree->DNA
get_DNA (Node a _ _ _)=a

trees_scores::[[DNATree]]->[EACHNODE]
trees_scores []=[]
trees_scores (x:xs)=let calculate (f:s:z)= score (get_DNA f) (get_DNA s)
										in [(x,calculate x)]++trees_scores xs  
										

highest_return ::[EACHNODE]->EACHNODE
highest_return []=([Nil],0)
highest_return (x:[])=x
highest_return (x:y:z)
				|snd x >snd y=highest_return ([x]++z)
				|otherwise=highest_return ([y]++z)  
 

treecombin_score::[DNATree]->[DNATree]
treecombin_score list =	mergedtree
						where 
						listreenodes=trees_scores(nubs(subset_two list))
						node=highest_return listreenodes
						old_tree1=let treelist (x:y:[])= x
					    	 	   in treelist (fst node)
					   	old_tree2=let treelist (x:y:[])= y
					    	 	   in treelist (fst node)
						mergedtree=let treelist (x:y:[])= [merge_two x y]
					    	 		in treelist (fst node) ++ [old_tree1] ++ [old_tree2]
											  						

treelist::[DNA]->[DNATree]
treelist []=[]
treelist (x:xs)=let singleton strand = Node strand 0 Nil Nil in [singleton x]++ treelist xs 

removetwo::[DNATree]->[DNATree]->[DNATree]
removetwo  [] []=[]
removetwo  x []=[]
removetwo  [] y=y
removetwo (x:xs) (y:ys)
			|x==y=removetwo (xs) (filter (/=x) ys)
			|otherwise=removetwo (xs) (filter (/=x) (y:ys))  

finalTree :: [DNATree] -> [DNATree]
finalTree   []=[]
finalTree  (x:[])=[x]
finalTree  (x:y:z)=finalTree  finalsingletree 
				 where 
				 mergedtree= head (treecombin_score (x:y:z))
				 twotreeremove=(tail(treecombin_score (x:y:z)))
				 finalsingletree=removetwo twotreeremove (x:y:z)++[mergedtree]
				
makeDNATree :: [DNA] ->DNATree
makeDNATree x = singletree
				where
				 listtrees=treelist x
				 singletree=head (finalTree listtrees)
-- -------------------------------------------------------------------------
--				PART 3
-- -------------------------------------------------------------------------

-- Even you would have realized it is hard to debug and figure out the tree
-- in the form in which it currently is displayed. Lets try to neatly print 
-- the DNATree. Each internal node should show the 
-- match score while leaves should show the DNA strand. In case the DNA strand 
-- is more than 10 characters, show only the first seven followed by "..." 
-- The tree should show like this for an evolution tree of
-- ["AACCTTGG","ACTGCATG", "ACTACACC", "ATATTATA"]
--
-- 20
-- +---ATATTATA
-- +---21
--     +---21
--     |   +---ACTGCATG
--     |   +---ACTACACC
--     +---AACCTTGG
--
-- Make helper functions as needed. It is a bit tricky to get it right. One
-- hint is to pass two extra string, one showing what to prepend to next 
-- level e.g. "+---" and another to prepend to level further deep e.g. "|   "

printbars::Int->[Char]
printbars 0=""
printbars 1="+---"
printbars n="|   "++printbars (n-1)

tree_print:: DNATree->Int->[Char]
tree_print Nil _=""
tree_print (Node dna score Nil Nil) n=printbars n ++dna++"\n"
tree_print (Node dna score left right) n=printbars n  ++show score ++"\n"++tree_print left (n+1) ++tree_print right (n+1)
draw :: DNATree -> [Char]
draw Nil =""
draw (Node dna score left right)=show score ++"\n"++tree_print left 1 ++tree_print right 1

-- ---------------------------------------------------------------------------
--				PART 4
-- ---------------------------------------------------------------------------
--
--
-- Our score function is inefficient due to repeated calls for the same 
-- suffixes. Lets make a dictionary to remember previous results. First you
-- will consider the dictionary as a list of tuples and write a lookup
-- function. Return Nothing if the element is not found. Also write the 
-- insert function. You can assume that the key is not already there.
type Dict a b = [(a,b)]
diction=[(1,2), (3,1),(4,1)]


lookupDict :: (Eq a) => a -> Dict a b -> Maybe b
lookupDict key []= Nothing
lookupDict key (x:xs)
		|key==fst x=Just (snd x) 
		|otherwise=lookupDict key xs 

insertDict :: (Eq a) => a -> b -> (Dict a b)-> (Dict a b)
insertDict key value dict=dict++[(key,value)]

-- We will improve the score function to also return the alignment along
-- with the score. The aligned DNA strands will have gaps inserted. You
-- can represent a gap with "-". You will need multiple let expressions 
-- to destructure the tuples returned by recursive calls.
--alignment :: String -> String -> ((String, String), Int)
--alignment :: String -> String -> (String, String)

maxisecond::[((String,String),Int)]->((String,String),Int)
maxisecond []=(([],[]),0)
maxisecond (x:[])=x
maxisecond (x:y:xs)
		|snd x>snd y=maxisecond (x:xs)
		|otherwise=maxisecond (y:xs)

alignment :: String -> String -> ((String, String), Int)
alignment [] []=(([],[]),0)
alignment x []=((x,[]),0)
alignment [] y=(([],y),0)
alignment (x:xs)(y:ys)
			|x==y=let ((a,b),c)=alignment xs ys; 
				 	  ((c1,c2),score2)=alignment (x:xs) (ys);
				 	  ((d1,d2),score3)=alignment (xs) (y:ys)
				  in   maxisecond [(("-"++c1,[y]++c2),score2+1), (([x]++a,[y]++b),c+3),(([x]++d1,"-"++d2),score3+1)]

		    |otherwise=let ((a,b),c)=alignment xs ys; 
				 	  ((c1,c2),score2)=alignment (x:xs) (ys);
				 	  ((d1,d2),score3)=alignment (xs) (y:ys)
				  in   maxisecond [(("-"++c1,[y]++c2),score2+1),(([x]++a,[y]++b),c+2),(([x]++d1,"-"++d2),score3+1)]
		

-- We will now pass a dictionary to remember previously calculated scores 
-- and return the updated dictionary along with the result. Use let 
-- expressions like the last part and pass the dictionary from each call
-- to the next. Also write logic to skip the entire calculation if the 
-- score is found in the dictionary. You need just one call to insert.




type ScoreDict = Dict (DNA,DNA) Int
dictionary=[]

apply_maps_x::String->ScoreDict->ScoreDict
apply_maps_x x dicts =map (\((a,b),c)->((x++a,b),c)) (dicts)

apply_maps_y::String->ScoreDict->ScoreDict
apply_maps_y y dicts =map (\((a,b),c)->((a,y++b),c)) (dicts)


scoreMemo :: (DNA,DNA) -> ScoreDict -> (ScoreDict,Int)
scoreMemo ([],[]) scoredict=(scoredict,0)
scoreMemo x scoredict
				|lookupDict x scoredict==Nothing=let (dna_strands,value)=alignment (fst x) (snd x)
												in (insertDict dna_strands value scoredict,value)
				|otherwise=let (Just value)=lookupDict x scoredict
						   in (scoredict,value)   
{- I tired doing the problem as mentioned by passing dictionary to other recursive call for all possibilities
-scoreMemo ([],[]) scoredict=(scoredict,0)
scoreMemo (x,[]) scoredict=(scoredict,0)
scoreMemo ([],y) scoredict=(scoredict,0)
scoreMemo (x:xs,y:ys) scoredict
			|lookupDict ([x],[y]) scoredict/=Nothing= (scoredict,0)
			|otherwise=let (dna_strands, value)=alignment [x] [y];
						   dict1=apply_maps_x ([x]) (fst(scoreMemo (xs,y:ys) (insertDict dna_strands value scoredict)));
						   dict2=apply_maps_y ([y]) (fst(scoreMemo (x:xs,y:ys) (insertDict dna_strands value dict1)));
						   in (dict2,value)
						 --}
-- In this part, we will use an alternate representation for the 
-- dictionary and rewrite the scoreMemo function using this new format.
-- The dictionary will be just the lookup function so the dictionary 
-- can be invoked as a function to lookup an element. To insert an
-- element you return a new function that checks for the inserted
-- element and returns the old dictionary otherwise. You will have to
-- think a bit on how this will work. An empty dictionary in this 
-- format is (\_->Nothing)

type Dict2 a b = a->Maybe b
insertDict2 :: (Eq a) => a -> b -> (Dict2 a b)-> (Dict2 a b)
insertDict2 = undefined

type ScoreDict2 = Dict2 (DNA,DNA) Int

scoreMemo2 :: (DNA,DNA) -> ScoreDict2 -> (ScoreDict2,Int)
scoreMemo2 = undefined

-- ---------------------------------------------------------------------------
-- 				PART 5
-- ---------------------------------------------------------------------------

-- Now, we will try to find the mutationDistance between two DNA sequences.
-- You have to calculate the number of mutations it takes to convert one 
-- (start sequence) to (end sequence). You will also be given a bank of 
-- sequences. However, there are a couple of constraints, these are as follows:

-- 1) The DNA sequences are of length 8
-- 2) For a sequence to be a part of the mutation distance, it must contain 
-- "all but one" of the neuclotide bases as its preceding sequence in the same 
-- order AND be present in the bank of valid sequences
-- 'AATTGGCC' -> 'AATTGGCA' is valid only if 'AATTGGCA' is present in the bank
-- 3) Assume that the bank will contain valid sequences and the start sequence
-- may or may not be a part of the bank.
-- 4) Return -1 if a mutation is not possible

	
-- mutationDistance "AATTGGCC" "TTTTGGCA" ["AATTGGAC", "TTTTGGCA", "AAATGGCC" "TATTGGCC", "TTTTGGCC"] == 3
-- mutationDistance "AAAAAAAA" "AAAAAATT" ["AAAAAAAA", "AAAAAAAT", "AAAAAATT", "AAAAATTT"] == 2

mutationDistance :: DNA -> DNA -> [DNA] -> Int
mutationDistance=undefined
-- ---------------------------------------------------------------------------
-- 				PART 6
-- ---------------------------------------------------------------------------
--
-- Now, we will write a function to transcribe DNA to RNA. 
-- The difference between DNA and RNA is of just one base i.e.
-- instead of Thymine it contains Uracil. (U)
--
transcribeDNA :: DNA -> RNA
transcribeDNA []=[]
transcribeDNA (x:xs)
	|x=='T'=['U']++transcribeDNA xs
	|otherwise=[x]++transcribeDNA xs

-- Next, we will translate RNA into proteins. A codon is a group of 3 neuclotides 
-- and forms an aminoacid. A protein is made up of various amino acids bonded 
-- together. Translation starts at a START codon and ends at a STOP codon. The most
-- common start codon is AUG and the three STOP codons are UAA, UAG and UGA.
-- makeAminoAcid should return Nothing in case of a STOP codon.
-- Your translateRNA function should return a list of proteins present in the input
-- sequence. 
-- Please note that the return type of translateRNA is [String], you should convert
-- the abstract type into a concrete one.
-- You might wanna use the RNA codon table from 
-- https://www.news-medical.net/life-sciences/RNA-Codons-and-DNA-Codons.aspx
-- 




codonTable = [("UUG","Leu"),("CUU","Leu"),("CUC","Leu"),("CUA","Leu"),("CUG","Leu"),("AUU","Ile"),("AUC","Ile"),("AUA","Ile"),
            ("UGU","Cys"),("UGC","Cys"),("UGG","Trp"),("CAU","His"),("CAC","His"),("CAA","Gln"),("AAG","Lys"),
            ("ACG","Thr"),("GUU","Val"),("GUC","Val"),("GUA","Val"),("GUG","Val"),("GCU","Ala"),("GCC","Ala"),("GCA","Ala"),("GCG","Ala"),("UAU","Tyr"),("UAC","Tyr"),
            ("GAU","Asp"),("GAC","Asp"),("GAA","Glu"),("GAG","Glu"),("GGU","Gly"),("GGC","Gly"),("GGA","Gly"),("GGG","Gly"),("AGA","Arg"),("AGG","Arg"),("AGU","Ser"),
            ("UCU","Ser"),("UCC","Ser"),("UCA","Ser"),("UCG","Ser"),("CCU","Pro"),("CCC","Pro"),("CCA","Pro"),("CCG","Pro"),("ACU","Thr"),("ACC","Thr"),("ACA","Thr"),	
            ("AGC","Ser"),("CGU","Arg"),("CGC","Arg"),("CGA","Arg"),("CGG","Arg"),("CAG","Gln"),("AUG","Met"),("UUU","Phe"),("UUC","Phe"),("UUA","Leu"),("AAU","Asn"),("AAC","Asn"),("AAA","Lys")]

makeAminoAcid :: Codon -> AminoAcid
makeAminoAcid []=Nothing
makeAminoAcid x=lookup x codonTable

amino :: RNA->[String]
amino []=[]
amino(x:[])=[]
amino(x:y:[])=[]
amino (w:x:y:z)=case makeAminoAcid ([w]++[x]++[y]) of 
				Just n ->[n]++amino z
				Nothing->[]	


translateRNA :: RNA -> [String]
translateRNA []=[]
translateRNA (x:[])=[]
translateRNA (x:y:[])=[]
translateRNA (w:x:y:z)
		|([w]++[x]++[y])=="AUG"=amino (w:x:y:z)
		|otherwise= translateRNA ([x]++[y]++z)
		

