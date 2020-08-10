import Prelude

type Sudoku = [[Int]]

beg :: Int -> Int
beg a
	| 0 <= a && a < 3 = 0
	| 3 <= a && a < 6 = 3
	| otherwise       = 6
	
end :: Int -> Int
end a
	| 0 <= a && a < 3 = 2
	| 3 <= a && a < 6 = 5
	| otherwise       = 8
	
rand = [ (x*a+b) `mod` 10 | a <- [1..9], b <- [0..9], x <- [1..9] ]

sudoku n = [ take 9 [ x | x <- (drop (n*i) rand) ] | i <- [1..9] ]

rowBuild s b = [ s!!i | i <- [b..8]]

colBuild s b c = [ s!!i!!c | i <- [b..8]]

subBuild br er bc ec s i j = [ s!!a!!b | a <- [br..er], a /= i, 
                                         b <- [bc..ec], b /= j ]
										 
validate :: Sudoku -> Int -> Int -> Int -> Int -> Int
validate s row col lim el =
	if  ((el `elem` (rowBuild (s!!row) lim))      == True) ||
	    ((el `elem` (colBuild s lim col))         == True) || 
	    ((el `elem` (subBuild (beg row) (end row) 
	     (beg col) (end col) s row col))          == True) 
		 then 0 else el		 
		  
unsolved n = [ [ validate (sudoku n) i j (i+1) ((sudoku n)!!i!!j) | j <- [0..8]] 
                                                                  | i <- [0..8]]
														
right s p = [ x | x <- [((s!!(fst p)!!(snd p))+1)..9], 
                 (validate s (fst p) (snd p) 0 x) > 0]

type Index = (Int,Int)

indexes s = [ (i,j) | i <- [0..8], j <- [0..8], s!!i!!j == 0 ]

insert s p el = [ [ if i == (fst p) && j == (snd p)
					then el else (s!!i!!j) | j <- [0..8]] | i <- [0..8] ]

solve :: Sudoku -> [Index] -> Int -> [Int] -> Int -> Int -> Sudoku
solve cur pos at r cnt max
  | (cnt == max) || (at > ((length pos)-1)) = []
  | ((pos!!at) == (head pos)) = new
  | (length r) > 0 = solve new pos (at-1) (right new (pos!!(at-1))) (cnt+1) max
  | otherwise = solve prev pos (at+1) (right cur (pos!!(at+1))) (cnt+1) max
	where new  = insert cur (pos!!at) (head r)
	      prev = insert cur (pos!!(at+1)) 0
		  
solved n = solve begin inds ((length inds)-1) (right begin (inds!!0)) 0
                 ((length (indexes (unsolved n)))^2) 
				 where begin = (unsolved n)
				       inds  = indexes (unsolved n)		

printSudoku [] = return ()
printSudoku s  = do putStrLn (show (head s));
				    printSudoku (tail s);
			
tomiSudoku n = do putStrLn "Your sudoku: ";
				  printSudoku (unsolved n);
				  putStrLn "My calculations: ";
				  if null (solved n) then putStrLn "Unsolvable sudoku!"
				  else printSudoku (solved n);
				  putStrLn "Come again!";
					
main = do putStr "Gimme number: ";
		  n <- getLine;
		  tomiSudoku (read n :: Int);
		  