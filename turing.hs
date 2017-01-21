import Debug.Trace
-- should generalize this stuff so don't have to copy paste code between DFA and TM impl in haskell
data State = State { name :: String }
instance Show State where
    show s = name s
-- this could be more readable...
-- use type variable instead perhaps 
-- maybe should also have type for configuration
type Input = 
type TransitionFunction = Input -> Output
data Direction = Left | Right
instance Show Direction where
    show Left = '<'
    show Right = '>'
-- states and sigma doesn't actually do anything yet
-- could use it to add an integrity check on input
-- initial and final not used yet, should be used to output "accept" or "reject"
data TM = TM { states :: [State]
               , sigma :: String
               , gamma :: String
               , delta :: State -> Char -> Char -> (State, Char, Direction, String)
               , initial :: State
               , final :: State
               }
-- could use 2D array instead for better runtime
mySplit :: Char -> String-> [String]
mySplit c lst = 
    foldr (\a (x:xs) -> if a == c then ([]:x:xs) else ((a:x):xs))
    [""] lst
makeTransition :: [String] -> (State,Char,Char,State,Char,Direction)
makeTransition (a:(b:bs):(c:cs):d:(e:es):(f:fs):xs) = (State a,b,c,State d,e, if f == '<' then Left else Right)
transitionList :: [(State,Char,Char,State,Direction)] -> State -> Char -> Char -> (State,Char,Direction)
-- https://www.reddit.com/r/haskell/comments/1psdai/is_there_a_foldwhile_function_in_haskell/ 
transitionList lst st c tt =
    foldr (\ (tstate,tinput,ttape,toutput,twrite,tdir) b -> 
                if tstate == name st && tinput == c && ttape == tt
                    then (toutput,twrite,tdir)
                    else b)
          (State "reject") lst
makeDelta :: String -> State -> Char -> Char -> (State, Char, Direction)
makeDelta inputString = transitionList .
    map (makeTransition . mySplit ',') $ mySplit '\n' inputString
runTM :: TM -> String -> State
runTM m = foldr (\a b -> (delta m) b a) (initial m)
main = do 
    let inputString = "q0,0,q1\nq1,0,q2"
    print $ runDFA (DFA [State "q0", State "q1", State "q2"] "abcd" (makeDelta inputString) (State "q0") (State "q2"))
                    "00"
    --print $ runDFA (DFA [] "" (makeDelta inputString) (State "q0") (State "q2"))
