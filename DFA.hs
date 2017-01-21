import Debug.Trace
data State = State { name :: String }
instance Show State where
    show s = name s
-- states and sigma doesn't actually do anything yet
-- could use it to add an integrity check on input
-- initial and final not used yet, should be used to output "accept" or "reject"
data DFA = DFA { states :: [State]
               , sigma :: String
               , delta :: State -> Char -> State
               , initial :: State
               , final :: State
               }
-- could use 2D array instead for better runtime
mySplit :: Char -> String-> [String]
mySplit c lst = 
    foldr (\a (x:xs) -> if a == c then ([]:x:xs) else ((a:x):xs))
    [""] lst
makeTransition :: [String] -> (String,Char,String)
makeTransition (a:(b:bs):c:xs) = (a,b,c)
transitionList :: [(String,Char,String)] -> State -> Char -> State
transitionList lst st c =
    foldr (\ (tstate,tinput,toutput) b -> 
                if tstate == name st && tinput == c
                    then State toutput
                    else b)
          (State "reject") lst
makeDelta :: String -> State -> Char -> State
makeDelta inputString = transitionList .
    map (makeTransition . mySplit ',') $ mySplit '\n' inputString
runDFA :: DFA -> String -> State
runDFA m = foldr (\a b -> (delta m) b a) (initial m)
main = do 
    let inputString = "q0,0,q1\nq1,0,q2"
    print $ runDFA (DFA [State "q0", State "q1", State "q2"] "abcd" (makeDelta inputString) (State "q0") (State "q2"))
                    "00"
    --print $ runDFA (DFA [] "" (makeDelta inputString) (State "q0") (State "q2"))
