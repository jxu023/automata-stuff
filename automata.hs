type Input = (State,Char)
type Output = State
data State = State { name :: String } -- make this Int so that can use array/matrix for lookup
instance Show State where
    show s = name s
instance Eq State where
    st1 == st2 = (name st1) == (name st2)
-- states and sigma doesn't actually do anything yet
-- could use it to add an integrity check on input
data DFA = DFA { states :: [State]
               , sigma :: String
               , delta :: Input -> Output
               , initial :: State
               , final :: State
               }
-- could use 2D array instead for better runtime
mySplit :: Char -> String-> [String]
mySplit c lst = 
    foldr (\a (x:xs) -> if a == c then ([]:x:xs) else ((a:x):xs))
    [""] lst
makeTransition :: [String] -> (Input,Output)
-- makeTransition might be the only function needed to correct after changing Input and Output, if just changing number of inputs
makeTransition (a:(b:bs):c:xs) = ((State a,b),State c)
transitionList :: [(Input,Output)] -> Input -> Output
-- https://www.reddit.com/r/haskell/comments/1psdai/is_there_a_foldwhile_function_in_haskell/ 
-- this is scanl or scanr
transitionList lst input =
    foldr (\ (lstin,lstout) b ->
                if lstin == input
                    then lstout
                    else b)
          (State "reject") lst
makeDelta :: String -> Input -> Output
makeDelta inputString = transitionList .
    map (makeTransition . mySplit ',') $ mySplit '\n' inputString
runDFA :: DFA -> String -> String
runDFA m str = if final m == foldr (\a b -> (delta m) (b,a)) (initial m) str
               then "accept"
               else "reject"
main = do 
    let inputString = "q0,0,q1\nq1,0,q2"
    --print $ runDFA (DFA [State "q0", State "q1", State "q2"] "abcd" (makeDelta inputString) (State "q0") (State "q2"))
    print $ runDFA (DFA [] "" (makeDelta inputString) (State "q0") (State "q2"))
                    "00"
