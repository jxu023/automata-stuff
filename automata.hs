-- using Input = State -> char doesn't really help at all
-- shouldn't really want a State -> Char function anyway right?
type Input = (State,Char)
type Output = State
data State = State { name :: String } -- make this Int so that can use array/matrix for lookup
instance Show State where
    show s = name s
instance Eq State where
    st1 == st2 = (name st1) == (name st2)
-- states and sigma doesn't actually do anything yet
-- could use it to add an integrity check on input
-- initial and final not used yet, should be used to output "accept" or "reject"
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
makeTransition (a:(b:bs):c:xs) = ((State a,b),State c)
transitionList :: [(Input,Output)] -> Input -> Output
-- also try the combination of foldr and takeWhile .. funciton composition , let acc = State "Reject" in foldr acc . takewhile
transitionList lst input =
    foldr (\ (lstin,lstout) b ->
                if lstin == input
                    then lstout
                    else b)
          (State "reject") lst
makeDelta :: String -> Input -> Output
makeDelta inputString = transitionList .
    map (makeTransition . mySplit ',') $ mySplit '\n' inputString
runDFA :: DFA -> String -> State
runDFA m = foldr (\a b -> (delta m) (b,a)) (initial m)
main = do 
    let inputString = "q0,0,q1\nq1,0,q2"
    print $ runDFA (DFA [State "q0", State "q1", State "q2"] "abcd" (makeDelta inputString) (State "q0") (State "q2"))
                    "00"
    --print $ runDFA (DFA [] "" (makeDelta inputString) (State "q0") (State "q2"))
