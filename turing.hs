import Debug.Trace
-- TM, state, tape -> state, tape, direction
data Direction = Lft | Rt
instance Eq Direction where
    Lft == Lft = True
    Rt == Rt = True
    Lft == Rt = False
    Rt == Lft = False
type Input = (State, Char)
type Output = (State, Char, Direction)
data State = State { name :: String } -- make this Int so that can use array/matrix for lookup
instance Show State where
    show s = name s
instance Eq State where
    st1 == st2 = (name st1) == (name st2)
-- states and sigma doesn't actually do anything yet
-- could use it to add an integrity check on input
data TM = TM { states :: [State]
               , sigma :: String
               , delta :: Input -> Output
               , initial :: State
               , final :: State -- this should be changed to [State]
             }
-- could use 2D array instead for better runtime
mySplit :: Char -> String-> [String]
mySplit c lst = 
    foldr (\a (x:xs) -> if a == c then ([]:x:xs) else ((a:x):xs))
    [""] lst
makeTransition :: [String] -> (Input,Output)
makeTransition (a:(b:bs):c:(d:ds):(e:es):xs) = ((State a,b),(State c,d,if e == '>' then Rt else Lft))
-- https://www.reddit.com/r/haskell/comments/1psdai/is_there_a_foldwhile_function_in_haskell/ 
-- actually this is just scanl or scanr
transitionList :: [(Input,Output)] -> Input -> Output
transitionList lst input =
    foldr (\ (lstin,lstout) b ->
                if lstin == input
                    then lstout
                    else b)
          (State "reject",'x',Rt) lst
makeDelta :: String -> Input -> Output
makeDelta inputString = transitionList .
    map (makeTransition . mySplit ',') $ mySplit '\n' inputString
-- convert Output to an effect on configuration
-- then convert configuration to Input
-- might need scanl/r here too
runTM :: TM -> String -> String
runTM m input = 
    let configuration = (initial m, "", input) :: (State,String,String)
        d = delta m
        recursTM :: (State,String,String) -> (State,String,String)
        recursTM (state,bf,af) =
            let (st,c,dir) = d (state,(if af == [] then ' ' else head af))
                as = if af == [] then [] else tail af
                (tst,tbf,taf) =  if dir == Lft then (st
                                                     ,if bf == [] then [] else (init bf)
                                                     ,(if bf == [] then [] else [last bf]) ++ [c] ++ as)
                                 else (st, bf ++ [c], as)
            in trace (name tst) $ if tst == (final m) || name tst == "reject" then (tst,"","") else recursTM (tst,tbf,taf)
        (out_state,bef,aft) = recursTM (initial m, "", input)
    in  if out_state == final m then "accept" else "reject"
-- could run as turing.exe < input.txt
main = do 
    let inputString = "0,1,1,1,>\n1,2,2,2,>\n2,3,3,3,<\n3,4,4,4,<"
    -- let inputString = getContents
    -- getContents >>= inputString
    --print $ runTM (TM [State "q0", State "q1", State "q2"] "abcd" (makeDelta inputString) (State "q0") (State "q2"))
    print $ runTM (TM [] "" (makeDelta inputString) (State "0") (State "4"))
                    "1234"
