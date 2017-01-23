import Debug.Trace
import System.IO.Unsafe
prettyTrace :: String -> a -> a
prettyTrace str expr = unsafePerformIO $ do
    putStr $ str ++ " yields "
    return expr
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
-- use 2D array instead for better runtime
mySplit :: Char -> String-> [String]
mySplit c lst = 
    foldr (\a (x:xs) -> if a == c then ([]:x:xs) else ((a:x):xs))
    [""] lst
makeTransition :: [String] -> (Input,Output)
makeTransition lst@(a:(b:bs):c:(d:ds):(e:es):xs) = ((State a,b),(State c,d,if e == '>' then Rt else Lft))
-- laziness is weird man
-- i make unspecified transitions go to reject
makeTransition _ = ((State "reject",'x'),(State "reject",'x',Rt))
-- use scanl or scanr instead
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
runTM :: TM -> String -> String
runTM m input = 
    let configuration = (initial m, "", input) :: (State,String,String)
        d = delta m
        recursTM :: (State,String,String) -> (State,String,String)
        recursTM (state,bf,af) =
            let (st,c,dir) = d (state,(if af == [] then ' ' else head af))
                as = if af == [] then [' '] else tail af
                (tst,tbf,taf) =  if dir == Lft then (st
                                                     ,if bf == [] then [] else (init bf)
                                                     ,(if bf == [] then [] else [last bf]) ++ [c] ++ as)
                                 else (st, bf ++ [c], as)
            --use state monad for ocunter =_=
            --in trace (iter ++ ": " ++ bf ++ "(q" ++ (name state) ++ ")" ++ af) $ if tst == (final m) || name tst == "reject" then (tst,"","") else recursTM (tst,tbf,taf)
            --in prettyTrace (bf ++ "(q" ++ (name state) ++ ")" ++ af) $ if tst == (final m) || name tst == "reject" then (tst,"","") else recursTM (tst,tbf,taf)
            in trace (bf ++ "(q" ++ (name state) ++ ")" ++ af) $ if tst == (final m) || name tst == "reject" then (tst,"","") else recursTM (tst,tbf,taf)
        (out_state,bef,aft) = recursTM (initial m, "", input)
    in  if out_state == final m then "accept" else "reject"
-- could run as turing.exe < input.txt
main = do 
    -- accept w = a^i b^j c^k | k = i*j
    --let transitionString = "0,1,1,1,>\n1,2,2,2,>\n2,3,3,3,<\n3,4,4,4,<"
    transitionString <- getContents 
    print $ init transitionString
    --print $ runTM (TM [State "q0", State "q1", State "q2"] "abcd" (makeDelta transitionString) (State "q0") (State "q2"))
    print $ runTM (TM [] "" (makeDelta transitionString) (State "0") (State "accept")) ""
