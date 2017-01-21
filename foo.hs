import Data.Array

main = do
    let arr = array ((1,1),(2,2)) [((1,1),1),((1,2),1),((2,1),1),((2,2),1)]
    print arr
