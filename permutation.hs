import Data.Array.IArray
import Data.Set (Set, member, insert, empty)


type IIArray = Array Integer Integer
type ISet = Set Integer
type Cycle = [Integer]

perm' :: [Integer] -> [Integer] -> ([Integer], [Integer])
perm' [] res = ([], res)
perm' [x] res = ([], x:res)
perm' (x:xx:xs) buf = perm' (xs ++ [xx]) (x:buf)

build :: [Integer] -> IIArray
build a = array (lower, upper) $ zip [lower..upper] $ snd $ perm' a []
  where
    lower :: Integer
    lower = 1
    upper :: Integer
    upper = toInteger $ length a

splitToCycles :: [Cycle] -> IIArray -> ISet -> Integer -> Integer -> Bool -> [Cycle]
splitToCycles currentCycles permArray used currentNode firstUnvisited False =
  if currentNode >= (toInteger $ length $ indices permArray)
    then currentCycles
    else splitToCycles ([]:currentCycles) permArray used firstUnvisited (succ firstUnvisited) True

splitToCycles currentCycles permArray used currentNode firstUnvisited True =
  if currentNode `member` used
    then splitToCycles currentCycles permArray used currentNode firstUnvisited False
    else splitToCycles updatedCycles permArray updatedUsedSet newCurrentNode newUnvisited True
      where
        updatedCycles = case currentCycles of
          [] -> [(currentNode:[])]
          _ -> (currentNode: (head currentCycles)):(tail currentCycles)
        updatedUsedSet = insert currentNode used
        newCurrentNode = permArray ! currentNode
        newUnvisited = if (succ currentNode) == firstUnvisited
                          then succ firstUnvisited
                          else firstUnvisited

getCycles :: Integer -> [Cycle]
getCycles n = filter (not . null) $ splitToCycles [] (build [1..n]) empty 1 1 True

orderOfPermutation :: Integer -> Integer
orderOfPermutation n = foldr1 lcm $ map (toInteger . length) $ getCycles n
