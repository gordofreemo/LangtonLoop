import System.IO
import Data.Map as M
import Data.Array as A
import Graphics.Gloss

-- Neumann Neighborhood
data Neighbourhood a = Neighbourhood {
  left :: a,
  top :: a,
  right :: a,
  bottom :: a,
  center :: a
  }
  deriving (Eq, Show)


-- Parse the langton transition table, where each String in the list of Strings
-- is a single line from the table. Returns a hashmap which maps neighbor 
-- encoding to next state. Also computes the symmetries of table automatically
readTable :: [String] -> Map Int Int
readTable xs = fromList kv
  where kv = concatMap (\x ->  f1 (Prelude.splitAt (length x - 1) x)) xs
        f1 (a,b) = fmap (kmap read) (Prelude.map (\x -> (x,b)) (symmetry a))
        kmap f (a,b) = (f a, f b)
        symmetry [a,b,c,d,e] = [[a,b,c,d,e],[a,e,b,c,d],[a,d,e,b,c],[a,c,d,e,b]]


-- We encode the Neighborhoods as integers as follows
-- If the Neumann neighborhood is 
--    x1 
--x2  x3  x4 
--    x5 
-- Then we represent it as the integer x3x1x4x5x2 , where xi is interpreted as a digit 
encodeNeighbourhood :: Neighbourhood Int -> Int
encodeNeighbourhood Neighbourhood {left=x2,top=x1,right=x4,bottom=x5,center=x3} =
  sum $ zipWith (*) [x2,x5,x4,x1,x3] [1,10,100,1000,10000]

nextState :: Map Int Int -> Neighbourhood Int -> Int
nextState m n = m M.! encodeNeighbourhood n

-- Get list of row/columns indices 
rowIndices :: Enum a => Array (a, b) e -> [a]
rowIndices a = [min..max] where ((min,_),(max,_)) = bounds a
colIndices :: Enum a1 => Array (a2, a1) e -> [a1]
colIndices a = [min..max] where ((_,min),(_,max)) = bounds a

-- Get ordered pair of row/column indices
rowIndices' :: Array (a,b) e -> (a,a)
rowIndices' a = (min,max) where ((min,_),(max,_)) = bounds a
colIndices' :: Array (a,b) e -> (b,b)
colIndices' a = (min,max) where ((_,min),(_,max)) = bounds a

-- get row/col k from array 
row :: (Enum t, Ix a2, Ix t) => Array (a2, t) a -> a2 -> [a]
row a j = [a A.! (j,k) | k <- colIndices a]
col :: (Enum t, Ix t, Ix b) => Array (t, b) a -> b -> [a]
col a j = [a A.! (k,j) | k <- rowIndices a]

-- Technically, this only work if we go out of bounds over/under 1, 
-- but this assumption is upheld by the current code 
(%) :: Array (Int, Int) a -> (Int,Int) -> a
a % (n,m) = a A.! (n',m')
  where n'
          | n < minr = maxr
          | n > maxr = minr
          | otherwise = n
        m'
          | m < minc = maxc
          | m > maxc = minc
          | otherwise = m
        (minr,maxr) = rowIndices' a
        (minc,maxc) = colIndices' a

emptyArray :: a -> Int -> Int -> Array (Int, Int) a
emptyArray i n m = listArray bounds $ replicate (n*m) i
  where bounds = ((0,0),(n-1,m-1))

-- Computer Neumann neighbourhood of given space 
neighbours :: Array (Int, Int) e -> (Int, Int) -> Neighbourhood e
neighbours a (n,m) = Neighbourhood x1 x2 x3 x4 x5
  where x1 = a % (n,m-1)
        x2 = a % (n-1,m)
        x3 = a % (n,m+1)
        x4 = a % (n+1,m)
        x5 = a % (n,m)

-- Apply transition table to given automaton state 
updateLangton :: Array (Int, Int) Int -> Map Int Int -> Array (Int, Int) Int
updateLangton a m = listArray (bounds a) [nextState m (neighbours a (j,k)) | j <- rows, k <- cols]
  where rows = rowIndices a
        cols = colIndices a
        row' = rowIndices' a
        col' = colIndices' a

expandLangton :: Array (Int, Int) Int -> Int -> Array (Int, Int) Int
expandLangton a x = listArray v2 [convert (j,k) | j <- [row1'..row2'], k <- [col1'..col2']]
  where ((row1,col1),(row2,col2)) = bounds a
        v2@((row1',col1'),(row2',col2')) = newBounds a x
        convert (x,y) = if (x < row1) || (x > row2) || (y < col1) || (y > col2) then 0 else a A.! (x,y)

-- If the Langton array has pushes up against edges of array, 
-- give new bounds with expanded size x on whatever side is being pushed against
newBounds :: Array (Int, Int) Int -> Int -> ((Int, Int),(Int,Int))
newBounds a x = ((newFstR, newFstC),(newLastR,newLastC))
  where firstRow = fst $ rowIndices' a
        lastRow  = snd $ rowIndices' a
        firstCol = fst $ colIndices' a
        lastCol  = snd $ colIndices' a
        newFstR  = if all (== 0) (row a firstRow) then firstRow else firstRow - x
        newLastR = if all (== 0) (row a lastRow)  then lastRow  else lastRow  + x
        newFstC  = if all (== 0) (col a firstCol) then firstCol else firstCol - x
        newLastC = if all (== 0) (col a lastCol)  then lastCol  else lastCol  + x


langtonString :: Array (Int, Int) Int -> String
langtonString a = concat [ concatMap p (row a m) ++ "\n" | m <- rowIndices a]
  where p 0 = "."
        p x = show x

langtonLoop board rules currStep maxStep = do
  if currStep == maxStep then return ()
  else do
    print $ "time t = " ++ show currStep
    putStrLn $ langtonString board 
    let newBoard = expandLangton board 5
    langtonLoop (updateLangton newBoard rules) rules (currStep + 1) maxStep


langtonTable :: String
langtonTable = "./files/langton-table.txt"

main = do
  handle <- openFile langtonTable ReadMode
  contents <- hGetContents handle
  let table = readTable (words contents)
  langtonLoop startLoop table 0 500
  hClose handle

startLoop :: Array (Int, Int) Int
startLoop =  listArray ((0,0),(9,14)) $ concatMap (fmap (read . return)) ["022222222000000",
       "217014014200000",
       "202222220200000",
       "272000021200000",
       "212000021200000",
       "202000021200000",
       "272000021200000",
       "212222221222220",
       "207107107111112",
       "022222222222220"]