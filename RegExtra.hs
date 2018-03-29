module RegExtra where
import Mon
import Reg
import Data.List

data AB = A | B deriving(Eq,Ord,Show)

infix 4 ===
class Equiv a where
  (===) :: a -> a -> Bool

instance (Eq c) => Equiv (Reg c) where
   r1 === r2 = (simpl r1) == (simpl r2)

instance Mon (Reg c) where
  m1 = Eps
  x <> y = x :> y
  
simpl :: (Eq c) => Reg c -> Reg c
-- basic cases --
simpl Empty = Empty
simpl Eps = Eps
simpl (Lit c) = Lit c

-- many--
simpl (Many Empty) = Eps
simpl (Many Eps) = Eps
simpl (Many (Many x)) = simpl (Many (simpl x))
simpl (Many x) = Many (simpl x)

-- brackets --
simpl (x :| (y1 :| y2)) = simpl ((x :| y1) :| y2)
simpl (x :> (y1 :> y2)) = simpl ((x :> y1) :> y2)

-- alternation --
simpl (x :| y)
    | x == Empty = simpl y
    | y == Empty = simpl x
    | otherwise = setToReg $ regToSet (x :| y)

-- concatenation --
simpl (x :> y)
    | (sx == Empty || sy == Empty) = Empty
    | (sx == Eps) = sy
    | (sy == Eps) = sx
    | otherwise = sx :> sy
    where sx = simpl x
          sy = simpl y

nullable :: Reg c -> Bool
nullable Eps = True
nullable Empty = False
nullable (Lit a) = False
nullable (Many x) = True
nullable (x :| y) = (nullable x) || (nullable y)
nullable (x :> y) = (nullable x) && (nullable y)


empty :: Eq c => Reg c -> Bool 
empty r = r === Empty

der :: Eq c => c -> Reg c -> Reg c
der c x = simpl $ simplDer c x
    where
        simplDer c Empty = Empty
        simplDer c Eps = Empty
        simplDer c (Lit a)
            | a == c = Eps
            | otherwise = Empty
        simplDer c (Many x) = (simplDer c x) :> (Many x)
        simplDer c (x :| y) = (simplDer c x) :| (simplDer c y)
        simplDer c (x :> y)
            | nullable x = ((simplDer c x) :> y) :| (simplDer c y)
            | otherwise = (simplDer c x) :> y 

ders :: Eq c => [c] -> Reg c -> Reg c
ders c r = simpl $ foldl (\x y -> simpl (der y x)) r c

accepts :: Eq c => Reg c -> [c] -> Bool
accepts r w = nullable $ ders w r

mayStart :: Eq c => c -> Reg c -> Bool
mayStart c r = not $ empty (der c r)

match :: Eq c => Reg c -> [c] -> Maybe [c]
match r w = matchHelper r w [] [] r
    where
        matchHelper r [] _ [] ro
            | nullable ro = Just []
            | otherwise = Nothing
        matchHelper r [] _ res _ = Just (reverse $ res)
        matchHelper r (h:t) acc res ro
            | nullable d = matchHelper d t (h:acc) (h:acc) ro
            | otherwise = matchHelper d t (h:acc) res ro
            where d = der h r

search :: Eq c => Reg c -> [c] -> Maybe [c]
search r []
    | nullable r = Just []
    | otherwise = Nothing
search r (h:t) 
    | mh == Nothing || mh == (Just []) = search r t
    | otherwise = mh
    where mh = match r (h:t)

findall :: Eq c => Reg c -> [c] -> [[c]]
findall r w = foldl (\x y -> let my = (match r y) in if my == Nothing then x else (fromJust my):x) [] (reverse $ suffixes w)
    where
        suffixes [] = []
        suffixes l@(_:s) = l:(suffixes s)
        fromJust (Just x) = x

char :: Char -> Reg Char
char = Lit

string :: [Char] -> Reg Char
string = foldr1 (:>) . map Lit

alts :: [Char] -> Reg Char
alts = foldr1 (:|) . map Lit

letter = alts ['a'..'z'] :| alts ['A'..'Z']
digit = alts ['0'..'9']
number = digit :> Many digit
ident = letter :> Many (letter :| digit)

many1 r = r :> Many r

-- simpl helpers --

regToSet :: Eq c => Reg c -> [Reg c]
regToSet (x :| y) = nub $ (regToSet x) ++ (regToSet y)
regToSet x = [simpl x]

setToReg :: Eq c => [Reg c] -> Reg c
setToReg [] = Empty
setToReg (h:t) = simpl2 $ h :| (setToReg t)
    where
        simpl2 (x :| Empty) = x
        simpl2 (Empty :| x) = x
        simpl2 x = x