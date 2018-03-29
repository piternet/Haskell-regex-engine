import Mon
import Reg
import RegExtra

a = Lit A
b = Lit B

-- Helper table functions
textcell align text = text ++ (replicate (align - length text) ' ')
cell align object = textcell align (show object)
topborder = do putStrLn $ "╔══════════╦" ++ (replicate 50 '═') ++ "╦═══╗"
midborder = do putStrLn $ "╠══════════╬" ++ (replicate 50 '═') ++ "╬═══╣"
botborder = do putStrLn $ "╚══════════╩" ++ (replicate 50 '═') ++ "╩═══╝"

header text = do
  putStrLn $ "╠══════════╩" ++ (replicate 50 '═') ++ "╩═══╣"
  putStrLn $ "║ " ++ textcell 64 text                 ++ "║"
  putStrLn $ "╠══════════╦" ++ (replicate 50 '═') ++ "╦═══╣"

-- Test output functions
passed_test name = do
  putStrLn $ "║ Passed   ║ " ++ textcell 49 name ++ "║ o ║"
  midborder

failed_test name expected result = do
  putStrLn $ "║ Failed   ║ " ++ textcell 49 name ++ "║ × ║ "
  putStrLn $ "║ Expected ║ " ++ cell 49 expected ++ "║   ║"
  putStrLn $ "║ Result   ║ " ++ cell 49 result   ++ "║   ║"
  midborder

test name expected result
  | expected == result = passed_test name
  | otherwise = failed_test name expected result

-- Tests
main = do
  topborder

  header "TESTING `===`"
  test "(a|b) === (b|a)" (True) (Many (a :| b) === Many (b :| a))

  header "TESTING `accepts`"
  test "'' ^= ^$" (True) (accepts Eps ([] :: [AB]))

  header "TESTING `nullable`"
  test "ab(ab)* = ø " (False) (nullable ((a :> b) :> Many (a:>b)))
  test "(ab)* = ø " (False) (nullable ((a :> b) :> Many (a:>b)))

  header "TESTING `mayStart`"
  test "a ^= ^(ab)*baa" (True) (mayStart A ((Many (a :> b) :> (b :> a)):>b))
  test "b ^= ^(ab)*baa" (True) (mayStart B ((Many (a :> b) :> (b :> a)):>b))
  test "b ^= ^(ab)*aa" (False) (mayStart B (Many (a :> b) :> (a :> a)))

  header "TESTING `match`"
  test "aabaabaa ~= ^(ab)*a(a|b)" (Just [A,A]) (match ((Many (a :> b) :> a) :> (a :| b)) [A,A,B,A,A,B,A,A])
  test "b ~= ^a*b" (Just [] :: Maybe [AB]) (match (Many a) [B])
  test "b ~= ^ab" (Nothing :: Maybe [AB]) (match (a :> b) [B])
  test "b ~= ^a*" (Just [A,A,A,A]) (match (Many a) [A,A,A,A,B,A,B,B])

  header "TESTING `search`"
  test "aababaa ~= ab(ab)*" (Just [A,B,A,B]) (search ((a :> b) :> Many (a:>b)) [A,A,B,A,B,A,A])
  test "aaaaaa ~= b" (Nothing) (search b [A,A,A,A,A,A])

  header "TESTING `findall`"
  test "aababaa ~= (ab)*" ([[],[A,B,A,B],[],[A,B],[],[],[]]) (findall (Many (a:>b)) [A,A,B,A,B,A,A])
  test "aaaaaa ~= b" ([]) (findall b [A,A,A,A,A,A])

  botborder
