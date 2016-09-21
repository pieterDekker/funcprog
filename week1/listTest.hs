import Data.Maybe

someLast :: [Char] -> Char
someLast string = last string

someScnLast :: [Char] -> Char
someScnLast string = string !! (length string - 2)

someLast2 :: [Char] -> [Char]
someLast2 string = [(someLast string)] ++ [(someScnLast string)]

testString :: String -> Bool
testString s = s == "Hoi"

testList :: [(Float, Float)]
testList = [(4,2),(9,3),(16,4),(25,0),(36,0),(49,7)]

listCompTest :: [(Float, Float)] -> [Float]
listCompTest ips = [a / b | (a,b) <- ips, b > 0]
