selfrle :: [Int]
selfrle = recRlSeq [1,2,2] [1,2,2]

options :: [Char] -> [Char]


-- xs is the list with x1 , x2 and x3

recRlSeq :: [Int] -> [Int] -> [Int]
recRlSeq xs seq
  | options xs seq == ""
  | options xs seq == ""






-- Do not change the fololowing wrapper code
wrapper :: String -> [Int]
wrapper input = take (read input::Int) selfrle

main =  print . wrapper =<< getLine
