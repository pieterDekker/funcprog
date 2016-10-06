selfrle :: [Int]
<<<<<<< HEAD
selfrle = recRlSeq [1,2,2] [1,2,2]

options :: [Char] -> [Char]


-- xs is the list with x1 , x2 and x3

recRlSeq :: [Int] -> [Int] -> [Int]
recRlSeq xs seq
  | options xs seq == ""
  | options xs seq == ""

=======
-- Insert your own code here.
>>>>>>> 060ea4de83b47f92a3f38af63147af988d571505





-- Do not change the fololowing wrapper code
wrapper :: String -> [Int]
wrapper input = take (read input::Int) selfrle

main =  print . wrapper =<< getLine
