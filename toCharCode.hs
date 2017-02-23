import Data.Char (ord)
import System.Environment

main = do 
 args <- getArgs
 case args of
  [] -> do 
   putStrLn "What do you want to convert?"
   toConvert <- getLine
   putStrLn $ output toConvert
  
  xs -> do
   putStrLn $ foldr1 (\acc x -> acc ++ "\n" ++ x) $ map output xs

output :: [Char] -> [Char]
output str = beginning ++ (toOrd str) ++ ending
             where
              beginning = "String.fromCharCode("
              ending = ")"

toOrd :: [Char] -> [Char]
toOrd [] = ""
toOrd str = foldr1 addCommas $ map toNum str
            where 
             toNum x = show $ ord x
             addCommas acc x = acc ++ "," ++ x