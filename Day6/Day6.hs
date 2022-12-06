import System.Environment
import Data.List

-- returns the offset to the packet start
findPacketStart :: String -> Int
findPacketStart (a:b:c:d:xs)
  | length (nub [a,b,c,d]) == 4 = 4
  | otherwise = 1 + findPacketStart (b:c:d:xs)

main :: IO()
main = do
  args <- getArgs
  let filename = head args
  input <- readFile filename
  let start = findPacketStart input
  putStrLn $ "Offset to packet start: " ++ show start
