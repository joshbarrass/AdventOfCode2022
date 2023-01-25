import System.Environment
import Data.List

-- returns the offset to the *end* of the first packet containing only
-- unique characters
findUniquePacket :: Int -> String -> Int
findUniquePacket len xs
  | (length $ nub (take len xs)) == len = len
  | otherwise = 1 + findUniquePacket len (tail xs)

findPacketStart :: String -> Int
findPacketStart = findUniquePacket 4

findMessageStart :: String -> Int
findMessageStart = findUniquePacket 14

main :: IO()
main = do
  args <- getArgs
  let filename = head args
  input <- readFile filename
  let start = findPacketStart input
  putStrLn $ "Offset to packet start: " ++ show start
  let msg = findMessageStart input
  putStrLn $ "Offset to message start: " ++ show msg
