import System.Environment
import Data.List

-- returns the offset to the packet start
findPacketStart :: String -> Int
findPacketStart (a:b:c:d:e:f:g:h:i:j:k:l:m:n:xs)
  | length (nub [a,b,c,d,e,f,g,h,i,j,k,l,m,n]) == 14 = 14
  | otherwise = 1 + findPacketStart (b:c:d:e:f:g:h:i:j:k:l:m:n:xs)

main :: IO()
main = do
  args <- getArgs
  let filename = head args
  input <- readFile filename
  let start = findPacketStart input
  putStrLn $ "Offset to packet start: " ++ show start
