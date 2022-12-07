import System.Environment
import Commands
import DirectoryTree

maxDirSize :: Int
maxDirSize = 100000

main :: IO()
main = do
  args <- getArgs
  let filename = head args
  input <- readFile filename
  let commands = parseTerminalLog input
  let t' = executeCommands newTerminal commands
  let dirSizes = map (\(node, size) -> (name node, size)) $ filter (isDir . fst) (getAllNodeSizes t')
  let sizes = map snd dirSizes
  let smallSizes = filter (<= maxDirSize) sizes
  let s = sum smallSizes
  putStrLn $ "Result: " ++ show s
