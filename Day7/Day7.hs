import System.Environment
import Commands
import DirectoryTree

maxDirSize :: Int
maxDirSize = 100000

filesystemSize :: Int
filesystemSize = 70000000

freeSpaceNeeded :: Int
freeSpaceNeeded = 30000000

main :: IO()
main = do
  args <- getArgs
  let filename = head args
  input <- readFile filename
  let commands = parseTerminalLog input
  let t' = executeCommands newTerminal commands
  let totalSpaceUsed = getSize (root t')
  putStrLn $ "Total space used: " ++ show totalSpaceUsed
  let currentFreeSpace = filesystemSize - totalSpaceUsed
  let minDeletionSize = freeSpaceNeeded - currentFreeSpace
  putStrLn $ "Must delete at least " ++ show minDeletionSize
  let dirSizes = map (\(node, size) -> (name node, size)) $ filter (isDir . fst) (getAllNodeSizes t')
  let sizes = map snd dirSizes
  let smallestToDelete = minimum (filter (>=minDeletionSize) sizes)
  putStrLn $ "Size of smallest dir you can delete to free enough space: " ++ show smallestToDelete
