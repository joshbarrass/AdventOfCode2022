import System.Environment
import Crates
import Moves
import Data.Stack

main :: IO()
main = do
  args <- getArgs
  let filename = head args
  input <- readFile filename
  let l = lines input
  let diagram = parseDiagram $ extractDiagram l
  let moves = parseMoves $ extractMoves l
  let diagram' = executeMoves diagram moves
  let topline = [snd $ (maybe (stackNew, ' ') id) $ stackPop s | s <- diagram']
  putStrLn topline
