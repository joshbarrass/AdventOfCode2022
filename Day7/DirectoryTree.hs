module DirectoryTree
  ( Node(..)
  , Terminal(..)
  , newTerminal
  , cd
  , updateCWD
  , getCWD
  , getAllNodeSizes
  , isDir
  ) where

import Data.List

data Node = Directory { name :: String, children :: [Node] } | File { name :: String, size :: Int } deriving (Show)

-- Terminal stores the state of the terminal as we traverse the
-- directories. root is the root node of our directory tree and cwd is
-- a list of indices that explain how to reach our current
-- working directory from the root node based on the orderin
data Terminal = Terminal { root :: Node, cwd :: [Int] } deriving (Show)

newTerminal :: Terminal
newTerminal = Terminal (Directory "/" []) []

foldCWD :: (a -> Node -> a) -> a -> Node -> [Int] -> a
foldCWD f a n [] = f a n
foldCWD f a n (x:xs) = f (foldCWD f a (children n !! x) xs) n

-- returns the cwd Node
-- getCWD :: Terminal -> Node
-- getCWD t = last $ traverse (root t) (cwd t)
--   where traverse = foldCWD (\xs x -> x:xs) []

getCWD :: Terminal -> Node
getCWD t = traverse (root t) (reverse $ cwd t)
  where traverse :: Node -> [Int] -> Node
        traverse node [] = node
        traverse node (x:xs) = children (traverse node xs) !! x 

replaceChildren :: Node -> [Node] -> Node
replaceChildren (Directory name _) c = Directory name c 

replaceChildN :: Node -> Int -> Node -> Node
replaceChildN (Directory name children) n new = Directory name children'
  where children' = take n children ++ new : drop (n+1) children

replaceCWD :: Terminal -> Node -> Terminal
replaceCWD t new = Terminal (traverseReplace (root t) (cwd t) new) (cwd t)
  where traverseReplace :: Node -> [Int] -> Node -> Node
        traverseReplace _ [] new = new
        traverseReplace node (n:ns) new = replaceChildN node n (traverseReplace nth ns new)
          where nth = children node !! n

updateCWD :: Terminal -> [Node] -> Terminal
updateCWD t children' = replaceCWD t $ replaceChildren (getCWD t) children'

cd :: Terminal -> String -> Terminal
cd t "." = t
cd (Terminal root cwd) ".." = Terminal root $ init cwd
cd (Terminal root _) "/" = Terminal root []
cd t newDir = let
  (Just index) = findIndex (\x -> (name x) == newDir) $ children (getCWD t)
  in Terminal (root t) $ cwd t ++ [index]

getSize :: Node -> Int
getSize (File _ s) = s
getSize (Directory _ children) = sum $ map getSize children

getAllNodeSizes :: Terminal -> [(Node, Int)]
getAllNodeSizes t = traverseSizes (root t)
  where traverseSizes :: Node -> [(Node, Int)]
        traverseSizes (File name size) = [(n, getSize n)]
          where n = File name size
        traverseSizes (Directory name children) = (n, getSize n) : concat [traverseSizes c | c <- children]
          where n = Directory name children

isDir :: Node -> Bool
isDir (File _ _) = False
isDir (Directory _ _ ) = True
