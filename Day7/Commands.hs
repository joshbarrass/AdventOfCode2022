module Commands
  ( parseTerminalLog
  , executeCommands
  ) where

import DirectoryTree
import Data.List

-- https://stackoverflow.com/a/6270337/7471232
--
import Data.Char (isSpace)

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace
--

data Command = Command { command :: String, output :: String } deriving Show

split :: (Eq a) => a -> [a] -> [[a]]
split c [] = []
split c xs = case ys of
                   c:zs -> y : split c zs
                   [] -> [y]
  where (y, ys) = break (==c) xs

parseTerminalLog :: String -> [Command]
parseTerminalLog = parseCommands . split '$'
  where parseCommands :: [String] -> [Command]
        parseCommands [] = []
        parseCommands (c:cs)
          | null (trim c) = parseCommands cs
          | otherwise = let
              (x, '\n':y) = break (=='\n') c
              in Command (trim x) (trim y) : parseCommands cs

parse_ls :: String -> [Node]
parse_ls output = [parseFile l | l <- lines output]
  where parseFile :: String -> Node
        parseFile l
          | take 3 l == "dir" = let dirname = trim (drop 4 l) in Directory dirname []
          | otherwise = let
              (filesize, ' ':filename) = break (==' ') l
              in File (trim filename) (read $ trim filesize)

executeCommands :: Terminal -> [Command] -> Terminal
executeCommands t [] = t
executeCommands t (c:cs)
  | take 2 (command c) == "cd" = let t' = cd t (drop 3 $ command c) in executeCommands t' cs
  | take 2 (command c) == "ls" = let
      children' = parse_ls $ output c
      t' = updateCWD t children'
      in executeCommands t' cs

testLog :: String
testLog = "$ cd /\n\
\$ ls\n\
\dir a\n\
\14848514 b.txt\n\
\8504156 c.dat\n\
\dir d\n\
\$ cd a\n\
\$ ls\n\
\dir e\n\
\29116 f\n\
\2557 g\n\
\62596 h.lst\n\
\$ cd e\n\
\$ ls\n\
\584 i\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd d\n\
\$ ls\n\
\4060174 j\n\
\8033020 d.log\n\
\5626152 d.ext\n\
\7214296 k\n"
