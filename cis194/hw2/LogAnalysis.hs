{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- Exercise 1
parseMessage :: String -> LogMessage
parseMessage = createLogMessage . words

createLogMessage :: [String] -> LogMessage
-- XXX: This works, but can it be made less repetitive?
createLogMessage ("I":x:xs)   = LogMessage Info (read x) (unwords xs)
createLogMessage ("W":x:xs)   = LogMessage Warning (read x) (unwords xs)
createLogMessage ("E":x:y:xs) = LogMessage (Error (read x)) (read y) (unwords xs)
createLogMessage xs           = Unknown (unwords xs)

parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- Exercise 2
-- XXX: This feels fishy, is this access function necessary?
timestamp :: LogMessage -> TimeStamp
timestamp (Unknown _)         = 0
timestamp (LogMessage _ ts _) = ts

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree   = tree
insert msg Leaf           = Node Leaf msg Leaf
insert msg (Node lt omsg rt)
  | ts < ots  = Node (insert msg lt) omsg rt
  | otherwise = Node lt omsg (insert msg rt)
  where ts  = timestamp msg
        ots = timestamp omsg

-- Exercise 3
build :: [LogMessage] -> MessageTree
build xs = foldl (flip insert) Leaf xs

-- Exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder = inOrderHelper []

inOrderHelper :: [LogMessage] -> MessageTree -> [LogMessage]
inOrderHelper msgs Leaf = msgs
inOrderHelper msgs (Node lt msg rt) = inOrderHelper msgs lt ++ [msg] ++ inOrderHelper msgs rt

-- Exercise 5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getMessage . filter isRelevant . sortLogs
  where sortLogs = inOrder . build
        isRelevant (LogMessage (Error sev) _ _)
          | sev >= 50 = True
          | otherwise = False
        isRelevant _ = False
        getMessage (LogMessage _ _ msg) = msg
