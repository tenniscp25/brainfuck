{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Brainfuck (evalOps, parseOps)
import Types (EvalState (..))

main :: IO ()
main = do
  -- hello world
  let hwCode =
        ">++++++++[<+++++++++>-]<.>++++[<+++++++>-]<+.+++++++..+++.>>++++++"
          <> "[<+++++++>-]<++.------------.>++++++[<+++++++++>-]<+.<.+++.------.--------.>>>"
          <> "++++[<++++++++>-]<+."
  hwOut <- interpretOps hwCode []
  putStrLn $ chr <$> hwOut

  -- bubble sort
  let bbCode =
        ">>,[>>,]<<[[<<]>>>>[<<[>+<<+>-]>>[>+<<<<[->]>[<]>>-]<<<[[-]>>"
          <> "[>+<-]>>[<<<+>>>-]]>>[[<+>-]>>]<]<<[>>+<<-]<<]>>>>[.>>]"
  bbOut <- interpretOps bbCode [4, 2, 3, 1, 5, 6, 7, 9, 8, 0]
  print bbOut

  -- squared 1 - 10
  let sqCode =
        "+[>++<-]>[<+++++>-]+<+[>[>+>+<<-]++>>[<<+>>-]>>>[-]++>[-]+>>>"
          <> "+[[-]++++++>>>]<<<[[<++++++++<++>>-]+<.<[>----<-]<]<<[>>>>>[>>>[-]+++++++++<"
          <> "[>-<-]+++++++++>[-[<->-]+[<<<]]<[>+<-]>]<<-]<<-]"
  sqOut <- interpretOps sqCode []
  putStrLn $ chr <$> sqOut

interpretOps :: (MonadFail m) => Text -> [Int] -> m [Int]
interpretOps s i = do
  ops <- parseOps s
  st <- execStateT (evalOps ops) (EvalState i mempty (replicate 256 0) 0)
  pure $ toList (_stOutput st)
