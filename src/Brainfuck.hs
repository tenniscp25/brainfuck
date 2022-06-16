{-# LANGUAGE LambdaCase #-}

module Brainfuck (parseOps, opsP, evalOps) where

import Control.Lens hiding (op)
import Relude.Unsafe ((!!))
import Text.Megaparsec
  ( MonadParsec (eof),
    Parsec,
    anySingleBut,
    choice,
    errorBundlePretty,
    parse,
    some,
  )
import Text.Megaparsec.Char (char)
import Types
import Prelude hiding (Op, many, some)

parseOps :: (MonadFail m) => Text -> m [Op]
parseOps = either (fail . errorBundlePretty) pure . parse opsP "bf"

opsP :: Parsec Void Text [Op]
opsP = some opP <* eof
  where
    decP = OpDec <$ char '-'
    incP = OpInc <$ char '+'
    leftP = OpLeft <$ char '<'
    rightP = OpRight <$ char '>'
    printP = OpPrint <$ char '.'
    readP = OpRead <$ char ','
    loopP = OpLoop <$> (char '[' *> some opP <* char ']')
    opP = choice [decP, incP, leftP, rightP, printP, readP, loopP, noopP]
    noopP = OpNoop <$ anySingleBut ']'

evalOps :: (MonadFail m) => [Op] -> StateT EvalState m ()
evalOps [] = pure ()
evalOps (op' : ops') = do
  evalOp op'
  evalOps ops'
  where
    evalOp = \case
      OpDec -> do
        ptr <- gets _stPtr
        modifying stMem $ ix ptr %~ pred
      OpInc -> do
        ptr <- gets _stPtr
        modifying stMem $ ix ptr %~ succ
      OpLeft -> do
        stPtr -= 1
      OpRight -> do
        stPtr += 1
      OpPrint -> do
        ptr <- gets _stPtr
        mem <- gets _stMem
        modifying stOutput (|> mem !! ptr)
      OpRead -> do
        gets _stInput >>= \case
          [] -> fail "Insufficient input"
          n : ns -> do
            ptr <- gets _stPtr
            stInput .= ns
            modifying stMem $ ix ptr .~ n
      OpLoop ops -> evalLoop ops
      OpNoop -> pure ()
    evalLoop ops = do
      ptr <- gets _stPtr
      mem <- gets _stMem
      when (ptr < 0) $ fail (show mem)
      when (mem !! ptr /= 0) $ do
        evalOps ops
        evalLoop ops
