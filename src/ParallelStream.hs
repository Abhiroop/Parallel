{-# LANGUAGE BangPatterns #-}
module ParallelStreams where

import Control.Monad.Par
import Control.DeepSeq

data IList a
  = Nil
  | Cons a (IVar (IList a))

--       ^  -------------
--       |        ^
--   sequential   |
--    producer    |
--              parallel
--              consumer

type Stream a = IVar (IList a)

instance NFData a => NFData (IList a) where
--  rnf Nil = r0
  rnf Nil = ()
  rnf (Cons a b) = rnf a `seq` rnf b

streamFromList :: NFData a => [a] -> Par (Stream a)
streamFromList xs = do
  var <- new
  fork $ loop xs var
  return var
  where loop [] var = put var Nil
        loop (x:xs) var = do
          tail <- new
          put var (Cons x tail)
          loop xs tail


streamFold :: (a -> b -> a) -> a -> Stream b -> Par a
streamFold fn !acc instrm = do
  ilist <- get instrm
  case ilist of
    Nil -> return acc
    Cons h t -> streamFold fn (fn acc h) t
