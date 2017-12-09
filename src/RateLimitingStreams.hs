{-# LANGUAGE BangPatterns #-}
module RateLimitingStreams where

import Control.Monad.Par
import Control.DeepSeq

data IList a
  = Nil
  | Cons a (IVar (IList a))
  | Fork (Par ()) (IList a)

type Stream a = IVar (IList a)

instance NFData a => NFData (IList a) where
  rnf Nil = ()
  rnf (Cons a b) = rnf a `seq` rnf b
  rnf (Fork p l) = p `seq` rnf l

type ForkDistance = Int
type ChunkSize    = Int

streamFromList :: NFData a => ForkDistance -> ChunkSize -> [a] -> Par (Stream a)
streamFromList f c xs = do
  var <- new
  fork $ loop f c xs var
  return var
  where loop _ _ [] var = put var Nil
        loop 0 c (x:xs) var = do
          tail <- new
          let op = loop c c xs tail
          put var (Fork op (Cons x tail))
        loop f c (x:xs) var = do
          tail <- new
          put var (Cons x tail)
          loop (f - 1) c xs tail

streamFold :: (a -> b -> a) -> a -> Stream b -> Par a
streamFold fn !acc instrm = acc `seq` do
  ilst <- get instrm
  case ilst of
    Cons h t          -> streamFold fn (fn acc h) t
    Fork p (Cons h t) -> fork p >> streamFold fn (fn acc h) t
    _                 -> return acc

streamMap :: NFData b => ForkDistance
                      -> ChunkSize
                      -> (a -> b)
                      -> Stream a
                      -> Par (Stream b)
streamMap f c fn instrm = do
  outstrm <- new
  fork $ loop f c instrm outstrm
  return outstrm
  where
    loop 0 c instrm outstrm = do
      l <- get instrm
      let op = loop c c instrm outstrm
      case l of
        x -> undefined --put instrm (Fork op x)
    loop f c instrm outstrm = do
      ilist <- get instrm
      case ilist of
        Nil -> put outstrm Nil
        Cons h t -> do
          newtl <- new
          put outstrm (Cons (fn h) newtl)
          loop (f-1) c t newtl
        Fork op (Cons h t) -> do
          fork op
          newtl <- new
          put outstrm (Cons (fn h) newtl)
          loop (f-1) c t newtl
