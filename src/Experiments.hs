module Experiments where

import Control.Monad.Par hiding (spawn)

spawn :: NFData a => Par a -> Par (IVar a)
spawn p = do
  i <-  new
  fork $ do x <- p; put i x
  return i

parMapM :: NFData b => (a -> Par b) -> [a] -> Par [b]
parMapM f as = do
  ibs <- mapM (spawn . f) as
  mapM get ibs

parMap :: NFData b => (a -> b) -> [a] -> Par [b]
parMap f as = do
  ibs <- mapM (spawn . return . f) as
  mapM get ibs
