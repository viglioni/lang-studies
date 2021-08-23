module Lib
  ( someFunc
  ) where

import Crypto.Lol.Types.Unsafe.Complex (fromReal)

someFunc :: IO ()
someFunc = do
  let a = fromReal (2 :: Int)
  print a
