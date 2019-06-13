module TreeNFData
  ( Tree(..)
  ) where

import Control.DeepSeq (NFData, rnf)

-- class NFData a where
--   rnf :: a -> ()
--   rnf a = a `seq` ()

-- The rnf name stands for “reduce to normal-form.”
-- It fully evaluates its argument and then returns ().

data Tree a = Empty | Branch (Tree a) a (Tree a)

instance NFData a => NFData (Tree a) where
  rnf Empty = ()
  rnf (Branch l x r) = rnf l `seq` rnf x `seq` rnf r

-- The idea is to just recursively apply `rnf` to
-- the components of the data type,
-- composing the calls to rnf together with seq.
