module D3.Base (D3,D3Eff,Value(..)) where

import Control.Monad.Eff (Eff)

foreign import data D3 :: !

type D3Eff a = forall e. Eff (d3 :: D3 | e) a

data Value a b = ValString String | ValFn (a -> b)
