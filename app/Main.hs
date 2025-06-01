{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import           Control.Lens (makeLenses, over, (^.))
import           Miso
import           Miso.String  (ms)

newtype Model = Model
    { _counter :: Int
    } deriving (Show, Eq)

data Action = IncreaseCount
            deriving (Show, Eq)

makeLenses ''Model

initModel :: Model
initModel = Model 0

updateModel :: Action -> Effect Model Action
updateModel IncreaseCount = get >>= put . over counter (+ 1)

viewModel :: Model -> View Action
viewModel mdl = div_ []
    [ text $ ms (mdl ^. counter)
    , button_ [onClick IncreaseCount] [text "INCREASE"]
    ]

main :: IO ()
main = run (startComponent $ defaultComponent initModel updateModel viewModel)
