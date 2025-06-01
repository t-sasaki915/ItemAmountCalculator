module Action (Action (..)) where

import           Miso.String (MisoString)

data Action = RawNumberToShulkerBoxesInputUpdate MisoString
            | CalculateRawNumberToShulkerBoxes

            | RawNumberToChestsInputUpdate MisoString
            | CalculateRawNumberToChests

            | StackUnitInputUpdate MisoString
            | UpdateStackUnit
            deriving (Show, Eq)
