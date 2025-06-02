module Action (Action (..)) where

import           Miso.String (MisoString)

data Action = RawNumberToShulkerBoxesInputUpdate MisoString
            | CalculateRawNumberToShulkerBoxes

            | RawNumberToChestsInputUpdate MisoString
            | CalculateRawNumberToChests

            | ShulkerBoxesToRawNumberShulkerBoxInputUpdate MisoString
            | ShulkerBoxesToRawNumberStackInputUpdate MisoString
            | ShulkerBoxesToRawNumberRemainInputUpdate MisoString
            | CalculateShulkerBoxesToRawNumber

            | StackUnitInputUpdate MisoString
            | UpdateStackUnit
            deriving (Show, Eq)
