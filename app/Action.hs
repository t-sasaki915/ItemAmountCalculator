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

            | ChestsToRawNumberLargeChestInputUpdate MisoString
            | ChestsToRawNumberChestInputUpdate MisoString
            | ChestsToRawNumberStackInputUpdate MisoString
            | ChestsToRawNumberRemainInputUpdate MisoString
            | CalculateChestsToRawNumber

            | StackUnitInputUpdate MisoString
            | UpdateStackUnit
            deriving (Show, Eq)
