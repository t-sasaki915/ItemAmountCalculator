module Calculator.RawNumberToShulkerBoxes (RawNumberToShulkerBoxes (..)) where

import           Miso
import           Miso.String (ms)

import           Action      (Action (RawNumberToShulkerBoxesInputUpdate))
import           Calculator  (CalculationContext (..), Calculator (..))
import           Model       hiding (stackUnit)

data RawNumberToShulkerBoxes = RawNumberToShulkerBoxes

instance Calculator RawNumberToShulkerBoxes where
    inputLenses RawNumberToShulkerBoxes =
        [ rawNumberToShulkerBoxesInput
        ]

    resultLenses RawNumberToShulkerBoxes =
        [ rawNumberToShulkerBoxesShulkerBoxes
        , rawNumberToShulkerBoxesStacks
        , rawNumberToShulkerBoxesRemains
        ]

    calculatePure RawNumberToShulkerBoxes ctx inputs =
        let rawNumber = inputs !! 0
            (shulkerBoxes, remain1) = rawNumber `divMod` (27 * stackUnit ctx)
            (stacks, remain2) = remain1 `divMod` stackUnit ctx in
                [shulkerBoxes, stacks, remain2]

    viewCalculatorPure RawNumberToShulkerBoxes results =
        let shulkerBoxes = results !! 0
            stacks       = results !! 1
            remains      = results !! 2 in
                div_ []
                    [ h2_ [] [text "Raw Number --> Shulker Boxes"]
                    , input_ [type_ "text", placeholder_ "Number or expression", onInput RawNumberToShulkerBoxesInputUpdate]
                    , div_ [class_ "spacer"] []
                    , span_ []
                        [ text "items = "
                        , text (ms shulkerBoxes)
                        , text " Shulker Boxes + "
                        , text (ms stacks)
                        , text " Stacks + "
                        , text (ms remains)
                        ]
                    ]
