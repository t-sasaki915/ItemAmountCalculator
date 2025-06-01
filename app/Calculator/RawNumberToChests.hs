module Calculator.RawNumberToChests (RawNumberToChests (..)) where

import           Miso
import           Miso.String (ms)

import           Action      (Action (RawNumberToChestsInputUpdate))
import           Calculator  (CalculationContext (..), Calculator (..))
import           Model       hiding (stackUnit)

data RawNumberToChests = RawNumberToChests

instance Calculator RawNumberToChests where
    inputLenses RawNumberToChests =
        [ rawNumberToChestsInput
        ]

    resultLenses RawNumberToChests =
        [ rawNumberToChestsLargeChests
        , rawNumberToChestsChests
        , rawNumberToChestsStacks
        , rawNumberToChestsRemains
        ]

    calculatePure RawNumberToChests ctx inputs =
        let rawNumber = inputs !! 0
            (largeChests, remain1) = rawNumber `divMod` (2 * 27 * stackUnit ctx)
            (chests, remain2) = remain1 `divMod` (27 * stackUnit ctx)
            (stacks, remain3) = remain2 `divMod` stackUnit ctx in
                [largeChests, chests, stacks, remain3]

    viewCalculatorPure RawNumberToChests results =
        let largeChests = results !! 0
            chests = results !! 1
            stacks = results !! 2
            remains = results !! 3 in div_ []
                [ h2_ [] [text "Raw Number --> Chests"]
                , input_ [type_ "text", placeholder_ "Number or expression", onInput RawNumberToChestsInputUpdate]
                , div_ [class_ "spacer"] []
                , span_ []
                    [ text "items = "
                    , text (ms largeChests)
                    , text " Large Chests + "
                    , text (ms chests)
                    , text " Chests + "
                    , text (ms stacks)
                    , text " Stacks + "
                    , text (ms remains)
                    ]
                ]
