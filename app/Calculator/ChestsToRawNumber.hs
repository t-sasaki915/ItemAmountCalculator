module Calculator.ChestsToRawNumber (ChestsToRawNumber (..)) where

import           Miso
import           Miso.String (ms)

import           Action      (Action (..))
import           Calculator  (CalculationContext (..), Calculator (..))
import           Model       hiding (stackUnit)

data ChestsToRawNumber = ChestsToRawNumber

instance Calculator ChestsToRawNumber where
    inputLenses ChestsToRawNumber =
        [ chestsToRawNumberLargeChestInput
        , chestsToRawNumberChestInput
        , chestsToRawNumberStackInput
        , chestsToRawNumberRemainInput
        ]

    resultLenses ChestsToRawNumber =
        [ chestsToRawNumberRawNumber
        ]

    calculatePure ChestsToRawNumber ctx inputs =
        let largeChests = inputs !! 0
            chests = inputs !! 1
            stacks = inputs !! 2
            remains = inputs !! 3
            rawNumber = (largeChests * (2 * 27 * stackUnit ctx)) + (chests * (27 * stackUnit ctx)) + (stacks * stackUnit ctx) + remains in
                [rawNumber]

    viewCalculatorPure ChestsToRawNumber results =
        let rawNumber = results !! 0 in
                div_ []
                    [ h2_ [] [text "Chests --> Raw Number"]
                    , span_ []
                        [ input_ [type_ "text", placeholder_ "Number or expression", onInput ChestsToRawNumberLargeChestInputUpdate]
                        , text " Large Chests + "
                        , input_ [type_ "text", placeholder_ "Number or expression", onInput ChestsToRawNumberChestInputUpdate]
                        , text " Chests + "
                        , input_ [type_ "text", placeholder_ "Number or expression", onInput ChestsToRawNumberStackInputUpdate]
                        , text " Stacks + "
                        , input_ [type_ "text", placeholder_ "Number or expression", onInput ChestsToRawNumberRemainInputUpdate]
                        ]
                    , div_ [class_ "spacer"] []
                    , span_ []
                        [ text "= "
                        , text (ms rawNumber)
                        , text " items"
                        ]
                    ]

