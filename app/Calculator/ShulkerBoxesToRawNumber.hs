module Calculator.ShulkerBoxesToRawNumber (ShulkerBoxesToRawNumber (..)) where

import           Miso
import           Miso.String (ms)

import           Action      (Action (..))
import           Calculator  (CalculationContext (..), Calculator (..))
import           Model       hiding (stackUnit)

data ShulkerBoxesToRawNumber = ShulkerBoxesToRawNumber

instance Calculator ShulkerBoxesToRawNumber where
    inputLenses ShulkerBoxesToRawNumber =
        [ shulkerBoxesToRawNumberShulkerBoxInput
        , shulkerBoxesToRawNumberStackInput
        , shulkerBoxesToRawNumberRemainInput
        ]

    resultLenses ShulkerBoxesToRawNumber =
        [ shulkerBoxesToRawNumberRawNumber
        ]

    calculatePure ShulkerBoxesToRawNumber ctx inputs =
        let shulkerBoxes = inputs !! 0
            stacks = inputs !! 1
            remains = inputs !! 2
            rawNumber = (shulkerBoxes * (27 * stackUnit ctx)) + (stacks * stackUnit ctx) + remains in
                [rawNumber]

    viewCalculatorPure ShulkerBoxesToRawNumber results =
        let rawNumber = results !! 0 in
                div_ []
                    [ h2_ [] [text "Shulker Boxes --> Raw Number"]
                    , span_ []
                        [ input_ [type_ "text", placeholder_ "Number or expression", onInput ShulkerBoxesToRawNumberShulkerBoxInputUpdate]
                        , text " Shulker Boxes + "
                        , input_ [type_ "text", placeholder_ "Number or expression", onInput ShulkerBoxesToRawNumberStackInputUpdate]
                        , text " Stacks + "
                        , input_ [type_ "text", placeholder_ "Number or expression", onInput ShulkerBoxesToRawNumberRemainInputUpdate]
                        ]
                    , div_ [class_ "spacer"] []
                    , span_ []
                        [ text "= "
                        , text (ms rawNumber)
                        , text " items"
                        ]
                    ]

