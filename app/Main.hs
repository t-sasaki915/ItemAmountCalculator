module Main (main) where

import           Control.Lens                       (set, (^.))
import           Data.Functor                       ((<&>))
import           Miso                               hiding (set)
import           Miso.String                        (fromMisoString)
import           Miso.Style                         (styleInline_)
import           Text.Read                          (readMaybe)

import           Action                             (Action (..))
import           Calculator                         (Calculator (..))
import           Calculator.RawNumberToChests       (RawNumberToChests (..))
import           Calculator.RawNumberToShulkerBoxes (RawNumberToShulkerBoxes (..))
import           Model

updateModel :: Action -> Effect Model Action
updateModel (RawNumberToShulkerBoxesInputUpdate newInput) =
    get >>= put . set rawNumberToShulkerBoxesInput newInput >>
        issue CalculateRawNumberToShulkerBoxes
updateModel (RawNumberToChestsInputUpdate newInput) =
    get >>= put . set rawNumberToChestsInput newInput >>
        issue CalculateRawNumberToChests

updateModel CalculateRawNumberToShulkerBoxes = calculate RawNumberToShulkerBoxes
updateModel CalculateRawNumberToChests       = calculate RawNumberToChests

updateModel (StackUnitInputUpdate newInput) =
    get >>= put . set stackUnitInput newInput

updateModel UpdateStackUnit = do
    input <- get <&> (^. stackUnitInput)

    if input == mempty
        then get >>= put . set stackUnit 64
        else
            case readMaybe (fromMisoString input) of
                Just newStackUnit -> get >>= put . set stackUnit newStackUnit
                Nothing           -> io_ $ alert "Unrecognisable number"

    recalculateAll

recalculateAll :: Effect Model Action
recalculateAll = mapM_ issue
    [ CalculateRawNumberToShulkerBoxes
    , CalculateRawNumberToChests
    ]

viewModel :: Model -> View Action
viewModel mdl = div_ []
    [ h1_ [] [text "Minecraft ItemAmountCalculator"]

    , viewCalculator RawNumberToShulkerBoxes mdl
    , viewCalculator RawNumberToChests mdl

    , h2_ [] [text "Settings"]

    , span_ [] [text "1 Stack = "]
    , input_ [type_ "text", placeholder_ "64", styleInline_ "width: 2em;", onInput StackUnitInputUpdate]
    , span_ [] [text " Items"]
    , div_ [class_ "spacer"] []
    , button_ [onClick UpdateStackUnit] [text "Apply"]

    , footer_ []
        [ span_ [] [text "Author: ", a_ [href_ "https://github.com/t-sasaki915"] [text "Toma Sasaki"]]
        , br_ []
        , span_ [] [text "Source: ", a_ [href_ "https://github.com/t-sasaki915/ItemAmountCalculator"] [text "t-sasaki915/ItemAmountCalculator"]]
        , br_ []
        , span_ [] [text "Developed with Haskell and ", a_ [href_ "https://haskell-miso.org/"] [text "miso"], text "."]
        ]
    ]

main :: IO ()
main = run (startComponent $ defaultComponent initialModel updateModel viewModel)
