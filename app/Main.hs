module Main (main) where

import           Control.Lens                       (set, (^.))
import           Data.Functor                       ((<&>))
import           Miso                               hiding (set)
import           Miso.String                        (fromMisoString)
import           Miso.Style                         (styleInline_)
import           Text.Read                          (readMaybe)

import           Action                             (Action (..))
import           Calculator                         (Calculator (..))
import           Calculator.ChestsToRawNumber       (ChestsToRawNumber (..))
import           Calculator.RawNumberToChests       (RawNumberToChests (..))
import           Calculator.RawNumberToShulkerBoxes (RawNumberToShulkerBoxes (..))
import           Calculator.ShulkerBoxesToRawNumber (ShulkerBoxesToRawNumber (..))
import           Model

updateModel :: Action -> Effect Model Action
updateModel (RawNumberToShulkerBoxesInputUpdate newInput) =
    get >>= put . set rawNumberToShulkerBoxesInput newInput >>
        issue CalculateRawNumberToShulkerBoxes

updateModel (RawNumberToChestsInputUpdate newInput) =
    get >>= put . set rawNumberToChestsInput newInput >>
        issue CalculateRawNumberToChests

updateModel (ShulkerBoxesToRawNumberShulkerBoxInputUpdate newInput) =
    get >>= put . set shulkerBoxesToRawNumberShulkerBoxInput newInput >>
        issue CalculateShulkerBoxesToRawNumber
updateModel (ShulkerBoxesToRawNumberStackInputUpdate newInput) =
    get >>= put . set shulkerBoxesToRawNumberStackInput newInput >>
        issue CalculateShulkerBoxesToRawNumber
updateModel (ShulkerBoxesToRawNumberRemainInputUpdate newInput) =
    get >>= put . set shulkerBoxesToRawNumberRemainInput newInput >>
        issue CalculateShulkerBoxesToRawNumber

updateModel (ChestsToRawNumberLargeChestInputUpdate newInput) =
    get >>= put . set chestsToRawNumberLargeChestInput newInput >>
        issue CalculateChestsToRawNumber
updateModel (ChestsToRawNumberChestInputUpdate newInput) =
    get >>= put . set chestsToRawNumberChestInput newInput >>
        issue CalculateChestsToRawNumber
updateModel (ChestsToRawNumberStackInputUpdate newInput) =
    get >>= put . set chestsToRawNumberStackInput newInput >>
        issue CalculateChestsToRawNumber
updateModel (ChestsToRawNumberRemainInputUpdate newInput) =
    get >>= put . set chestsToRawNumberRemainInput newInput >>
        issue CalculateChestsToRawNumber

updateModel CalculateRawNumberToShulkerBoxes = calculate RawNumberToShulkerBoxes
updateModel CalculateRawNumberToChests       = calculate RawNumberToChests
updateModel CalculateShulkerBoxesToRawNumber = calculate ShulkerBoxesToRawNumber
updateModel CalculateChestsToRawNumber       = calculate ChestsToRawNumber

updateModel (StackUnitInputUpdate newInput) =
    get >>= put . set stackUnitInput newInput

updateModel UpdateStackUnit = do
    input <- get <&> (^. stackUnitInput)

    if input == mempty
        then get >>= put . set stackUnit 64
        else
            case readMaybe (fromMisoString input) of
                Just newStackUnit ->
                    if newStackUnit > 0
                        then get >>= put . set stackUnit newStackUnit
                        else io_ $ alert "This must be more than 0."

                Nothing -> io_ $ alert "Unrecognisable number."

    recalculateAll

recalculateAll :: Effect Model Action
recalculateAll = mapM_ issue
    [ CalculateRawNumberToShulkerBoxes
    , CalculateRawNumberToChests
    , CalculateShulkerBoxesToRawNumber
    , CalculateChestsToRawNumber
    ]

viewModel :: Model -> View Action
viewModel mdl = div_ []
    [ h1_ [] [text "Minecraft ItemAmountCalculator"]

    , viewCalculator RawNumberToShulkerBoxes mdl
    , viewCalculator RawNumberToChests mdl
    , viewCalculator ShulkerBoxesToRawNumber mdl
    , viewCalculator ChestsToRawNumber mdl

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
