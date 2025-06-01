{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import           Control.Lens          (makeLenses, set, (^.))
import           Data.Either.Extra     (eitherToMaybe)
import           Data.Functor          ((<&>))
import           Miso                  hiding (set)
import           Miso.String           (MisoString, fromMisoString, ms)
import           Miso.Style            (styleInline_)
import           Text.Parsec.Expr.Math (evaluate, parse)
import           Text.Read             (readMaybe)

data Model = Model
    { _rawNumberToShulkerBoxesInput        :: MisoString
    , _rawNumberToShulkerBoxesShulkerBoxes :: Int
    , _rawNumberToShulkerBoxesStacks       :: Int
    , _rawNumberToShulkerBoxesRemains      :: Int

    , _rawNumberToChestsInput              :: MisoString
    , _rawNumberToChestsLargeChests        :: Int
    , _rawNumberToChestsChests             :: Int
    , _rawNumberToChestsStacks             :: Int
    , _rawNumberToChestsRemains            :: Int

    , _stackUnitInput                      :: MisoString
    , _stackUnit                           :: Int
    } deriving (Show, Eq)

data Action = RawNumberToShulkerBoxesInputUpdate MisoString
            | CalculateRawNumberToShulkerBoxes

            | RawNumberToChestsInputUpdate MisoString
            | CalculateRawNumberToChests

            | StackUnitInputUpdate MisoString
            | UpdateStackUnit
            deriving (Show, Eq)

makeLenses ''Model

initModel :: Model
initModel = Model
    { _rawNumberToShulkerBoxesInput        = mempty
    , _rawNumberToShulkerBoxesShulkerBoxes = 0
    , _rawNumberToShulkerBoxesStacks       = 0
    , _rawNumberToShulkerBoxesRemains      = 0

    , _rawNumberToChestsInput              = mempty
    , _rawNumberToChestsLargeChests        = 0
    , _rawNumberToChestsChests             = 0
    , _rawNumberToChestsStacks             = 0
    , _rawNumberToChestsRemains            = 0

    , _stackUnitInput                      = mempty
    , _stackUnit                           = 64
    }

updateModel :: Action -> Effect Model Action
updateModel (RawNumberToShulkerBoxesInputUpdate newInput) =
    get >>= put . set rawNumberToShulkerBoxesInput newInput >>
        issue CalculateRawNumberToShulkerBoxes

updateModel CalculateRawNumberToShulkerBoxes = do
    input <- get <&> (^. rawNumberToShulkerBoxesInput)

    oneStack <- get <&> (^. stackUnit)

    case evaluateExpr input of
        Just value ->
            let (shulkerBoxes, remain1) = value `divMod` (27 * oneStack)
                (stacks, remain2) = remain1 `divMod` oneStack in
                    submitResult shulkerBoxes stacks remain2
        Nothing ->
            submitResult 0 0 0
    where
        submitResult :: Int -> Int -> Int -> Effect Model Action
        submitResult shulkerBoxes stacks remains = get >>= put .
            set rawNumberToShulkerBoxesShulkerBoxes shulkerBoxes .
                set rawNumberToShulkerBoxesStacks stacks .
                    set rawNumberToShulkerBoxesRemains remains

updateModel (RawNumberToChestsInputUpdate newInput) =
    get >>= put . set rawNumberToChestsInput newInput >>
        issue CalculateRawNumberToChests

updateModel CalculateRawNumberToChests = do
    input <- get <&> (^. rawNumberToChestsInput)

    oneStack <- get <&> (^. stackUnit)

    case evaluateExpr input of
        Just value ->
            let (largeChests, remain1) = value `divMod` (2 * 27 * oneStack)
                (chests, remain2) = remain1 `divMod` (27 * oneStack)
                (stacks, remain3) = remain2 `divMod` oneStack in
                    submitResult largeChests chests stacks remain3
        Nothing ->
            submitResult 0 0 0 0
    where
        submitResult :: Int -> Int -> Int -> Int -> Effect Model Action
        submitResult largeChests chests stacks remains = get >>= put .
            set rawNumberToChestsLargeChests largeChests .
                set rawNumberToChestsChests chests .
                    set rawNumberToChestsStacks stacks .
                        set rawNumberToChestsRemains remains

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

evaluateExpr :: MisoString -> Maybe Int
evaluateExpr str =
    case evaluate mempty (eitherToMaybe $ parse (fromMisoString str)) :: Maybe Double of
        Just value -> Just (round value)
        Nothing    -> Nothing

viewModel :: Model -> View Action
viewModel mdl = div_ []
    [ h1_ [] [text "Minecraft ItemAmountCalculator"]

    , h2_ [] [text "Raw Number --> Shulker Boxes"]
    , input_ [type_ "text", placeholder_ "Number or expression", onInput RawNumberToShulkerBoxesInputUpdate]
    , div_ [class_ "spacer"] []
    , span_ []
        [ text "items = "
        , text (ms $ mdl ^. rawNumberToShulkerBoxesShulkerBoxes)
        , text " Shulker Boxes + "
        , text (ms $ mdl ^. rawNumberToShulkerBoxesStacks)
        , text " Stacks + "
        , text (ms $ mdl ^. rawNumberToShulkerBoxesRemains)
        ]

    , h2_ [] [text "Raw Number --> Chests"]
    , input_ [type_ "text", placeholder_ "Number or expression", onInput RawNumberToChestsInputUpdate]
    , div_ [class_ "spacer"] []
    , span_ []
        [ text "items = "
        , text (ms $ mdl ^. rawNumberToChestsLargeChests)
        , text " Large Chests + "
        , text (ms $ mdl ^. rawNumberToChestsChests)
        , text " Chests + "
        , text (ms $ mdl ^. rawNumberToChestsStacks)
        , text " Stacks + "
        , text (ms $ mdl ^. rawNumberToChestsRemains)
        ]

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
main = run (startComponent $ defaultComponent initModel updateModel viewModel)
