{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import           Control.Lens          (makeLenses, set, (^.))
import           Data.Either.Extra     (eitherToMaybe)
import           Miso                  hiding (set)
import           Miso.String           (MisoString, fromMisoString, ms)
import           Text.Parsec.Expr.Math (evaluate, parse)

data Model = Model
    { _rawNumberToShulkerBoxesShulkerBoxes :: Int
    , _rawNumberToShulkerBoxesStacks       :: Int
    , _rawNumberToShulkerBoxesRemains      :: Int
    } deriving (Show, Eq)

newtype Action = RawNumberToShulkerBoxesInputUpdate MisoString
               deriving (Show, Eq)

makeLenses ''Model

initModel :: Model
initModel = Model
    { _rawNumberToShulkerBoxesShulkerBoxes = 0
    , _rawNumberToShulkerBoxesStacks       = 0
    , _rawNumberToShulkerBoxesRemains      = 0
    }

updateModel :: Action -> Effect Model Action
updateModel (RawNumberToShulkerBoxesInputUpdate rawExpr) =
    case evaluateExpr rawExpr of
        Just value ->
            let (shulkerBoxes, remain1) = value `divMod` (27 * 64)
                (stacks, remain2) = remain1 `divMod` 64 in
                    submitResult shulkerBoxes stacks remain2
        Nothing ->
            submitResult 0 0 0
    where
        submitResult :: Int -> Int -> Int -> Effect Model Action
        submitResult shulkerBoxes stacks remains = get >>= put .
            set rawNumberToShulkerBoxesShulkerBoxes shulkerBoxes .
                set rawNumberToShulkerBoxesStacks stacks .
                    set rawNumberToShulkerBoxesRemains remains

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
        , text "SB + "
        , text (ms $ mdl ^. rawNumberToShulkerBoxesStacks)
        , text "st + "
        , text (ms $ mdl ^. rawNumberToShulkerBoxesRemains)
        , span_ [] []
        ]

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
