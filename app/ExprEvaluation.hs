module ExprEvaluation (evaluateExpr) where

import           Data.Either.Extra     (eitherToMaybe)
import           Miso.String           (MisoString, fromMisoString)
import           Text.Parsec.Expr.Math (evaluate, parse)

evaluateExpr :: MisoString -> Maybe Int
evaluateExpr ""  = Just 0
evaluateExpr str =
    case evaluate mempty (eitherToMaybe $ parse (fromMisoString str)) :: Maybe Double of
        Just value -> Just (round value)
        Nothing    -> Nothing
