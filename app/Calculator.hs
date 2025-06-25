{-# LANGUAGE ImpredicativeTypes #-}

module Calculator
    ( Calculator (..)
    , CalculationContext (..)
    ) where

import           Control.Lens           (Lens', set, (^.))
import           Control.Monad          (forM, forM_)
import           Control.Monad.Except   (ExceptT, runExceptT, throwError)
import           Control.Monad.RWS.Lazy (RWST)
import           Miso                   (Effect, View, get, put)
import           Miso.String            (MisoString)

import           Action                 (Action)
import           ExprEvaluation         (evaluateExpr)
import           Model                  (Model)
import qualified Model

newtype CalculationContext = CalculationContext
    { stackUnit :: Int
    }

class Calculator a where
    inputLenses :: a -> [Lens' Model MisoString]

    resultLenses :: a -> [Lens' Model Int]

    calculatePure :: a -> CalculationContext -> [Int] -> [Int]

    viewCalculatorPure :: a -> [Int] -> View Action

    calculate :: a -> Effect Model Action
    calculate a = do
        runExceptT evaluateInputs >>= \case
            Right inputs ->
                calculationContext >>= \ctx ->
                    let calculationResults = calculatePure a ctx inputs in
                        forM_ (zip (resultLenses a) calculationResults) $ \(resultLens, result) ->
                            get >>= put . set resultLens result

            Left () ->
                forM_ (resultLenses a) $ \resultLens ->
                    get >>= put . set resultLens 0
        where
            calculationContext :: (Monoid w, Monad m) => RWST r w Model m CalculationContext
            calculationContext = get >>= \state -> pure $
                CalculationContext
                    { stackUnit = state ^. Model.stackUnit
                    }

            evaluateInputs :: (Monoid w, Monad m) => ExceptT () (RWST r w Model m) [Int]
            evaluateInputs = get >>= \state ->
                forM (inputLenses a) $ \inputLens ->
                    case evaluateExpr (state ^. inputLens) of
                        Just value -> pure value
                        Nothing    -> throwError ()

    {- HLINT ignore "Avoid lambda" -}
    viewCalculator :: a -> Model -> View Action
    viewCalculator a mdl =
        let results = map (\resultLens -> mdl ^. resultLens) (resultLenses a) in
            viewCalculatorPure a results
