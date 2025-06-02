{-# LANGUAGE TemplateHaskell #-}

module Model
    ( Model (..)
    , rawNumberToShulkerBoxesInput
    , rawNumberToShulkerBoxesShulkerBoxes
    , rawNumberToShulkerBoxesStacks
    , rawNumberToShulkerBoxesRemains
    , rawNumberToChestsInput
    , rawNumberToChestsLargeChests
    , rawNumberToChestsChests
    , rawNumberToChestsStacks
    , rawNumberToChestsRemains
    , shulkerBoxesToRawNumberShulkerBoxInput
    , shulkerBoxesToRawNumberStackInput
    , shulkerBoxesToRawNumberRemainInput
    , shulkerBoxesToRawNumberRawNumber
    , chestsToRawNumberLargeChestInput
    , chestsToRawNumberChestInput
    , chestsToRawNumberStackInput
    , chestsToRawNumberRemainInput
    , chestsToRawNumberRawNumber
    , stackUnitInput
    , stackUnit
    , initialModel
    ) where

import           Control.Lens (makeLenses)
import           Miso.String  (MisoString)

data Model = Model
    { _rawNumberToShulkerBoxesInput           :: MisoString
    , _rawNumberToShulkerBoxesShulkerBoxes    :: Int
    , _rawNumberToShulkerBoxesStacks          :: Int
    , _rawNumberToShulkerBoxesRemains         :: Int

    , _rawNumberToChestsInput                 :: MisoString
    , _rawNumberToChestsLargeChests           :: Int
    , _rawNumberToChestsChests                :: Int
    , _rawNumberToChestsStacks                :: Int
    , _rawNumberToChestsRemains               :: Int

    , _shulkerBoxesToRawNumberShulkerBoxInput :: MisoString
    , _shulkerBoxesToRawNumberStackInput      :: MisoString
    , _shulkerBoxesToRawNumberRemainInput     :: MisoString
    , _shulkerBoxesToRawNumberRawNumber       :: Int

    , _chestsToRawNumberLargeChestInput       :: MisoString
    , _chestsToRawNumberChestInput            :: MisoString
    , _chestsToRawNumberStackInput            :: MisoString
    , _chestsToRawNumberRemainInput           :: MisoString
    , _chestsToRawNumberRawNumber             :: Int

    , _stackUnitInput                         :: MisoString
    , _stackUnit                              :: Int
    } deriving (Show, Eq)

makeLenses ''Model

initialModel :: Model
initialModel = Model
    { _rawNumberToShulkerBoxesInput           = mempty
    , _rawNumberToShulkerBoxesShulkerBoxes    = 0
    , _rawNumberToShulkerBoxesStacks          = 0
    , _rawNumberToShulkerBoxesRemains         = 0

    , _rawNumberToChestsInput                 = mempty
    , _rawNumberToChestsLargeChests           = 0
    , _rawNumberToChestsChests                = 0
    , _rawNumberToChestsStacks                = 0
    , _rawNumberToChestsRemains               = 0

    , _shulkerBoxesToRawNumberShulkerBoxInput = mempty
    , _shulkerBoxesToRawNumberStackInput      = mempty
    , _shulkerBoxesToRawNumberRemainInput     = mempty
    , _shulkerBoxesToRawNumberRawNumber       = 0

    , _chestsToRawNumberLargeChestInput       = mempty
    , _chestsToRawNumberChestInput            = mempty
    , _chestsToRawNumberStackInput            = mempty
    , _chestsToRawNumberRemainInput           = mempty
    , _chestsToRawNumberRawNumber             = 0

    , _stackUnitInput                         = mempty
    , _stackUnit                              = 64
    }
