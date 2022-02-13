module AnimationState where

type Vector = (Int, Int)

data Status = Status { position :: Vector, direction :: Vector }
  deriving (Show)

data Env = Env { frame :: Vector }
  deriving (Show)