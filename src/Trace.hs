module Trace where

import Control.Lens as Lens

data Trace s a = Trace
  { _path :: ![s]
  -- ^ States passed during execution
  , _transitions :: ![a]
  -- ^ Transitions during execution
  , _accepted :: !Bool
  -- ^ Whether the input was accepted or not
  } deriving (Show)

makeLenses ''Trace

nil :: Trace s a
nil =
  Trace {_path = [], _transitions = [], _accepted = False}

state :: Trace s a -> s
state = head . (^. path)
