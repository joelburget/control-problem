module Gridworld.Types where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Random
import Data.Generic
import Data.Maybe
import Gridworld.MatrixUtil
import Data.TypeNat (class Sized, sized)

foreign import data Agent :: *

data Five
data Seven

instance sf :: Sized Five where
  sized _ = 5

instance ss :: Sized Seven where
  sized _ = 7

-- game state

data PlayState = Play | Pause

instance heytingPlayState :: HeytingAlgebra PlayState where
  ff = Pause
  tt = Play

  not Play = Pause
  not Pause = Play

  disj Play _ = Play
  disj _ Play = Play
  disj _ _ = Pause

  conj Play Play = Play
  conj _ _ = Pause

  implies a b = not a || b

derive instance genericPlayState :: Generic PlayState
instance eqPlayState :: Eq PlayState where eq = gEq

-- TODO rename from Character, which is misleading
data Character
  = Block
  | Robot
  | Camera
  | Person1
  | Person2

  | Empty
  | Hole
  | Gaze

derive instance genericCharacter :: Generic Character
instance eqCharacter :: Eq Character where eq = gEq

-- | The playing field
--
-- All mutating operations need to keep `botPosition` and `values` in sync.
type Field =
  { botPosition :: Position
  , values :: Mat Five Seven Character
  }

serializeField :: Field -> Array Int
serializeField {values} =
  let cToI char = case char of
        Empty -> 0
        Block -> 1
        Robot -> 2
        Camera -> 4
        Person1 -> 8
        Person2 -> 16

        -- we never serialize `Hole` or `Gaze` -- they're for display only
        -- (they should never end up here, they're only ever instantiated *in*
        -- the view function
        _ -> -1
  in toArray $ map cToI values


-- models

type RewardResult =
  { field :: Field
  , terminate :: Terminate
  , reward :: Reward
  }

type GameModel =
  { name :: String
  , description :: String
  , moveBot :: Field -> Direction -> Maybe Field
  , checkReward :: Field -> Boolean -> Eff (random :: RANDOM) RewardResult
  , initField :: Field
  }

-- checkReward: field, alreadyRewarded
-- updateAfterAction: field, iteration, epsilon, stepsSinceReset, gamesPlayed, timesRewarded
type SimulationState =
  { gameModel :: GameModel
  , agent :: Maybe Agent
  , field :: Field
  , alreadyRewarded :: Boolean
  , stepsSinceReset :: Int
  , playState :: PlayState
  , iteration :: Int
  , epsilon :: Number

  , gamesPlayed :: Int
  , timesRewarded :: Int

  , averageRewards :: Array Number

  -- TODO also consider adding these as adjustable params (not updated as part
  -- of the algorithm):

  -- num_hidden_units: 200,
  -- experience_add_every: 2,
  -- learning_steps_per_iteration: 10,
  -- experience_size: 20000,
  -- alpha: 0.01,
  -- gamma: 0.99
  }


-- algorithm feedback

data Reward = Reward | NoReward
data Terminate = Terminate | Continue

derive instance genericReward :: Generic Reward
instance eqReward :: Eq Reward where eq = gEq
-- instance showReward :: Show Reward where show = gShow

derive instance genericTerminate :: Generic Terminate
instance eqTerminate :: Eq Terminate where eq = gEq
-- instance showTerminate :: Show Terminate where show = gShow


-- position manipulation

data Direction = North | South | East | West

unserializeDirection :: Int -> Direction
unserializeDirection 0 = North
unserializeDirection 1 = South
unserializeDirection 2 = West
unserializeDirection _ = East

type Position = {x :: Int, y :: Int}
type Vector = {dx :: Int, dy :: Int}

getPos :: Field -> Position -> Maybe Character
getPos {values} {x, y} = getElem x y values

setPos :: Position -> Character -> Field -> Field
setPos {x, y} c field =
  let values = setElem x y c field.values
  in field { values = values }

addPos :: Position -> Vector -> Position
addPos {x, y} {dx, dy} = {x: x + dx, y: y + dy}

subPos :: Position -> Vector -> Position
subPos {x, y} {dx, dy} = {x: x - dx, y: y - dy}

scaleVec :: Vector -> Int -> Vector
scaleVec {dx, dy} c = {dx: c * dx, dy: c * dy}

dirDelta :: Direction -> Vector
dirDelta dir = case dir of
  North -> {dx: 0, dy: -1}
  South -> {dx: 0, dy: 1}
  East -> {dx: 1, dy: 0}
  West -> {dx: -1, dy: 0}


-- random number generation / rewarding

rewardFailureRate :: Number
rewardFailureRate = 0.2


-- ports

-- port agentLearn :: Boolean -> Cmd msg
-- port agentAct :: (Array Int, Boolean) -> Cmd msg
-- port agentMoveBot :: (Int -> msg) -> Sub msg
