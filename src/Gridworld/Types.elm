port module Gridworld.Types exposing (..)

import Array exposing (Array)
import Matrix exposing (Matrix, get, map)
import Maybe exposing (andThen, withDefault)
import Random


-- game state

type PlayState = Play | Pause

-- TODO rename from Character, which is misleading
type Character
  = Block
  | Robot
  | Camera
  | Person

  | Empty
  | Hole
  | Gaze

-- | The playing field
--
-- All mutating operations need to keep `botPosition` and `values` in sync.
type alias Field =
  { botPosition : Position
  , values : Maybe (Matrix Character)
  }

serializeField : Field -> Array Int
serializeField {values} =
  let cToI char = case char of
        Empty -> 0
        Block -> 1
        Robot -> 2
        Camera -> 4
        Person -> 8

        -- we never serialize `Hole` or `Gaze` -- they're for display only
        -- (they should never end up here, they're only ever instantiated *in*
        -- the view function
        _ -> -1
  in case values of
       Just {data} -> Array.map cToI data
       Nothing -> Array.empty


-- models

type alias GameModel =
  { name : String
  , description : String
  , moveBot : Field -> Direction -> Maybe Field
  , checkReward : Field -> Bool -> Cmd (Terminate, Reward)
  , initField : Field
  }

-- checkReward: field, alreadyRewarded
-- updateAfterAction: field, iteration, epsilon, stepsSinceReset, gamesPlayed, timesRewarded
type alias SimulationState =
  { gameModel : GameModel
  , field : Field
  , alreadyRewarded : Bool
  , stepsSinceReset : Int
  , playState : PlayState
  , iteration : Int
  , epsilon : Float

  , gamesPlayed : Int
  , timesRewarded : Int

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

type Reward = Reward | NoReward
type Terminate = Terminate | Continue


-- position manipulation

type Direction = North | South | East | West
type alias Position = (Int, Int)
type alias Vector = (Int, Int)

getPos : Field -> Position -> Maybe Character
getPos {values} (x, y) = values `andThen` get x y

setPos : Position -> Character -> Field -> Field
setPos (x, y) c field =
  let values = Maybe.map (Matrix.set x y c) field.values
  in { field | values = values }

addPos : Position -> Vector -> Position
addPos (x, y) (dx, dy) = (x + dx, y + dy)

subPos : Position -> Vector -> Position
subPos (x, y) (dx, dy) = (x - dx, y - dy)

dirDelta : Direction -> Vector
dirDelta dir = case dir of
  North -> (0, -1)
  South -> (0, 1)
  East -> (1, 0)
  West -> (-1, 0)


-- random number generation / rewarding

-- XXX this should not go here
floatGenerator : Random.Generator Float
floatGenerator = Random.float 0 1

rewardFailureRate : Float
rewardFailureRate = 0.8


-- ports

port agentLearn : Bool -> Cmd msg
port agentAct : (Array Int, Bool) -> Cmd msg
port agentMoveBot : (Int -> msg) -> Sub msg
