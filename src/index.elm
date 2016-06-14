port module Gridworld exposing (..)

import Array exposing (Array)
import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Matrix exposing (Matrix, get, map)
import Maybe exposing (andThen, withDefault)
import Random
import String


-- model

type PlayState = Play | Pause

type alias Model =
  { field : Field
  , alreadyRewarded : Bool
  , stepsSinceReset : Int
  , playState : PlayState
  , iteration : Int
  , epsilon : Float

  -- TODO also consider adding these as adjustable params (not updated as part
  -- of the algorithm):

  -- num_hidden_units: 200,
  -- experience_add_every: 2,
  -- learning_steps_per_iteration: 10,
  -- experience_size: 20000,
  -- alpha: 0.01,
  -- gamma: 0.99
  }

init : (Model, Cmd Msg)
init = (
  { field = initialField
  , alreadyRewarded = False
  , stepsSinceReset = 0
  , playState = Pause
  -- XXX these are duplicated / not used
  , epsilon = 1.0
  , iteration = 0
  },
  Cmd.none
  )


type Msg
  -- messages for agent
  = AgentLearn Bool

  -- messages from agent
  | AgentActResponse Int

  -- other?
  | PlayPause
  | UseReward Terminate Reward


-- TODO rename from Character, which is misleading
type Character
  = Block
  | Robot
  | Camera
  | Empty


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
  in case values of
       Just {data} -> Array.map cToI data
       Nothing -> Array.empty

initialField : Field
initialField =
  let e = Empty
      b = Block
      r = Robot
      c = Camera
  in { botPosition = (6, 0)
     , values = Matrix.fromList
       [ [ e,e,e,e,e,e,r ]
       , [ e,e,e,b,e,e,b ]
       , [ e,b,e,e,b,e,e ]
       , [ e,e,b,e,e,b,e ]
       , [ c,e,e,e,e,e,e ]
       ]
     }

type Reward = Reward | NoReward
type Terminate = Terminate | Continue

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


validMove : Field -> Position -> Direction -> Bool
validMove field pos dir =
  let target = pos `addPos` dirDelta dir
      thingInDirection = getPos field target
  in case thingInDirection of
       -- if we're pushing into a block, make sure that the move is
       -- transitively valid, ie the block can move in this direction
       Just Block -> validMove field target dir
       Just _ -> True
       Nothing -> False


-- | Push `char` into `pos` in the direction of `dir`.
--
-- Failure indicated with `Nothing` means we can't push in that direction
-- because we hit a wall.
push : Position -> Direction -> Character -> Field -> Maybe Field
push pos dir char field =
  let nextPos = pos `addPos` dirDelta dir
      occupyingChar = getPos field pos

      field' = case occupyingChar of
        Just Block -> push nextPos dir Block field
        Just Robot -> push nextPos dir Robot field
        -- overwrite an empty spot or camera
        Just _ -> Just field
        -- the matrix api gives back Nothing if we go outside the bounds
        Nothing -> Nothing

  -- Now that we've moved the occupying character out of the way (or not)
  -- overwrite that spot.
  in Maybe.map (setPos pos char) field'

moveBot : Field -> Direction -> Maybe Field
moveBot startField dir =
  -- transitively move all the boxes in this direction
  let botPosition = startField.botPosition
      delta = dirDelta dir
      -- push from *behind* the bot with an empty box
      newField = push botPosition dir Empty startField
      newBotPosition = botPosition `addPos` delta
  in case newField of
       Just field -> Just { field | botPosition = newBotPosition }
       Nothing -> Nothing

floatGenerator : Random.Generator Float
floatGenerator = Random.float 0 1

rewardFailureRate : Float
rewardFailureRate = 0.8

checkReward : Model -> Cmd Msg
checkReward model = flip Random.generate floatGenerator <| \rand ->
  let field = model.field
  -- look up location 6, 4
  in case field.values `andThen` get 6 4 of
    Just Block ->
      let field' = setPos (6, 4) Empty field
          reward =
            if model.alreadyRewarded == True
            then NoReward
            -- isn't this a bug in the original program? (checks
            -- Math.random() < rewardFailureRate, ie awarding 80% of the time
            -- instead of 20%)
            else if rand > rewardFailureRate then Reward else NoReward
          terminationCheck : Int -> Terminate -> Terminate
          terminationCheck i doTerminate =
            -- if vision is blocked, don't terminate
            if doTerminate /= Terminate || getPos field (i, 4) == Just Block
            then Continue
            else Terminate
          terminate = List.foldr terminationCheck Terminate [0..5]
      in UseReward terminate reward
    _ -> UseReward Continue NoReward


update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  let agentSerialized = (serializeField model.field, model.alreadyRewarded)
  in case action of

       AgentLearn reinforce ->
         ( model
         , if model.playState == Play then agentLearn reinforce else Cmd.none
         )

       AgentActResponse dir ->
         let dir' = case dir of
               0 -> North
               1 -> South
               2 -> West
               _ -> East
             field' = moveBot model.field dir'
             model' = case field' of
               Just field'' -> { model | field = field'' }
               Nothing -> model
         in (model', checkReward model')

       PlayPause ->
         let newPlayState = if model.playState == Play then Pause else Play
         in ( { model | playState = newPlayState }
            , if newPlayState == Play
              then agentAct agentSerialized
              else Cmd.none
            )

       UseReward terminate reward -> useReward model terminate reward

useReward : Model -> Terminate -> Reward -> (Model, Cmd Msg)
useReward model terminate reward =
    -- shrink epsilon/exploration rate every order of magnitude moves
    let model' = if model.iteration % 10 == 0
                 then { model | epsilon = model.epsilon / 2 }
                 else model
        -- reset if told to terminate or if we've gone 1000 steps
        model'' = if terminate == Terminate || model'.stepsSinceReset == 1000
                  then { model | field = initialField, stepsSinceReset = 0 }
                  else { model | stepsSinceReset = model.stepsSinceReset + 1 }
        agentSerialized = (serializeField model''.field, model''.alreadyRewarded)
        cmds = Cmd.batch
          [ agentLearn (if reward == Reward then True else False)
          , if model''.playState == Play then agentAct agentSerialized else Cmd.none
          ]
    in (model'', cmds)

port agentLearn : Bool -> Cmd msg
port agentAct : (Array Int, Bool) -> Cmd msg

port agentMoveBot : (Int -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions model = agentMoveBot AgentActResponse


-- view

view : Model -> Html Msg
view model =
  let buttonText = if model.playState == Play then "pause" else "play"
  in div []
       [ button [ onClick PlayPause ] [ text buttonText ]
       , environmentView model.field
       ]

environmentView : Field -> Html a
environmentView {values} =
  let data1 : Maybe (Array Character)
      data1 = Maybe.map .data values
      data2 : Maybe (List Character)
      data2 = Maybe.map Array.toList data1
      -- data3 : Maybe (VirtualDom.Node a)
      data3 = Maybe.map (List.map characterView) data2
      rows = div [ style [ ("display", "flex"), ("flex-flow", "row wrap"), ("width", "224px") ] ]
  in withDefault (text "error") (Maybe.map rows data3)

characterView : Character -> Html a
characterView character =
  let sty = style [ ("width", "32px"), ("height", "32px") ]
  in case character of
       Empty -> div [ sty ] []
       Block -> img [ sty, src "block.png" ] []
       Camera -> img [ sty, src "camera.png" ] []
       Robot -> img [ sty, src "robot.png" ] []


main = Html.program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }
