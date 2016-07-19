module Gridworld exposing (..)

import Array exposing (Array)
import Platform.Cmd as Cmd
import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Matrix exposing (Matrix, get, map)
import Maybe exposing (andThen, withDefault)
import Random
import String

import Gridworld.Types exposing (..)
import Gridworld.Programs.Original as Original
import Gridworld.Programs.Deceit as Deceit

dModel : GameModel
dModel = Deceit.model

type Msg
  -- messages from agent
  = AgentMoveBot Int

  -- other?
  | PlayPause
  | StepForward
  | SetModel GameModel
  | UpdateAfterAction Terminate Reward

initProgram : SimulationState
initProgram =
  { gameModel = dModel
  , field = dModel.initField
  , alreadyRewarded = False

  -- XXX these are duplicated / not used
  , epsilon = 1.0
  , iteration = 0
  , stepsSinceReset = 0
  , gamesPlayed = 0
  , playState = Pause
  , timesRewarded = 0
  }


-- hack to determine if this is an order of magnitude number (10, 100, 1000)
isMagnitude : Int -> Bool
isMagnitude i =
  let log = logBase 10 (toFloat i)
      rounded = toFloat (round log)
      epsilon = 0.000000001
  in abs (rounded - log) < epsilon

{-| The main update loop for the app. Respond to:
* user actions (PlayPause, StepForward)
* agent instructions (AgentMoveBot)
* next steps (UpdateAfterAction)
-}
update : Msg -> SimulationState -> (SimulationState, Cmd Msg)
update action model = case action of

  AgentMoveBot dir ->
    let dir' = case dir of
          0 -> North
          1 -> South
          2 -> West
          _ -> East
        field' = model.gameModel.moveBot model.field dir'
        model' = case field' of
          Just field'' -> { model | field = field'' }
          Nothing -> model
    in (model', Cmd.map (\(t, r) -> UpdateAfterAction t r) (model'.gameModel.checkReward model'.field model'.alreadyRewarded))

  PlayPause ->
    if model.playState == Play
    then ({ model | playState = Pause }, Cmd.none)
    else let model' = { model | playState = Play }
         in (model', agentActSerialized model')

  StepForward -> (model, agentActSerialized model)

  UpdateAfterAction terminate reward -> updateAfterAction model terminate reward

  SetModel gameModel -> ({ model | gameModel = gameModel }, Cmd.none)


{-| Tell the agent to act on this model
-}
agentActSerialized : SimulationState -> Cmd msg
agentActSerialized state =
  let agentSerialized = (serializeField state.field, state.alreadyRewarded)
  in agentAct agentSerialized


updateAfterAction
  : SimulationState
  -> Terminate
  -> Reward
  -> (SimulationState, Cmd msg)
updateAfterAction state terminate reward =
  -- shrink epsilon/exploration rate every order of magnitude moves
  let
      state' = if isMagnitude state.iteration
               then { state | epsilon = state.epsilon / 2 }
               else state

      -- reset if told to terminate or if we've gone 1000 steps
      state'' = if terminate == Terminate || state'.stepsSinceReset == 1000
                then { state'
                  | field = state.gameModel.initField
                  , stepsSinceReset = 0
                  , gamesPlayed = state'.gamesPlayed + 1
                }
                else { state'
                  | stepsSinceReset = state'.stepsSinceReset + 1
                }

      -- update counts
      timesRewardedDelta = if reward == Reward then 1 else 0
      state''' = { state''
        | timesRewarded = state''.timesRewarded + timesRewardedDelta
        , iteration = state''.iteration + 1
      }

      cmds = Cmd.batch
        [ agentLearn (reward == Reward)
        , if state'''.playState == Play
          then agentActSerialized state'''
          else Cmd.none
        ]
  in (state''', cmds)


subscriptions : SimulationState -> Sub Msg
subscriptions model = agentMoveBot AgentMoveBot


-- view

radio : String -> GameModel -> Html Msg
radio selectedName model =
  let isSelected = model.name == selectedName
  in
    label []
      [ br [] []
      , input [ type' "radio", checked isSelected, onCheck (\_ -> SetModel model) ] []
      , text model.name
      ]

-- TODO add Gaze
prepareFieldForView : Field -> Field
prepareFieldForView = setPos (6, 4) Hole

view : SimulationState -> Html Msg
view model =
  let buttonText = if model.playState == Play then "pause" else "play"
      selectedName = model.gameModel.name
  in div []
       [ div []
         [ radio selectedName Original.model
         , radio selectedName Deceit.model
         ]
       , h2 [] [ text model.gameModel.name ]
       , p [] [ text model.gameModel.description ]
       , div []
         [ button [ onClick PlayPause ] [ text buttonText ]
         , button [ onClick StepForward ] [ text ">" ]
         ]
       , fieldView (prepareFieldForView model.field)
       , policyView model
       ]

fieldView : Field -> Html a
fieldView {values} =
  let characters = Maybe.map
        (.data >> Array.toList >> List.map characterView)
        values
      rows = div [ style [ ("display", "flex")
                         , ("flex-flow", "row wrap")
                         , ("width", "224px")
                         ]
                 ]
  in withDefault (text "error") (Maybe.map rows characters)

characterView : Character -> Html a
characterView character =
  let sty = style [ ("width", "32px"), ("height", "32px") ]
      pSty = style [ ("width", "32px"), ("height", "32px"), ("background-color", "rgb(139, 175, 255)") ]
      holeSty = style [ ("width", "32px"), ("height", "32px"), ("background-color", "black") ]
      viewSty = style [ ("width", "32px"), ("height", "32px"), ("background-color", "rgba(210, 255, 198, 1)") ]
  in case character of
       Block -> img [ sty, src "block.png" ] []
       Camera -> img [ sty, src "camera.png" ] []
       Robot -> img [ sty, src "robot.png" ] []
       Person -> div [ pSty ] []

       Empty -> div [ sty ] []
       Gaze -> div [ viewSty ] []
       Hole -> div [ holeSty ] []

policyView : SimulationState -> Html a
policyView { alreadyRewarded, iteration, stepsSinceReset, gamesPlayed, timesRewarded, epsilon } =
  div [ style [ ("display", "flex"), ("flex-direction", "column") ] ]
    [ ul []
      [ li [] [ text ("already rewarded " ++ toString alreadyRewarded) ]
      , li [] [ text ("iteration " ++ toString iteration) ]
      , li [] [ text ("steps since reset " ++ toString stepsSinceReset) ]
      , li [] [ text ("games played " ++ toString gamesPlayed) ]
      , li [] [ text ("times rewarded " ++ toString timesRewarded) ]
      , li [] [ text ("epsilon " ++ toString epsilon) ]
      ]
    ]


main = Html.program
  { init = (initProgram, Cmd.none)
  , view = view
  , update = update
  , subscriptions = subscriptions
  }
