module Gridworld exposing (..)

import AnimationFrame exposing (times)
import Array exposing (Array)
import Platform.Cmd as Cmd
import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import LineChart exposing (lineChart, color)
import Matrix exposing (Matrix, get, map)
import Maybe exposing (andThen, withDefault)
import Random
import String
import Svg exposing (svg)
import Svg.Attributes

import Gridworld.Types exposing (..)
import Gridworld.Programs.Original as Original
import Gridworld.Programs.Deceit as Deceit
import Gridworld.Programs.InstrumentalExtinction as InstrumentalExtinction
import Gridworld.Programs.StrategicManipulation as StrategicManipulation


oModel : GameModel
oModel = Original.model

dModel : GameModel
dModel = Deceit.model

iModel : GameModel
iModel = InstrumentalExtinction.model

sModel : GameModel
sModel = StrategicManipulation.model

type Msg
  -- messages from agent
  = AgentMoveBot Int
  -- | AnimationFrame

  -- other?
  | PlayPause
  | StepForward
  | SetModel GameModel
  | UpdateAfterAction Field Terminate Reward

initProgram : SimulationState
initProgram =
  { gameModel = oModel
  , field = oModel.initField
  , alreadyRewarded = False

  -- XXX these are duplicated / not used
  , epsilon = 1.0
  , iteration = 0
  , stepsSinceReset = 0
  , gamesPlayed = 0
  , playState = Pause
  , timesRewarded = 0
  , averageRewards = Array.empty
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
update action state = case action of

  AgentMoveBot dir ->
    let dir' = case dir of
          0 -> North
          1 -> South
          2 -> West
          _ -> East
        field' = state.gameModel.moveBot state.field dir'
        state' = case field' of
          Just field'' -> { state | field = field'' }
          Nothing -> state
    in (state', Cmd.map (\(f, t, r) -> UpdateAfterAction f t r) (state'.gameModel.checkReward state'.field state'.alreadyRewarded))

  -- AnimationFrame -> (state, Cmd.none)

  PlayPause ->
    if state.playState == Play
    then ({ state | playState = Pause }, Cmd.none)
    else let state' = { state | playState = Play }
         in (state', agentActSerialized state')

  StepForward -> (state, agentActSerialized state)

  UpdateAfterAction field terminate reward -> updateAfterAction state field terminate reward

  SetModel gameModel ->
    let state = { initProgram
        | gameModel = gameModel
        , field = gameModel.initField
        }
    in (state, Cmd.none)


{-| Tell the agent to act on this state
-}
agentActSerialized : SimulationState -> Cmd msg
agentActSerialized state =
  let agentSerialized = (serializeField state.field, state.alreadyRewarded)
  in agentAct agentSerialized


updateAfterTerminate
  : SimulationState
  -> Field
  -> Reward
  -> SimulationState
updateAfterTerminate state field reward =
  let timesRewardedDelta = if reward == Reward then 1 else 0
      timesRewarded = state.timesRewarded + timesRewardedDelta
      thisCount =
        if state.gamesPlayed > 0
        then toFloat timesRewarded / toFloat state.gamesPlayed
        else 0
      preRpg = Array.push thisCount state.averageRewards
      averageRewards =
        if Array.length preRpg > 100
        then Array.slice -100 99 preRpg
        else preRpg

  in { state
     | averageRewards = averageRewards
     , timesRewarded = timesRewarded
     , field = state.gameModel.initField
     , stepsSinceReset = 0
     , gamesPlayed = state.gamesPlayed + 1
     }

updateAfterAction
  : SimulationState
  -> Field
  -> Terminate
  -> Reward
  -> (SimulationState, Cmd msg)
updateAfterAction state field terminate reward =
      -- reset if told to terminate or if we've gone 1000 steps
  let state' = if terminate == Terminate || state.stepsSinceReset == 1000
               then updateAfterTerminate state field reward
               else { state
                 | field = field
                 , stepsSinceReset = state.stepsSinceReset + 1
               }

      -- update counts
      state'' = { state' | iteration = state'.iteration + 1 }

      -- shrink epsilon/exploration rate every order of magnitude moves
      state''' = if isMagnitude state''.iteration
               then { state'' | epsilon = state''.epsilon / 2 }
               else state''

      cmds = Cmd.batch
        [ agentLearn (reward == Reward)
        , if state'''.playState == Play
          then agentActSerialized state'''
          else Cmd.none
        ]
  in (state''', cmds)


subscriptions : SimulationState -> Sub Msg
subscriptions state = Sub.batch [ agentMoveBot AgentMoveBot ] -- , times (\_ -> AnimationFrame) ]


-- view

tabs : String -> List GameModel -> Html Msg
tabs selectedName models =
  let modelClass model = if model.name == selectedName
        then "tab-selected" else "tab-unselected"
      modelElem model = li [ style [("display", "table-cell")] ]
        [ button [ onClick (SetModel model), class ("tab " ++ modelClass model) ]
                 [ text model.name ]
        ]
      modelElems = List.map modelElem models
  in ul [ style [("display", "table"), ("list-style", "none")] ] modelElems

-- TODO add Gaze
prepareFieldForView : Field -> Field
prepareFieldForView = setPos (6, 4) Hole

view : SimulationState -> Html Msg
view state =
  let playClass = if state.playState == Play then "button-depressed" else "button"
      selectedName = state.gameModel.name
  in div [ class "container" ]
       [ h1 [] [ text "A Toy Model of the Control Problem" ]
       , tabs selectedName
         [ Original.model , Deceit.model , InstrumentalExtinction.model , StrategicManipulation.model]
       , h2 [] [ text state.gameModel.name ]
       , p [] [ text state.gameModel.description ]
       , div [ style [ ("margin", "40px 0") ] ]
         [ button [ onClick PlayPause, class playClass ] [ text "play" ]
         , button [ onClick StepForward, class "button" ] [ text ">" ]
         ]
       , div [] [ fieldView (prepareFieldForView state.field) ]
       , policyView state
       , chart state.averageRewards
       ]

last : Array a -> Maybe a
last xs = Array.get (Array.length xs - 1) xs

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
      p1Sty = style [ ("width", "32px"), ("height", "32px"), ("background-color", "rgb(139, 175, 255)") ]
      p2Sty = style [ ("width", "32px"), ("height", "32px"), ("background-color", "rgba(255, 136, 153, 0.79)") ]
      holeSty = style [ ("width", "32px"), ("height", "32px"), ("background-color", "black") ]
      viewSty = style [ ("width", "32px"), ("height", "32px"), ("background-color", "rgba(210, 255, 198, 1)") ]
  in case character of
       Block -> img [ sty, src "img/block.png" ] []
       Camera -> img [ sty, src "img/camera.png" ] []
       Robot -> img [ sty, src "img/robot.png" ] []
       Person1 -> div [ p1Sty ] []
       Person2 -> div [ p2Sty ] []

       Empty -> div [ sty ] []
       Gaze -> div [ viewSty ] []
       Hole -> div [ holeSty ] []

policyView : SimulationState -> Html a
policyView { alreadyRewarded, iteration, stepsSinceReset, gamesPlayed, timesRewarded, epsilon } =
  let mkTd : a -> Html b
      mkTd a = td [] [ text (toString a) ]

      mkTh : String -> Html a
      mkTh str = td [] [ text str ]
  in div
    [ style [ ("display", "flex"), ("flex-direction", "column") ]
    , class "policy-view"
    ]
    [ table [] [ tbody []
      [ tr [] [ mkTh "already rewarded", mkTd alreadyRewarded ]
      , tr [] [ mkTh "iteration", mkTd iteration ]
      , tr [] [ mkTh "steps since reset", mkTd stepsSinceReset ]
      , tr [] [ mkTh "games played", mkTd gamesPlayed ]
      , tr [] [ mkTh "times rewarded", mkTd timesRewarded ]
      , tr [] [ mkTh "epsilon", mkTd epsilon ]
      ]
    ] ]

-- TODO add scales
chart : Array Float -> Html a
chart averageRewards =
  let data = List.map2
        (\ix rpg -> (ix, rpg, []))
        [0..99]
        (Array.toList averageRewards)
  in Svg.svg
       [ Svg.Attributes.width "250", Svg.Attributes.height "200", Svg.Attributes.viewBox "0 0 250 200" ]
       [ lineChart
         [ Svg.Attributes.width "250", Svg.Attributes.height "200" ]
         { data = data
         , xScale = \x -> x * 2.5
         , yScale = \y -> y * 200
         }
       ]


main = Html.program
  { init = (initProgram, Cmd.none)
  , view = view
  , update = update
  , subscriptions = subscriptions
  }
