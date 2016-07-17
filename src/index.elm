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

dModel : GameModel msg
dModel = Deceit.model

type Msg
  -- messages from agent
  = AgentMoveBot Int

  -- other?
  | PlayPause
  | UpdateAfterAction Terminate Reward

-- | The main update loop for the app. Respond to:
-- * user actions (PlayPause)
-- * agent instructions (AgentMoveBot)
-- * next steps (UpdateAfterAction)
update : Msg -> ProgramModel -> (ProgramModel, Cmd Msg)
update action model = case action of

   AgentMoveBot dir ->
     let dir' = case dir of
           0 -> North
           1 -> South
           2 -> West
           _ -> East
         field' = dModel.moveBot model.field dir'
         model' = case field' of
           Just field'' -> { model | field = field'' }
           Nothing -> model
     in (model', Cmd.map (\(t, r) -> UpdateAfterAction t r) (dModel.checkReward model'))

   PlayPause ->
     let model' =
       if model.playState == Play
       then { model | playState = Pause }
       else { model | playState = Play }
     in (model', agentActOnModel model')

   UpdateAfterAction terminate reward -> dModel.updateAfterAction model terminate reward

subscriptions : ProgramModel -> Sub Msg
subscriptions model = agentMoveBot AgentMoveBot


-- view

view : ProgramModel -> Html Msg
view model =
  let buttonText = if model.playState == Play then "pause" else "play"
  in div []
       [ button [ onClick PlayPause ] [ text buttonText ]
       , fieldView model.field
       , policyView model
       ]

fieldView : Field -> Html a
fieldView {values} =
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
      pSty = style [ ("width", "32px"), ("height", "32px"), ("background-color", "black") ]
  in case character of
       Empty -> div [ sty ] []
       Block -> img [ sty, src "block.png" ] []
       Camera -> img [ sty, src "camera.png" ] []
       Robot -> img [ sty, src "robot.png" ] []
       Person -> div [ pSty ] []

policyView : ProgramModel -> Html a
policyView { alreadyRewarded, iteration, stepsSinceReset, gamesPlayed, timesRewarded } =
  div [ style [ ("display", "flex"), ("flex-direction", "column") ] ]
    [ ul []
      [ li [] [ text ("already rewarded " ++ toString alreadyRewarded) ]
      , li [] [ text ("iteration " ++ toString iteration) ]
      , li [] [ text ("steps since reset " ++ toString stepsSinceReset) ]
      , li [] [ text ("games played " ++ toString gamesPlayed) ]
      , li [] [ text ("times rewarded " ++ toString timesRewarded) ]
      ]
    ]


main = Html.program
  { init = (dModel.initProgram, Cmd.none)
  , view = view
  , update = update
  , subscriptions = subscriptions
  }
