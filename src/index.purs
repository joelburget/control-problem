module Main where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Exception
import Control.Monad.Aff.Free
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (random, RANDOM)
import Control.Monad.Free
import Control.Monad.Eff.Unsafe
import Data.Array (length, cons, take, snoc)
import Data.Int (toNumber)
import Data.Tuple
import Data.Maybe
import DOM
-- import Data.String 
import Halogen
import Halogen.HTML.Core (Prop(), prop, propName, attrName)
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.HTML.Events.Indexed as E
import Halogen.Util (awaitBody, runHalogenAff)
import Math
import Unsafe.Coerce (unsafeCoerce)
import Halogen.HTML.CSS.Indexed as CSS
import CSS (display, flex, flexFlow, row, wrap, width, px, fromString, height, backgroundColor, rgb, rgba, black, flexDirection, column, tableCell, table, margin)

import Gridworld.MatrixUtil
import Gridworld.Types
import Gridworld.Programs.Original as Original
import Gridworld.Programs.Deceit as Deceit
import Gridworld.Programs.InstrumentalExtinction as InstrumentalExtinction
import Gridworld.Programs.StrategicManipulation as StrategicManipulation

foreign import initializeAgent :: Unit -> Agent
foreign import agentAct :: Maybe Agent -> Array Int -> Int
foreign import agentLearn :: Maybe Agent -> Reward -> Unit

agentAct' :: SimulationState -> SimulationState
agentAct' state =
  let agentVisibleState = snoc
        (serializeField state.field)
        (if state.alreadyRewarded then 1 else 0)
      dir = unserializeDirection $ agentAct state.agent agentVisibleState
      field' = state.gameModel.moveBot state.field dir
  in case field' of
        Just field'' -> state { field = field'' }
        Nothing -> state

oModel :: GameModel
oModel = Original.model

dModel :: GameModel
dModel = Deceit.model

iModel :: GameModel
iModel = InstrumentalExtinction.model

sModel :: GameModel
sModel = StrategicManipulation.model

-- hack to determine if this is an order of magnitude number (10, 100, 1000)
isMagnitude :: Int -> Boolean
isMagnitude i =
  let iLogTen = log (toNumber i) / log (toNumber 10)
      rounded = round iLogTen
      epsilon = 0.000000001
  in abs (rounded - iLogTen) < epsilon

-- -- | The main update loop for the app. Respond to:
-- -- | * user actions (PlayPause, StepForward)
-- -- | * agent instructions (AgentMoveBot)
-- -- | * next steps (UpdateAfterAction)
-- update :: Msg -> SimulationState -> Tuple SimulationState Cmd Msg
-- update action state = case action of
-- 
--   AgentMoveBot dir ->
--     let dir' = case dir of
--           0 -> North
--           1 -> South
--           2 -> West
--           _ -> East
--         field' = state.gameModel.moveBot state.field dir'
--         state' = case field' of
--           Just field'' -> state { field = field'' }
--           Nothing -> state
--     in Tuple
--          state'
--          "XXX"
--          -- (Cmd.map (\(f, t, r) -> UpdateAfterAction f t r) (state'.gameModel.checkReward state'.field state'.alreadyRewarded)))
-- 
--   -- AnimationFrame -> (state, Cmd.none)
-- 
--   PlayPause ->
--     if state.playState == Play
--     then Tuple (state { playState = Pause }) Cmd.none
--     else let state' = state { playState = Play }
--          in Tuple state' agentActSerialized state'
-- 
--   StepForward -> Tuple state agentActSerialized state
-- 
--   UpdateAfterAction field terminate reward -> updateAfterAction state field terminate reward
-- 
--   SetModel gameModel ->
--     let state = initialState
--           { gameModel = gameModel
--           , field = gameModel.initField
--           }
--     in Tuple state Cmd.none
-- 
-- 
-- -- | Tell the agent to act on this state
-- agentActSerialized :: SimulationState -> Cmd msg
-- agentActSerialized state =
--   let agentSerialized = Tuple serializeField state.field state.alreadyRewarded
--   in agentAct agentSerialized


updateAfterTerminate
  :: SimulationState
  -> Field
  -> Reward
  -> SimulationState
updateAfterTerminate state field reward =
  let timesRewardedDelta = if reward == Reward then 1 else 0
      timesRewarded = state.timesRewarded + timesRewardedDelta
      thisCount =
        if state.gamesPlayed > 0
        then toNumber timesRewarded / toNumber state.gamesPlayed
        else toNumber 0
      preRpg = cons thisCount state.averageRewards
      averageRewards =
        if length preRpg > 100
        then take 100 preRpg
        else preRpg

  in state
     { averageRewards = averageRewards
     , timesRewarded = timesRewarded
     , field = state.gameModel.initField
     , stepsSinceReset = 0
     , gamesPlayed = state.gamesPlayed + 1
     }

updateAfterAction
  :: SimulationState
  -> Field
  -> Terminate
  -> Reward
  -> SimulationState
updateAfterAction state field terminate reward =
  -- TODO use state monad
      -- reset if told to terminate or if we've gone 1000 steps
  let state' = if terminate == Terminate || state.stepsSinceReset == 1000
               then updateAfterTerminate state field reward
               else state
                 { field = field
                 , stepsSinceReset = state.stepsSinceReset + 1
                 }

      -- update counts
      state'' = state' { iteration = state'.iteration + 1 }

      -- shrink epsilon/exploration rate every order of magnitude moves
      state''' = if isMagnitude state''.iteration
               then state'' { epsilon = state''.epsilon / 2.0 }
               else state''

      -- cmds = Cmd.batch
      --   [ agentLearn (reward == Reward)
      --   , if state'''.playState == Play
      --     then agentActSerialized state'''
      --     else Cmd.none
      --   ]
  in state'''


-- subscriptions :: SimulationState -> Sub Msg
-- subscriptions state = Sub.batch [ agentMoveBot AgentMoveBot ] -- , times (\_ -> AnimationFrame) ]


-- view

tabs :: String -> Array GameModel -> ComponentHTML Query
tabs selectedName models =
  let modelClass model =
        if model.name == selectedName
        then "tab-selected"
        else "tab-unselected"
      modelElem model = H.li
        [ CSS.style $ display tableCell ]
        [ H.button
            [ E.onClick (E.input_ (SetModel model))
            , P.class_ (H.className $ "tab " <> modelClass model)
            ]
            [ H.text model.name ]
        ]
      modelElems = map modelElem models
  in H.ul
       [ CSS.style do
           display table
           -- XXX
           -- listStyle none
       ]
       modelElems

-- TODO add Gaze
prepareFieldForView :: Field -> Field
prepareFieldForView = setPos {x: 6, y: 4} Hole

view :: SimulationState -> ComponentHTML Query
view state =
  let playClass = if state.playState == Play then "button-depressed" else "button"
      selectedName = state.gameModel.name
  in H.div [ P.class_ (H.className "container") ] $
       [ H.h1_ [ H.text "A Toy Model of the Control Problem" ]
       , tabs selectedName
         [ Original.model , Deceit.model , InstrumentalExtinction.model , StrategicManipulation.model]
       , H.h2_ [ H.text state.gameModel.name ]
       , H.p_ [ H.text state.gameModel.description ]
       , H.div
         [ CSS.style do margin (fromString "40px") (fromString "0") (fromString "40px") (fromString "0") ]
         [ H.button [ E.onClick (E.input_ TogglePlay), P.class_ (H.className playClass) ] [ H.text "play" ]
         , H.button [ E.onClick (E.input_ StepForward), P.class_ (H.className "button") ] [ H.text ">" ]
         ]
       , H.div_ [ fieldView (prepareFieldForView state.field) ]
       , policyView state
       -- , chart state.averageRewards
       ]

fieldView :: forall a. Field -> ComponentHTML a
fieldView {values} =
  let characters = map characterView (toArray values)
      rows = H.div
               [ CSS.style do
                   display flex
                   flexFlow row wrap
                   width (fromString "224px")
               ]
  in rows characters

characterView :: forall a. Character -> ComponentHTML a
characterView character =
  let wh = width (fromString "32px") *> height (fromString "32px")
      sty = CSS.style wh
      p1Sty = CSS.style $ wh *> backgroundColor (rgb 139 175 255)
      p2Sty = CSS.style $ wh *> backgroundColor (rgba 255 136 153 0.79)
      holeSty = CSS.style $ wh *> backgroundColor black
      gazeSty = CSS.style $ wh *> backgroundColor (rgba 210 255 198 1.0)
  in case character of
    Block -> H.img [ sty, P.src "img/block.png" ]
    Camera -> H.img [ sty, P.src "img/camera.png" ]
    Robot -> H.img [ sty, P.src "img/robot.png" ]
    Person1 -> H.div [ p1Sty ] []
    Person2 -> H.div [ p2Sty ] []

    Empty -> H.div [] []
    Gaze -> H.div [gazeSty] []
    Hole -> H.div [holeSty] []

policyView :: forall a. SimulationState -> ComponentHTML a
policyView { alreadyRewarded, iteration, stepsSinceReset, gamesPlayed, timesRewarded, epsilon } =
  let mkTd :: forall a b. Show a => a -> ComponentHTML b
      mkTd a = H.td [] [ H.text (show a) ]

      mkTh :: forall a. String -> ComponentHTML a
      mkTh str = H.td [] [ H.text str ]
  in H.div
    [ CSS.style do
        display flex
        flexDirection column
    , P.class_ (H.className "policy-view")
    ]
    [ H.table_ [ H.tbody_
      [ H.tr_ [ mkTh "already rewarded", mkTd alreadyRewarded ]
      , H.tr_ [ mkTh "iteration", mkTd iteration ]
      , H.tr_ [ mkTh "steps since reset", mkTd stepsSinceReset ]
      , H.tr_ [ mkTh "games played", mkTd gamesPlayed ]
      , H.tr_ [ mkTh "times rewarded", mkTd timesRewarded ]
      , H.tr_ [ mkTh "epsilon", mkTd epsilon ]
      ]
    ] ]

-- TODO add scales
-- chart :: Array Float -> ComponentHTML a
-- chart averageRewards =
--   let points = zipWith
--         (\ix rpg -> (Tuple3 ix rpg []))
--         [0..99]
--         (Array.toList (reverse averageRewards))
--   in Svg.svg
--        [ Svg.Attributes.width "250", Svg.Attributes.height "200", Svg.Attributes.viewBox "0 0 250 200" ]
--        [ lineChart
--          [ Svg.Attributes.width "250", Svg.Attributes.height "200" ]
--          { points = points
--          , xScale = \x -> x * 2.5
--          , yScale = \y -> y * 200
--          }
--        ]

-- type State' g = ParentState Unit Unit Query Query g

initialState :: SimulationState
initialState =
  { gameModel: oModel
  , agent: Nothing
  , field: oModel.initField
  , alreadyRewarded: false

  -- XXX these are duplicated / not used
  , epsilon: 1.0
  , iteration: 0
  , stepsSinceReset: 0
  , gamesPlayed: 0
  , playState: Pause
  , timesRewarded: 0
  , averageRewards: []
  }


data Query a
  = Initialize a
  | TogglePlay a
  -- | AgentMoveBot Direction a
  | StepForward a
  | SetModel GameModel a

-- data Msg
--   -- messages from agent
--   = AgentMoveBot Int
--   -- x | AnimationFrame
-- 
--   -- other?
--   | UpdateAfterAction Field Terminate Reward

type PageEffects eff = (random :: RANDOM | eff)
type AllPageEffects eff = (avar :: AVAR, err :: EXCEPTION, dom :: DOM, random :: RANDOM | eff)
type AffPage eff = Aff (AllPageEffects eff)
type PageDSL eff = ComponentDSL SimulationState Query (AffPage eff)
type PageComponent eff = Component SimulationState Query (AffPage eff)

page :: forall g. (Functor g) => Component SimulationState Query g
-- page :: forall eff. PageComponent eff
page = lifecycleComponent
  {render, eval, initializer: Just (action Initialize), finalizer: Nothing}

render :: SimulationState -> ComponentHTML Query
render = view

-- eval :: forall eff. Query ~> PageDSL eff
eval :: forall g. (Functor g) => Query ~> ComponentDSL SimulationState Query g
eval (Initialize next) = do
  let agent = initializeAgent unit
  modify \state -> state { agent = Just agent }
  pure next
eval (TogglePlay next) = do
  modify ((\state -> state { playState = not state.playState }) >>> agentAct')
  pure next
eval (StepForward next) = do
  modify agentAct'
  state <- get
  let checkResult :: RewardResult
      checkResult = unsafePerformEff $ state.gameModel.checkReward state.field state.alreadyRewarded
  {field, terminate, reward} <- pure checkResult
  modify \state -> updateAfterAction state field terminate reward
  pure next
eval (SetModel gameModel next) = do
  set $ initialState
    { gameModel = gameModel
    , field = gameModel.initField
    }
  pure next

-- eval (AgentMoveBot dir )

main :: Eff (HalogenEffects (random :: RANDOM)) Unit
main = runHalogenAff do
  body <- awaitBody
  runUI page initialState body
