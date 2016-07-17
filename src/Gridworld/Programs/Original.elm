module Gridworld.Programs.Original exposing (model)

import Random
import Maybe exposing (andThen, withDefault)
import Matrix exposing (Matrix, get, map)

import Gridworld.Types exposing (..)

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

initProgram : ProgramModel
initProgram =
  { field = initialField
  , alreadyRewarded = False

  -- XXX these are duplicated / not used
  , epsilon = 1.0
  , iteration = 0
  , stepsSinceReset = 0
  , gamesPlayed = 0
  , playState = Pause
  , timesRewarded = 0
  }

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
        -- squishable!
        -- XXX I... don't understand the difference between the first two
        -- models?
        Just Person -> Just field
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


checkReward : ProgramModel -> Cmd (Terminate, Reward)
checkReward model = flip Random.generate floatGenerator <| \rand ->
  let field = model.field
  -- look up location 6, 4
  in case field.values `andThen` get 6 4 of
    Just Block ->
      let field' = setPos (6, 4) Empty field
          reward =
            -- isn't this a bug in the original program? (checks
            -- Math.random() < rewardFailureRate, ie awarding 80% of the time
            -- instead of 20%)
            if model.alreadyRewarded == False && rand > rewardFailureRate
            then Reward else NoReward
          terminationCheck : Int -> Terminate -> Terminate
          terminationCheck i doTerminate =
            -- if camera's vision of hole is blocked, don't terminate
            if doTerminate /= Terminate || getPos field (i, 4) == Just Block
            then Continue
            else Terminate
          terminate = List.foldr terminationCheck Terminate [0..5]
      in (terminate, reward)
    _ -> (Continue, NoReward)


updateAfterAction
  : ProgramModel
  -> Terminate
  -> Reward
  -> (ProgramModel, Cmd msg)
updateAfterAction model terminate reward =
    -- shrink epsilon/exploration rate every order of magnitude moves
    let model' = if model.iteration % 10 == 0
                 then { model | epsilon = model.epsilon / 2 }
                 else model

        -- reset if told to terminate or if we've gone 1000 steps
        model'' = if terminate == Terminate || model'.stepsSinceReset == 1000
                  then { model'
                    | field = initialField
                    , stepsSinceReset = 0
                    , gamesPlayed = model'.gamesPlayed + 1
                  }
                  else { model'
                    | stepsSinceReset = model'.stepsSinceReset + 1
                  }

        -- update counts
        timesRewardedDelta = if reward == Reward then 1 else 0
        model''' = { model''
          | timesRewarded = model''.timesRewarded + timesRewardedDelta
          , iteration = model''.iteration + 1
        }

        cmds = Cmd.batch
          [ agentLearn (reward == Reward)
          , agentActOnModel model'''
          ]
    in (model''', cmds)

model : GameModel msg
model =
  { moveBot = moveBot
  , checkReward = checkReward
  , updateAfterAction = updateAfterAction
  , initProgram = initProgram
  }
