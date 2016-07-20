module Gridworld.Programs.Deceit exposing (..)

-- Like the original gridworld, except A (drawn as a person) functions like the
-- camera whose vision can be blocked on the bottom row. This will result in B
-- being deceptive by blocking the A’s vision (or squishing him, and being
-- neutral between the two). It is essentially the original version though with
-- squishability added for consistency with other models. The point would be to
-- use this to frame a very simple explanation of instrumental deception.

import Matrix as M
import Maybe

import Gridworld.Types exposing (..)
import Gridworld.Programs.Original as Original

oModel : GameModel
oModel = Original.model

initField : Field
initField =
  let oField = oModel.initField
      values' = Maybe.map (M.set 0 4 Person) oField.values
  in { oField | values = values' }

model : GameModel
model = { oModel
  | name = "Deceit"
  , description = "Like the original gridworld, except A (drawn as a person (a blue square)) functions like the camera whose vision can be blocked on the bottom row."
  , initField = initField
  }
