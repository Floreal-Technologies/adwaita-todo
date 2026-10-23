module Todo.Model where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text

newtype TodoId = TodoId Word
  deriving stock (Show)
  deriving newtype (Eq, Ord)

data Todo = Todo
  { id :: TodoId
  , title :: Text
  , done :: Bool
  }
  deriving stock (Eq, Show)

data Filter
  = All
  | Active
  | Completed
  deriving stock (Eq, Show, Enum, Bounded)

-- | All the filters, in the order of the toggles of the view.
allFilters :: [Filter]
allFilters = [minBound .. maxBound]

matches :: Filter -> Todo -> Bool
matches All _ = True
matches Active todo = not todo.done
matches Completed todo = todo.done

-- | True when there is at least one todo and all todos are done.
allDone :: Model -> Bool
allDone model =
  not (Map.null model.todos) && all (\todo -> todo.done) model.todos

data Model = Model
  { todos :: Map TodoId Todo
  , nextId :: TodoId
  , filter :: Filter
  }
  deriving stock (Eq, Show)

init :: Model
init =
  Model
    { todos = Map.empty
    , nextId = TodoId 0
    , filter = All
    }

data Message
  = Add Text
  | SetDoneStatus TodoId Bool
  | ToggleAll
  | Delete TodoId
  | SetFilter Filter
  deriving stock (Eq, Show)

update :: Message -> Model -> Model
update message model = case message of
  Add raw ->
    let text = Text.strip raw
        todoId@(TodoId n) = model.nextId
        todo = Todo {id = todoId, title = text, done = False}
    in if Text.null text
         then model
         else
           model
             { todos = Map.insert todo.id todo model.todos
             , nextId = TodoId (n + 1)
             }
  SetDoneStatus todoId value ->
    model {todos = Map.adjust (\todo -> todo {done = value}) todoId model.todos}
  ToggleAll ->
    let value = not (allDone model)
    in model {todos = Map.map (\todo -> todo {done = value}) model.todos}
  Delete todoId ->
    model {todos = Map.delete todoId model.todos}
  SetFilter newFilter ->
    model {filter = newFilter}
