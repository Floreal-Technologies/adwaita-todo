module Todo.Model where

import Data.List qualified as List
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
  deriving stock (Eq, Show, Ord)

parseFilter :: Text -> Maybe Filter
parseFilter = \case
  "All" -> Just All
  "Active" -> Just Active
  "Completed" -> Just Completed
  _ -> Nothing

matches :: Filter -> Todo -> Bool
matches All _ = True
matches Active todo = not todo.done
matches Completed todo = todo.done

visibleTasks :: Model -> [Todo]
visibleTasks model =
  List.filter (matches model.filter) (Map.elems model.todos)

data Model = Model
  { todos :: Map TodoId Todo
  , nextId :: TodoId
  , filter :: Filter
  }
  deriving stock (Eq, Show)

data Effect
  = Save [Todo]
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
  | Delete TodoId
  | SetFilter Filter
  deriving stock (Eq, Ord)

update :: Message -> Model -> (Model, [Effect])
update message model = case message of
  Add raw ->
    let text = Text.strip raw
        todoId@(TodoId n) = model.nextId
        todo = Todo {id = todoId, title = text, done = False}
    in if Text.null text
         then (model, [])
         else
           withTodos
             (Map.insert todo.id todo)
             model {nextId = TodoId (n + 1)}
  SetDoneStatus todoId value ->
    withTodos (Map.adjust (\todo -> todo {done = value}) todoId) model
  Delete todoId -> do
    withTodos (Map.delete todoId) model
  SetFilter newFilter -> do
    (model {filter = newFilter}, [])
  where
    -- This is where we determine if our todos have changed,
    -- so that we can save them.
    withTodos f changed =
      let result = changed {todos = f changed.todos}
      in if result.todos == model.todos
           then (result, [])
           else (result, [Save (Map.elems result.todos)])
