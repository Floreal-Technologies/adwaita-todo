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

data Model = Model
  { todos :: Map TodoId Todo
  , nextId :: TodoId
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
    }

data Message
  = Add Text
  | SetDoneStatus TodoId Bool
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
  where
    -- This is where we determine if our todos have changed,
    -- so that we can save them.
    withTodos f changed =
      let result = changed {todos = f changed.todos}
      in if result.todos == model.todos
           then (result, [])
           else (result, [Save (Map.elems result.todos)])
