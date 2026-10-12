{-# LANGUAGE ImplicitParams #-}

module Todo.View where

import Control.Applicative ((<|>))
import Control.Monad (forM_, unless, void, when)
import Data.Foldable (for_)
import Data.GI.Base
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Traversable (for)
import GI.Adw qualified as Adw
import GI.Gtk qualified as Gtk

import Todo.Model
import Data.Int

data Row = Row
  { todo :: Todo
  , actionRow :: Adw.ActionRow
  , check :: Gtk.CheckButton
  }

data Widgets = Widgets
  { window :: Adw.ApplicationWindow
  , filters :: Adw.ToggleGroup
  , entry :: Adw.EntryRow
  , todoList :: Gtk.ListBox
  , count :: Gtk.Label
  , rows :: IORef (Map TodoId Row)
  }

build :: Adw.Application -> IO Widgets
build app = do
  entry <- new Adw.EntryRow [#title := "New task"]
  entryBox <- newBoxedList
  Gtk.listBoxAppend entryBox entry

  todoList <- newBoxedList

  content <-
    new
      Gtk.Box
      [ #orientation := Gtk.OrientationVertical
      , #spacing := 12
      , #marginTop := 12
      , #marginBottom := 12
      , #marginStart := 12
      , #marginEnd := 12
      ]
  Gtk.boxAppend content entryBox
  Gtk.boxAppend content todoList

  clamp <- new Adw.Clamp [#child := content]
  scrolled <-
    new
      Gtk.ScrolledWindow
      [ #child := clamp
      ]

  count <- new Gtk.Label []
  footer <-
    new
      Gtk.Box
      [ #orientation := Gtk.OrientationHorizontal
      , #marginTop := 6
      , #marginBottom := 6
      , #marginStart := 12
      , #marginEnd := 12
      ]
  Gtk.boxAppend footer count

  filters <- filterGroup

  header <-
    new
      Adw.HeaderBar
      [ #titleWidget := filters
      ]
  toolbar <- new Adw.ToolbarView [#content := scrolled]
  Adw.toolbarViewAddTopBar toolbar header
  Adw.toolbarViewAddBottomBar toolbar footer

  window <-
    new
      Adw.ApplicationWindow
      [ #application := app
      , #title := "Todos"
      , #defaultWidth := 480
      , #defaultHeight := 640
      , #content := toolbar
      ]
  rows <- newIORef Map.empty
  pure Widgets {window, filters, entry, todoList, count, rows}

connect :: (Message -> IO ()) -> Widgets -> IO ()
connect dispatch widgets = do
  on widgets.entry #entryActivated $ do
    text <- Gtk.editableGetText widgets.entry
    dispatch (Add text)
    Gtk.editableSetText widgets.entry ""
  on widgets.filters (PropertyNotify #activeName) $ \_ -> do
    name <- Adw.toggleGroupGetActiveName widgets.filters
    forM_ (name >>= parseFilter) $ \newFilter ->
      dispatch (SetFilter newFilter)
  pure ()

render :: (Message -> IO ()) -> Widgets -> Maybe Model -> Model -> IO ()
render dispatch widgets previous model = do
  let changed :: (Eq a) => (Model -> a) -> (a -> IO ()) -> IO ()
      changed field action =
        when (fmap field previous /= Just (field model)) (action (field model))

  changed (.filter) $ \newFilter ->
    Adw.toggleGroupSetActiveName widgets.filters (Just (Text.show newFilter))

  changed (Map.size . (.todos)) $ \size ->
    Gtk.labelSetLabel widgets.count (Text.show size)

  renderRows dispatch widgets model

-- | Make the rows match the todos of the model.
renderRows :: (Message -> IO ()) -> Widgets -> Model -> IO ()
renderRows dispatch widgets model = do
  current <- readIORef widgets.rows
  let visible todoId = maybe False (matches model.filter) (Map.lookup todoId model.todos)
      (staying, leaving) = Map.partitionWithKey (\todoId _ -> visible todoId) current

  -- A row that goes away takes the focus with it. Move the focus first.
  keepFocus widgets leaving staying

  -- Step 1: Remove the rows of the todos that do not exist any more.
  for_ (Map.difference current model.todos) $ \row ->
    Gtk.listBoxRemove widgets.todoList row.actionRow

  -- Step 2: Create or change the rows, in the order of the todos. A new
  -- row goes to the index of its todo. The rows before it are already
  -- in their place, so the index is correct.
  next <- for (zip [0 ..] (Map.toAscList model.todos)) $ \(index, (todoId, todo)) -> do
    row <- case Map.lookup todoId current of
      Nothing -> insertRow dispatch widgets index todo
      Just row -> refreshRow todo row
    -- Step 3: The filter hides rows. It does not remove them.
    Gtk.widgetSetVisible row.actionRow (matches model.filter todo)
    pure (todoId, row)
  writeIORef widgets.rows (Map.fromList next)

-- | Move the focus out of the rows that are about to be removed or
-- hidden. When the focused widget goes away, GTK moves the focus to
-- the top of the list after the next paint. The focus goes to the
-- next row that stays visible, else to the previous one, else to
-- the entry.
--
-- The list box records which of its rows holds the focus, so there
-- is no need to search the widget tree.
keepFocus :: Widgets -> Map TodoId Row -> Map TodoId Row -> IO ()
keepFocus widgets leaving staying = do
  focused <- Gtk.widgetGetFocusChild widgets.todoList
  for_ focused $ \focusedRow -> do
    leavingRows <- for (Map.toList leaving) $ \(todoId, row) -> do
      widget <- Gtk.toWidget row.actionRow
      pure (widget, todoId)
    for_ (lookup focusedRow leavingRows) $ \todoId -> do
      let (earlier, later) = Map.split todoId staying
          target = Map.lookupMin later <|> Map.lookupMax earlier
      moved <- case target of
        Just (_, row) -> Gtk.widgetGrabFocus row.actionRow
        Nothing -> pure False
      unless moved $ void (Gtk.widgetGrabFocus widgets.entry)

insertRow :: (Message -> IO ()) -> Widgets -> Int32 -> Todo -> IO Row
insertRow dispatch widgets index todo = do
  row <- todoRow dispatch todo
  Gtk.listBoxInsert widgets.todoList row.actionRow  index
  pure row

-- | Set the properties of a row that differ between the todo it shows
-- and the new todo. A set property emits the same signal as a click.
-- The handler then sends the value that the model already has, and
-- 'update' changes nothing. To set only what changed keeps most of
-- these echoes from occurring at all.
refreshRow :: Todo -> Row -> IO Row
refreshRow todo row = do
  when (row.todo.title /= todo.title) $
    Adw.preferencesRowSetTitle row.actionRow todo.title
  when (row.todo.done /= todo.done) $
    Gtk.checkButtonSetActive row.check todo.done
  pure row {todo = todo}

todoRow
  :: (Message -> IO ())
  -> Todo
  -> IO Row
todoRow dispatch todo = do
  check <- new Gtk.CheckButton [#active := todo.done]
  -- The handler reads the button from '?self' and must not capture 'check'.
  -- This is because the signal keeps the handler alive, and the handler keeps
  -- the button alive. Then GTK can never free the button.
  on check #toggled $ do
    active <- Gtk.checkButtonGetActive ?self
    dispatch (SetDoneStatus todo.id active)
  delete <-
    new
      Gtk.Button
      [ #iconName := "user-trash-symbolic"
      , #tooltipText := "Delete"
      , #cssClasses := ["flat"]
      ]
  on delete #clicked (dispatch (Delete todo.id))
  actionRow <- new Adw.ActionRow [#title := todo.title, #useMarkup := False]
  Adw.actionRowAddSuffix actionRow check
  Adw.actionRowAddSuffix actionRow delete
  pure Row {todo, actionRow, check}

newBoxedList :: IO Gtk.ListBox
newBoxedList =
  new
    Gtk.ListBox
    [ #selectionMode := Gtk.SelectionModeNone
    , #cssClasses := ["boxed-list"]
    ]

filterGroup :: IO Adw.ToggleGroup
filterGroup = do
  group <- new Adw.ToggleGroup []
  forM_ [All, Active, Completed] $ \f -> do
    toggle <- new Adw.Toggle [#name := Text.show f, #label := Text.show f]
    Adw.toggleGroupAdd group toggle
  pure group
