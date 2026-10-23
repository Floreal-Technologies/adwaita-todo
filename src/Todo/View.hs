{-# LANGUAGE ImplicitParams #-}

module Todo.View where

import Control.Applicative ((<|>))
import Control.Monad (forM_, unless, void, when)
import Data.Foldable (for_)
import Data.GI.Base
import Data.Int (Int32)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Traversable (for)
import GI.Adw qualified as Adw
import GI.Gtk qualified as Gtk

import Todo.Model

-- | The widgets of one row of the todo list. A row lives as long as
-- its todo.
data Row = Row
  { actionRow :: Adw.ActionRow
  , check :: Gtk.CheckButton
  }

-- | The widgets that live as long as the window.
data Widgets = Widgets
  { window :: Adw.ApplicationWindow
  , filters :: Adw.ToggleGroup
  , entry :: Adw.EntryRow
  , toggleAll :: Gtk.Button
  , todoList :: Gtk.ListBox
  , count :: Gtk.Label
  }

build :: Adw.Application -> IO Widgets
build app = do
  -- A button, not a check button. See 'connect'.
  toggleAll <-
    new
      Gtk.Button
      [ #iconName := "object-select-symbolic"
      , #valign := Gtk.AlignCenter
      , #cssClasses := ["flat"]
      ]
  entry <- new Adw.EntryRow [#title := "New task"]
  Adw.entryRowAddPrefix entry toggleAll
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
  pure Widgets {window, filters, entry, toggleAll, todoList, count}

connect :: (Message -> IO ()) -> Widgets -> IO ()
connect dispatch widgets = do
  on widgets.entry #entryActivated $ do
    text <- Gtk.editableGetText widgets.entry
    dispatch (Add text)
    Gtk.editableSetText widgets.entry ""
  on widgets.toggleAll #clicked (dispatch ToggleAll)
  on widgets.filters (PropertyNotify #active) $ \_ -> do
    index <- Adw.toggleGroupGetActive widgets.filters
    for_ (lookup index (zip [0 ..] allFilters)) $ \newFilter ->
      dispatch (SetFilter newFilter)
  pure ()

render
  :: (Message -> IO ())
  -> Widgets
  -> Map TodoId Row
  -> Maybe Model
  -> Model
  -> IO (Map TodoId Row)
render dispatch widgets rows previous model = do
  let changed :: (Eq a) => (Model -> a) -> (a -> IO ()) -> IO ()
      changed field action =
        when (fmap field previous /= Just (field model)) (action (field model))

  changed (.filter) $ \newFilter ->
    Adw.toggleGroupSetActive widgets.filters (fromIntegral (fromEnum newFilter))

  changed (Map.size . (.todos)) $ \size ->
    Gtk.labelSetLabel widgets.count (Text.show size)
  changed allDone $ \done ->
    Gtk.widgetSetTooltipText widgets.toggleAll (Just (toggleAllTooltip done))
  changed (not . Map.null . (.todos)) (Gtk.widgetSetSensitive widgets.toggleAll)

  renderRows dispatch widgets rows previous model

-- | Make the rows match the todos of the model.
renderRows
  :: (Message -> IO ())
  -> Widgets
  -> Map TodoId Row
  -> Maybe Model
  -> Model
  -> IO (Map TodoId Row)
renderRows dispatch widgets rows previous model = do
  let visible todoId = maybe False (matches model.filter) (Map.lookup todoId model.todos)
      (staying, leaving) = Map.partitionWithKey (\todoId _ -> visible todoId) rows

  -- A row that goes away takes the focus with it. Move the focus first.
  keepFocus widgets leaving staying

  -- Step 1: Remove the rows of the todos that do not exist any more.
  for_ (Map.difference rows model.todos) $ \row ->
    Gtk.listBoxRemove widgets.todoList row.actionRow

  -- Step 2: Create or change the rows, in the order of the todos. A new
  -- row goes to the index of its todo. The rows before it are already
  -- in their place, so the index is correct. The filter hides rows. It
  -- does not remove them.
  next <- for (zip [0 ..] (Map.toAscList model.todos)) $ \(index, (todoId, todo)) -> do
    row <- case Map.lookup todoId rows of
      Nothing -> insertRow dispatch widgets index model.filter todo
      Just row -> do
        refreshRow previous model todo row
        pure row
    pure (todoId, row)
  pure (Map.fromList next)

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

insertRow :: (Message -> IO ()) -> Widgets -> Int32 -> Filter -> Todo -> IO Row
insertRow dispatch widgets index activeFilter todo = do
  row <- todoRow dispatch todo
  Gtk.widgetSetVisible row.actionRow (matches activeFilter todo)
  Gtk.listBoxInsert widgets.todoList row.actionRow index
  pure row

-- | Set the properties of a row that differ between the previous model
-- and the new one. This is the 'changed' of 'render', one level down:
-- the field is a function of the model and of the todo of the row.
--
-- A set property emits the same signal as a click. The handler then
-- sends the value that the model already has, and 'update' changes
-- nothing. To set only what changed keeps most of these echoes from
-- occurring at all.
refreshRow :: Maybe Model -> Model -> Todo -> Row -> IO ()
refreshRow previous model todo row = do
  let changed :: (Eq a) => (Model -> Todo -> a) -> (a -> IO ()) -> IO ()
      changed field action = do
        let before = do
              old <- previous
              oldTodo <- Map.lookup todo.id old.todos
              pure (field old oldTodo)
        when (before /= Just (field model todo)) (action (field model todo))

  changed (\_ t -> t.title) (Adw.preferencesRowSetTitle row.actionRow)
  changed (\_ t -> t.done) (Gtk.checkButtonSetActive row.check)
  changed (\m t -> matches m.filter t) (Gtk.widgetSetVisible row.actionRow)

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
  pure Row {actionRow, check}

newBoxedList :: IO Gtk.ListBox
newBoxedList =
  new
    Gtk.ListBox
    [ #selectionMode := Gtk.SelectionModeNone
    , #cssClasses := ["boxed-list"]
    ]

toggleAllTooltip :: Bool -> Text
toggleAllTooltip True = "Mark all as not done"
toggleAllTooltip False = "Mark all as done"

filterGroup :: IO Adw.ToggleGroup
filterGroup = do
  group <- new Adw.ToggleGroup []
  forM_ allFilters $ \f -> do
    toggle <- new Adw.Toggle [#label := Text.show f]
    Adw.toggleGroupAdd group toggle
  pure group
