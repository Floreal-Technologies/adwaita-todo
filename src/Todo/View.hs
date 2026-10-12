module Todo.View where

import Control.Monad (forM_, when)
import Data.GI.Base
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import GI.Adw qualified as Adw
import GI.Gtk qualified as Gtk
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)

import Todo.Model

data Row = Row Adw.ActionRow Gtk.CheckButton

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

  -- The rows are still built again on each render.
  Gtk.listBoxRemoveAll widgets.todoList
  forM_ (visibleTasks model) $ \todo -> do
    row <- todoRow dispatch todo
    Gtk.listBoxAppend widgets.todoList row

todoRow
  :: (Message -> IO ())
  -> Todo
  -> IO Adw.ActionRow
todoRow dispatch todo = do
  check <- new Gtk.CheckButton [#active := todo.done]
  on check #toggled $ do
    active <- Gtk.checkButtonGetActive check
    dispatch (SetDoneStatus todo.id active)
  delete <-
    new
      Gtk.Button
      [ #iconName := "user-trash-symbolic"
      , #tooltipText := "Delete"
      , #cssClasses := ["flat"]
      ]
  on delete #clicked (dispatch (Delete todo.id))
  row <- new Adw.ActionRow [#title := todo.title, #useMarkup := False]
  Adw.actionRowAddSuffix row check
  Adw.actionRowAddSuffix row delete
  pure row

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
