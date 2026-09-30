module Todo.View where

import Control.Monad (forM_)
import Data.GI.Base
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import GI.Adw qualified as Adw
import GI.Gtk qualified as Gtk

import Todo.Model

view
  :: (Message -> IO ())
  -> Model
  -> IO Gtk.Widget
view dispatch model = do
  entry <- new Adw.EntryRow [#title := "New task"]
  on entry #entryActivated $ do
    text <- Gtk.editableGetText entry
    dispatch (Add text)
  entryBox <- newBoxedList
  Gtk.listBoxAppend entryBox entry

  todoList <- newBoxedList
  forM_ (visibleTasks model) $ \todo -> do
    row <- todoRow dispatch todo
    Gtk.listBoxAppend todoList row

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

  count <- new Gtk.Label [#label := Text.show (Map.size model.todos)]
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
  Adw.toggleGroupSetActiveName filters (Just (Text.show model.filter))
  on filters (PropertyNotify #activeName) $ \_ -> do
    name <- Adw.toggleGroupGetActiveName filters
    forM_ (name >>= parseFilter) $ \newFilter ->
      dispatch (SetFilter newFilter)

  header <-
    new
      Adw.HeaderBar
      [ #titleWidget := filters
      ]
  toolbar <- new Adw.ToolbarView [#content := scrolled]
  Adw.toolbarViewAddTopBar toolbar header
  Adw.toolbarViewAddBottomBar toolbar footer
  Gtk.toWidget toolbar

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
