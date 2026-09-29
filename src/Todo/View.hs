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
  forM_ (model.todos) $ \todo -> do
    row <- new Adw.ActionRow [#title := todo.title, #useMarkup := False]
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

  header <- new Adw.HeaderBar []
  toolbar <- new Adw.ToolbarView [#content := scrolled]
  Adw.toolbarViewAddTopBar toolbar header
  Adw.toolbarViewAddBottomBar toolbar footer
  Gtk.toWidget toolbar

newBoxedList :: IO Gtk.ListBox
newBoxedList =
  new
    Gtk.ListBox
    [ #selectionMode := Gtk.SelectionModeNone
    , #cssClasses := ["boxed-list"]
    ]
