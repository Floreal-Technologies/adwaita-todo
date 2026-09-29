module Todo.Runtime where

import Control.Monad
import Data.GI.Base (AttrOp (..), new)
import Data.IORef (newIORef, readIORef, writeIORef)
import GI.Adw qualified as Adw
import GI.GLib qualified as GLib
import GI.Gtk qualified as Gtk

import Todo.Model qualified as Model
import Todo.View qualified as View

run :: Adw.Application -> IO ()
run app = do
  window <-
    new
      Adw.ApplicationWindow
      [ #application := app
      , #title := "Todos"
      , #defaultWidth := 480
      , #defaultHeight := 640
      ]
  ref <- newIORef Model.init
  let {- rec -}
      dispatch :: Model.Message -> IO ()
      dispatch message =
        void $ GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
          step message
          pure GLib.SOURCE_REMOVE

      step :: Model.Message -> IO ()
      step message = do
        oldModel <- readIORef ref
        let (newModel, _effects) = Model.update message oldModel
        writeIORef ref newModel
        content <- View.view dispatch newModel
        Adw.applicationWindowSetContent window (Just content)

  content <- View.view dispatch Model.init
  Adw.applicationWindowSetContent window (Just content)
  Gtk.windowPresent window
