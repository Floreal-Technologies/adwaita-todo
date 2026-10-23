module Todo.Runtime where

import Control.Monad
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as Map
import GI.Adw qualified as Adw
import GI.GLib qualified as GLib
import GI.Gtk qualified as Gtk

import Todo.Model qualified as Model
import Todo.View qualified as View

run :: Adw.Application -> IO ()
run app = do
  widgets <- View.build app
  -- The runtime owns all the state: the model, and the rows that
  -- show its todos.
  modelRef <- newIORef Model.init
  rowsRef <- newIORef Map.empty
  let {- rec -}
      dispatch :: Model.Message -> IO ()
      dispatch message =
        void $ GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
          step message
          pure GLib.SOURCE_REMOVE

      step :: Model.Message -> IO ()
      step message = do
        oldModel <- readIORef modelRef
        oldRows <- readIORef rowsRef
        let newModel = Model.update message oldModel
        newRows <- View.render dispatch widgets oldRows (Just oldModel) newModel
        writeIORef modelRef newModel
        writeIORef rowsRef newRows

  View.connect dispatch widgets
  rows <- View.render dispatch widgets Map.empty Nothing Model.init
  writeIORef rowsRef rows
  Gtk.windowPresent widgets.window
