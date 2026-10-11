module Todo.Runtime where

import Control.Monad
import Data.IORef (newIORef, readIORef, writeIORef)
import GI.Adw qualified as Adw
import GI.GLib qualified as GLib
import GI.Gtk qualified as Gtk

import Todo.Model qualified as Model
import Todo.View qualified as View

run :: Adw.Application -> IO ()
run app = do
  ref <- newIORef Model.init
  widgets <- View.build app
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
        View.render dispatch widgets (Just oldModel) newModel

  View.connect dispatch widgets
  View.render dispatch widgets Nothing Model.init
  Gtk.windowPresent widgets.window
