module Main (main) where

import Data.GI.Base (AttrOp (..), new, on)
import GI.Adw qualified as Adw
import GI.Gio qualified as Gio

import Todo.Runtime qualified as Runtime

main :: IO ()
main = do
  app <- new Adw.Application [#applicationId := "tech.floreal.AdwaitaTodo"]
  on app #activate (Runtime.run app)
  Gio.applicationRun app Nothing
  pure ()
