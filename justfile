[private]
default:
    @just --list

run:
    cabal run adwaita-todo

style:
    cabal-gild adwaita-todo.cabal
    fourmolu --mode inplace src app

build:
    cabal build all

