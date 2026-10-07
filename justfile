[private]
default:
    @just --list

run:
    cabal run adwaita-todo

style:
    cabal-gild lexorank.cabal
    fourmolu --mode inplace src test

build:
    cabal build all

