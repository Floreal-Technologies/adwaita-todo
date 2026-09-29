[private]
default:
    @just --list

style:
    cabal-gild lexorank.cabal
    fourmolu --mode inplace src test

build:
    cabal build all

