# README #

Labs for "TDA452 - Functional Programming" course

## Quick Setup ##

### Start Haskell ###
If you have installed the Haskell Platform, open a terminal and type ghci (the name of the executable of the GHC interpreter) at the command prompt. Alternatively, if you are on Windows, you may choose WinGHCi in the Start menu.
```
$ ghci
    GHCi, version 6.12.3: http://www.haskell.org/ghc/  :? for help
    Loading package base ... linking ... done.
    Prelude>
```

### Using the GHCi ###
If you already have created a file called example.hs in Haskell you can use :l and :r as abbreviations for :load and :reload.
```
Prelude>  :l example.hs
[1 of 1] Compiling Main             ( example.hs, interpreted )
Ok, modules loaded: Main.
*Main>
```
and you can use :reload to reload the file(s) after changes.

## Lab assignment 1: the power function ##

In this lab assignment, we implemented the well-known "power" function in two different new ways. The power function takes two arguments n and k and computes n^k (works only for non-negative k).

## Lab Assignment 2: BlackJack ##

In this lab assignment, we implemented a Haskell program for playing a card game called BlackJack.
