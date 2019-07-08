{-# LANGUAGE OverloadedStrings, TypeFamilies, DataKinds,
  DeriveGeneric, TypeOperators #-} --add these to package.yaml

module Main where

import App
import DBstuff


main :: IO ()
main = do
  runDBstuff --creates new sqlite database
  run