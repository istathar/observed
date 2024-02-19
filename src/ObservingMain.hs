{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Main where

import Core.Program
import Core.Telemetry

version :: Version
version = $(fromPackage)

config :: Config
config =
    simpleConfig
        []

main :: IO ()
main = do
    context <- configure version None config

    context' <- initializeTelemetry [consoleExporter, structuredExporter, honeycombExporter] context

    executeWith context' program

program :: Program None ()
program = do
    info "Setting up"

    telemetry
        [ metric "build.version" (versionNumberFrom version)
        , metric "build.commit" (gitDescriptionFrom version)
        ]

    beginTrace $ do
        encloseSpan "The Top" $ do
            write "Hello World"
