{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Main where

import Control.Monad (forM_)
import Core.Program
import Core.Telemetry
import Core.Text

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

    forM_ [(1 :: Int) ..] $ \i -> do
        beginTrace $ do
            encloseSpan "The Top" $ do
                sleepThread 1
                write "Hello"

                encloseSpan "The Middle" $ do
                    sleepThread 1
                    write "World"

                    encloseSpan "The Bottom" $ do
                        sleepThread 1
                        write "Goodbye"

                telemetry
                    [ metric "level" ("INFO" :: Rope)
                    , metric "iteration" i
                    ]
