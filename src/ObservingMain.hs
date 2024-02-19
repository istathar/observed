{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Main where

import Control.Monad (forM_)
import Core.Program
import Core.System
import Core.Telemetry
import Core.Text
import System.Random (randomRIO)

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
                info "Beginning iteration"
                sleepThread 1
                write "Hello"

                encloseSpan "The Middle" $ do
                    sleepThread 1
                    write "World"

                    activity "The Bottom"

                telemetry
                    [ metric "loglevel" ("INFO" :: Rope)
                    , metric "iteration" i
                    ]

        --
        -- Now sleep a small amount of time before simulating the next event.
        --

        info "Sleeping"
        delay <- liftIO $ do
            randomRIO (1, 10 :: Int)
        debugS "delay" delay
        sleepThread (fromIntegral delay)

activity :: Rope -> Program None ()
activity label = do
    encloseSpan label $ do
        info "Performing activity"
        raceThreads_
            ( do
                delay <- liftIO $ do
                    randomRIO (4.0, 6.0 :: Float)
                debugS "working" delay
                sleepThread (toRational delay)

                write "Goodbye"
                telemetry
                    [ metric "loglevel" ("INFO" :: Rope)
                    ]
            )
            ( do
                delay <- liftIO $ do
                    randomRIO (0.0, 10.0 :: Float)
                debugS "kabooma" delay
                sleepThread (toRational delay)

                let e = ActivityFailed
                telemetry
                    [ metric "error" (show e)
                    , metric "loglevel" ("ERROR" :: Rope)
                    ]
                throw e
            )

data StepProblem = ActivityFailed
    deriving (Show)

instance Exception StepProblem
