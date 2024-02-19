{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

    --
    -- Establish global context we want to be on every span.
    --

    telemetry
        [ metric "build.version" (versionNumberFrom version)
        , metric "build.commit" (gitDescriptionFrom version)
        , metric "level" ("INFO" :: Rope)
        ]

    --
    -- Loop forever.
    --

    forM_ [(1 :: Int) ..] $ \i -> do
        --
        -- Simulate an event
        --

        beginTrace $ do
            encloseSpan "The Top" $ do
                catch
                    ( do
                        info "Beginning iteration"
                        sleepThread 1
                        write "Hello"

                        encloseSpan "The Middle" $ do
                            sleepThread 1
                            write "World"

                            activity
                                "The Bottom"
                                (randomNap (4.0, 6.0))
                    )
                    ( \(_ :: StepProblem) -> do
                        warn "Activity failed"
                    )

                telemetry
                    [ metric "level" ("INFO" :: Rope)
                    , metric "iteration" i
                    ]

        --
        -- Sleep a small amount of time before simulating the next event.
        --

        info "Sleeping"
        delay <- liftIO $ do
            randomRIO (1.0, 5.0 :: Float)
        debugS "delay" delay
        sleepThread (toRational delay)

activity :: Rope -> Program None () -> Program None ()
activity label subprogram = do
    encloseSpan label $ do
        info "Performing activity"
        raceThreads_
            ( do
                subprogram
            )
            ( do
                randomFailure (0.0, 10.0)
            )

data StepProblem = ActivityFailed
    deriving (Show)

instance Exception StepProblem

randomNap :: (Float, Float) -> Program None ()
randomNap range = do
    delay <- liftIO $ do
        randomRIO range
    debugS "work" delay
    sleepThread (toRational delay)

randomFailure :: (Float, Float) -> Program None ()
randomFailure range = do
    delay <- liftIO $ do
        randomRIO range
    debugS "boom" delay
    sleepThread (toRational delay)

    let e = ActivityFailed
    telemetry
        [ metric "error" (show e)
        , metric "level" ("ERROR" :: Rope)
        ]
    throw e
