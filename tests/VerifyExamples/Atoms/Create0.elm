module VerifyExamples.Atoms.Create0 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Atoms exposing (..)

type Metality
    = Metal
    | NonMetal

metality : AtomInfo Metality
metality =
    create
        (\atom ->
            let
                { period, group } =
                    get atom periodicTable
            in
            if
                (period == 1)
                    || (period <= 6)
                            && (group >= 10 + period)
            then
                NonMetal
            else
                Metal
        )



spec0 : Test.Test
spec0 =
    Test.test "#create: \n\n    get U metality\n    --> Metal" <|
        \() ->
            Expect.equal
                (
                get U metality
                )
                (
                Metal
                )