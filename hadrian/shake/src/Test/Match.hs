
-- | Test the rule matching facilities - alternatives, priority etc.
module Test.Match(main) where

import Development.Shake
import Test.Type


main = testBuild test $ do
    let output x file = writeFile' file x

    ["or*","*or"] |%> output ""

    alternatives $ do
        "alternative.t*" %> output "alternative.t*"
        "alternative.*" %> output "alternative.*"

    priority 100 $ priority 0 $ "priority.txt" %> output "100"
    priority 50 $ "priority.txt" %> output "50"

    alternatives $ do
        priority 20 $ "altpri.txt" %> output "20"
        priority 40 $ "altpri.txt" %> output "40"
    priority 30 $ "altpri.txt" %> output "30"

    alternatives $ do
        priority 21 $ "altpri2.txt" %> output "21"
        priority 22 $ "altpri2.txt" %> output "22"
    priority 23 $ "altpri2.txt" %> output "23"

    priority 55 $ alternatives $ "x" %> output "55"
    priority 51 $ "x" %> output "51"

    priority 42 $ alternatives $ "xx" %> output "42"
    priority 43 $ "xx" %> output "43"

    priority 10 $ do
        priority 6 $ "change" %> output "6"
        priority 7 $ "change" %> output "7"
        priority 8 $ "change" %> output "8"
    priority 9 $ "change" %> output "9"


test build = do
    build ["clean"]
    build ["or"]

    build ["alternative.foo","alternative.txt"]
    assertContents "alternative.foo" "alternative.*"
    assertContents "alternative.txt" "alternative.t*"

    build ["priority.txt"]
    assertContents "priority.txt" "100"

    build ["altpri.txt","altpri2.txt"]
    assertContents "altpri.txt" "30"
    assertContents "altpri2.txt" "23"

    build ["x","xx"]
    assertContents "x" "55"
    assertContents "xx" "43"

    assertException ["matches multiple rules","3"] $ build ["change","--quiet"]
