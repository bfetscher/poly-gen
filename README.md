poly-gen
========

A couple scripts to generate Haskell modules for testing GHC testing.
(Based on on language in "Testing an Optimising Compiler by Generating 
Random Lambda Terms" [Palka et al, 2011].)

To use, from the command line:
racket gen-module <number of test expressions>

It is not fast, so use a small number!
