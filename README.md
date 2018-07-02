# ruleDB

The motivation for this repo is identical to [dbgen](https://github.com/kris-brown/dbgen),
but implemented in Haskell instead. The project was originally developed here
before the highly-dynamic nature of the problem (very little is known at
compile time) made Python a more reasonable language for development.

TemplateHaskell is currently used to compromise between static typing and
runtime-declared data structures. At runtime, TH reads an input file and
recompiles the relevant portions of the code. 
