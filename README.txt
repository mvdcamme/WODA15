This repository contains an implementation of the formal framework proposed in the paper “A Formal Foundation for Trace-Based JIT Compilers” and presented in WODA 2015.
The formal semantics on which this implementation is based may be uploaded at a later point.

Currently, the implementation consists of just one interpreter machine: an interpreter for a subset of Scheme.
To execute a Scheme program in this framework, load “evaluator.scm” and run the start function; you can then enter the program you wish to run.

Some 20+ test-programs have been uploaded to this repository as well.
The tests can be run by loading “tests/testing.rkt” and calling run-test on the path to the test program you wish to run, e.g. executing (run-test fac-test-path) will run the faculty-test in the framework.
All test paths are defined in the module “test-paths.rkt”.