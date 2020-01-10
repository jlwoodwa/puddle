Design Document
===============

The purpose of this document is to plan and describe Puddle's design.

Overview
--------

Puddle is a small imperative language. It will have variables, expressions,
assignment, and control statements. Later, functions and arrays might be added.

Semantics
---------

Puddle's semantics are entirely value-based; there is no such thing as a
reference. This has the benefit of making the language simpler and ruling out
many kinds of bugs in Puddle code. The needs that reference semantics meet in
other language will be met in other ways, such as allowing functions to return
structures.
