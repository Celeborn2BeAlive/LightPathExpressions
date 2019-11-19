# Light Path Expressions Library

## Introduction

This is code extracted from [Open Shading Language](https://github.com/imageworks/OpenShadingLanguage) library to handle light path expressions parsing in the context of light transport simulation.

## Current State

For this first iteration the goal was to:
- keep the code as close as possible to the one from OSL
- make it compile
- make the test "accum_test.cpp" works (provided in OSL source code)
- don't spent too much time on that

To make it compile without dependencies to other parts of OSL and OpenImageIO I did the following modifications:
- Replace OIIO::ustring with std::string
- Replace OIIO::Strutil::format with std::to_string
- Take Labels definitions from oslclosure.h/.cpp
- Replace ASSERT with assert
- Remove OSLEXECPUBLIC macro usage
- Replace OSL namespace with LPE
- Remove OSL_NAMESPACE_ENTER macro usage

And to make the test pass I had to change string comparisons since the code is now using std::string instead of ustring.

## Future Devs / Investigations

### More efficient string representation

I replaced ustring with std::string mainly to be fast on the first iteration. I'm aware this is not efficient at all so I plan to go back to some kind of unique strings representation, but simpler than the one from OIIO (not planning on providing thread safety, offer an API to predeclare every LPE label and get back its ustring id).

### Provide a C API

The initial motivation for this code extraction is to integrate LPEs to Blender Cycles CPU and GPU so I need a C API.

I might even convert the whole code to C to simplify its usage and integration in any project (maybe making something like stb_* libs, one header and one C file).

### Study the usage of the LPE automata on GPU

As said previously the goal is to use the automata both for the CPU and GPU implementation of Cycles, so I need to ensure that it can be used or modify its representation.

### More and better automated unit tests

Right now tests are in tests/accum_test.cpp that was already in OSL source code. Maybe we need more, and an integration in the build process.

## Code metrics

Some metrics extracted from the source code to evaluate its complexity:

Number of lines in each file:
- 300 ./include/LPE/accum.h
- 94  ./include/LPE/optautomata.h
- 244 ./src/accum.cpp
- 661 ./src/automata.cpp
- 337 ./src/automata.h
- 464 ./src/lpeparse.cpp
- 149 ./src/lpeparse.h
- 220 ./src/lpexp.cpp
- 209 ./src/lpexp.h
- 197 ./tests/accum_test.cpp

Total number of lines: 2875