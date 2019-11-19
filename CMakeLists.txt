cmake_minimum_required(VERSION 3.14)

project(LPE)

set(SRC_FILES
  src/automata.cpp 
  src/automata.h 
  src/lpeparse.cpp 
  src/lpeparse.h 
  src/lpexp.h 
  src/lpexp.cpp
  src/optautomata.h
  src/accum.cpp
  src/accum.h
)

add_library(LPE SHARED ${SRC_FILES})

add_executable(accum_test tests/accum_test.cpp ${SRC_FILES})
target_include_directories(accum_test PRIVATE src)