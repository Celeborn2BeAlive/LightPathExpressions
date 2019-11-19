cmake_minimum_required(VERSION 3.14)

project(LPE)

set(SRC_FILES
  automata.cpp 
  automata.h 
  lpeparse.cpp 
  lpeparse.h 
  lpexp.h 
  lpexp.cpp
  optautomata.h
  accum.cpp
  accum.h
)

add_library(LPE SHARED ${SRC_FILES})

add_executable(accum_test accum_test.cpp ${SRC_FILES})