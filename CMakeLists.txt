cmake_minimum_required(VERSION 3.14)

project(LPE)

set(SRC_FILES
  src/automata.cpp 
  src/automata.h 
  src/lpeparse.cpp 
  src/lpeparse.h 
  src/lpexp.h 
  src/lpexp.cpp
  src/accum.cpp
  include/LPE/accum.h
  include/LPE/optautomata.h
)

add_library(LPE STATIC ${SRC_FILES})
target_include_directories(LPE PUBLIC include)

add_executable(accum_test tests/accum_test.cpp)
target_link_libraries(accum_test LPE)