cmake_minimum_required(VERSION 3.16)

project(LPE)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

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

add_executable(lpe_tests tests/lpe_tests.cpp)
target_link_libraries(lpe_tests LPE)
target_include_directories(lpe_tests PUBLIC src)