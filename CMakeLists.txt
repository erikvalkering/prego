cmake_minimum_required(VERSION 3.16...3.23)

project(
  prego
  VERSION 0.0.1
  DESCRIPTION ""
  LANGUAGES CXX)

include(FetchContent)

FetchContent_Declare(
  fmt
  GIT_REPOSITORY https://github.com/fmtlib/fmt
  GIT_TAG        b50e685) # 11.0.1
FetchContent_MakeAvailable(fmt)

add_executable(prego prego.test.cpp)
target_compile_features(prego PRIVATE cxx_std_20)
target_link_libraries(prego PRIVATE fmt::fmt)
