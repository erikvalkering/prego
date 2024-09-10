cmake_minimum_required(VERSION 3.16...3.23)

project(
  prego
  VERSION 0.0.1
  DESCRIPTION ""
  LANGUAGES CXX)

enable_testing()

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

include(FetchContent)

FetchContent_Declare(
  fmt
  GIT_REPOSITORY      https://github.com/fmtlib/fmt
  GIT_TAG             b50e685 # 11.0.1
  UPDATE_DISCONNECTED 1
)
FetchContent_MakeAvailable(fmt)

FetchContent_Declare(
  ut
  GIT_REPOSITORY      https://github.com/boost-ext/ut
  GIT_TAG             6a4dedd # 2.1.0
  PATCH_COMMAND       git apply ${CMAKE_CURRENT_SOURCE_DIR}/boostext.ut.6a4dedd.patch # replaces std::format with fmt::format due to lack thereof in Termux
  UPDATE_DISCONNECTED 1
)
FetchContent_MakeAvailable(ut)

add_executable(test_prego prego.test.cpp)
target_compile_features(test_prego PRIVATE cxx_std_23)
target_link_libraries(test_prego PRIVATE fmt::fmt)
target_link_libraries(test_prego PRIVATE ut)
target_compile_definitions(test_prego PRIVATE BOOST_UT_DISABLE_MODULE)

add_test(NAME prego_test_suite COMMAND test_prego)

