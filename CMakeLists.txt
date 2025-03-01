cmake_minimum_required(VERSION 3.16...3.23)

project(
  prego
  VERSION 0.0.1
  DESCRIPTION ""
  LANGUAGES CXX)

enable_testing()

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

include(cmake/CPM.cmake)

CPMAddPackage("gh:fmtlib/fmt#11.0.1")
CPMAddPackage("gh:boost-ext/ut#v2.3.0")

add_executable(test_prego prego.test.cpp)
target_compile_features(test_prego PRIVATE cxx_std_23)
target_link_libraries(test_prego PRIVATE fmt::fmt)
target_link_libraries(test_prego PRIVATE ut)
target_compile_definitions(test_prego PRIVATE BOOST_UT_DISABLE_MODULE)
target_compile_options(test_prego PRIVATE
#  $<$<CXX_COMPILER_ID:GNU>:-fsanitize=address -fsanitize=undefined -static-libasan>
  $<$<AND:$<CXX_COMPILER_ID:Clang>,$<CXX_COMPILER_VERSION:18>>:-fsanitize=address -fsanitize=undefined -static-libasan>
  $<$<AND:$<CXX_COMPILER_ID:Clang>,$<CXX_COMPILER_VERSION:19>>:-fsanitize=address -fsanitize=undefined -static-libsan>
)
add_test(NAME prego_test_suite COMMAND test_prego)

