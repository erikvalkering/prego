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

cpmaddpackage("gh:fmtlib/fmt#11.0.1")
cpmaddpackage("gh:boost-ext/ut#v2.3.0")

set(clang18 "$<AND:$<CXX_COMPILER_ID:Clang>,$<CXX_COMPILER_VERSION:18>>")
set(clang19 "$<AND:$<CXX_COMPILER_ID:Clang>,$<CXX_COMPILER_VERSION:19>>")

add_library(prego INTERFACE)
target_include_directories(prego INTERFACE include)

add_executable(sandbox examples/sandbox.cpp)
target_compile_features(sandbox PRIVATE cxx_std_23)
target_link_libraries(sandbox PRIVATE prego)

add_executable(test_prego
  tests/prego.test.cpp
  tests/basics.cpp
  tests/lifetimes.cpp
  tests/syntaxes.cpp
  tests/insertion_order_map.cpp
  tests/graph_traversal.cpp
  tests/types.cpp
  tests/transparent_syntax.cpp
  tests/integration_tests.cpp
  tests/mutations.cpp
)
target_compile_features(test_prego PRIVATE cxx_std_23)
target_link_libraries(test_prego PRIVATE prego)
target_link_libraries(test_prego PRIVATE fmt::fmt)
target_link_libraries(test_prego PRIVATE ut)
target_compile_definitions(test_prego PRIVATE BOOST_UT_DISABLE_MODULE)
target_compile_options(test_prego PRIVATE
  $<${clang18}:-fsanitize=address -fsanitize=undefined -static-libasan>
  $<${clang19}:-fsanitize=address -fsanitize=undefined -static-libsan>
  $<$<CXX_COMPILER_ID:MSVC>:/bigobj>
)
add_test(NAME prego_test_suite COMMAND test_prego)

