cmake_minimum_required(VERSION 3.16...3.23)

project(
  prego
  VERSION 0.0.1
  DESCRIPTION ""
  LANGUAGES CXX)

add_executable(prego prego.test.cpp)
target_compile_features(prego PRIVATE cxx_std_20)
