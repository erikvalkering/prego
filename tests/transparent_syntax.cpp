#include "common.h"

static suite<"transparent syntax"> _ = [] {
  "test_implicit_conversions"_test = [] {
    atom missi = "Missi"s;

    std::string m1 = missi;
  };
};
