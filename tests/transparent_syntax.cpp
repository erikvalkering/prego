#include "common.h"

static suite<"transparent syntax"> _ = [] {
  "test_implicit_conversions"_test = [] {
    atom missi = "Missi"s;
    calc faaiv = [] { return "Faaiv"s; };

    std::string m = missi;
    std::string f = faaiv;
  };
};
