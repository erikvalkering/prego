#include <boost/ut.hpp>

#include <prego/prego.h>

using namespace boost::ut;
using namespace std::string_literals;

using prego::atom;
using prego::autorun;
using prego::calc;
using prego::spy;

static suite<"utilities_tests"> _ = [] {
  "spy_calc"_test = [] {
    auto z = false;
    calc a = [] { return 1729; } + spy([&] { z = true; });
    a();
    expect(that % z == true);
  };

  "spy_calc_expression"_test = [] {
    atom a = 42;
    auto z = false;
    calc b = a + 1729 + spy([&] { z = true; });
    b();
    expect(that % z == true);
  };

  "spy_autorun"_test = [] {
    atom a = 42;
    auto z = false;
    autorun([=] { [[maybe_unused]] auto x = int{a + 1729}; } +
            spy([&] { z = true; }));
    expect(that % z == true);
  };
};
