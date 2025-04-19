#include <boost/ut.hpp>

#include <prego/prego.h>

#include <optional>

using namespace boost::ut;

using prego::atom;

static suite<"mutations"> _ = [] {
  "mutation"_test = [] {
    atom x = 42;
    x = 1729; // direct mutation via assignment

    atom y = std::optional{42};
    y.emplace(1729); // direct mutation via emplace

    y.reset(); // indirect mutation via non-const member function
    expect(y == std::nullopt);

    atom z = std::vector{42};
    // z[0] = 1729; // indirect mutation via reference
  };
};
