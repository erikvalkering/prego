#pragma once

#include <prego/prego.h>

#include <boost/ut.hpp>

#include <tuple>
#include <vector>

using namespace boost::ut;

using prego::insertion_order_map;

auto to_vector(auto &&rng) {
  using T = std::ranges::range_value_t<decltype(rng)>;

  auto r = std::vector<T>{};
  for (auto &&x : rng)
    r.push_back(x);

  return r;
}

#define CONCAT2(a, b) a##b
#define CONCAT(a, b) CONCAT2(a, b)
#define _ CONCAT(placeholder_, __LINE__)
