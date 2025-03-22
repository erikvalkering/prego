#pragma once

#include <prego/prego.h>

#include <boost/ut.hpp>

#include <string>
#include <tuple>
#include <vector>

using namespace std::string_literals;
using namespace boost::ut;

using prego::atom;
using prego::autorun;
using prego::calc;
using prego::global_scope_manager;
using prego::insertion_order_map;
using prego::make_atom;
using prego::scope_manager_t;

auto to_vector(auto &&rng) {
  using T = std::ranges::range_value_t<decltype(rng)>;

  auto r = std::vector<T>{};
  for (auto &&x : rng)
    r.push_back(x);

  return r;
}
