#include "common.h"

static suite<"transparent syntax"> _ = [] {
  "comparisons"_test = [] {
    atom x = 42;

    calc y1 = x <=> 1729;
    calc y2 = x == 1729;
    calc y3 = x > 1729;
    calc y4 = x < 1729;
    calc y5 = x >= 1729;
    calc y6 = x <= 1729;

    auto y7 = bool{x > 1729};

    calc y8 = 1729 <=> x;
    calc y9 = 1729 == x;
    calc yA = 1729 > x;
    calc yB = 1729 < x;
    calc yC = 1729 >= x;
    calc yD = 1729 <= x;

    auto yE = bool{1729 > x};

    atom y = 1729;

    calc z1 = x <=> y;
    calc z2 = x == y;
    calc z3 = x > y;
    calc z4 = x < y;
    calc z5 = x >= y;
    calc z6 = x <= y;

    auto z7 = bool{x > y};

    calc w1 = [=] { return x == y; };
    calc w2 = [=] {
      auto z = x == y;
      return z;
    };
    calc w3 = [=] {
      auto z = x == y;
      return z ? true : false;
    };
    expect(w1 == false);
    expect(w2 == false);
    expect(w3 == false);
  };

  "implicit_conversions"_test = [] {
    atom missi = "Missi"s;
    calc faaiv = [] { return "Faaiv"s; };

    std::string m = missi;
    std::string f = faaiv;
  };

  "get_result_t_atom"_test = [] {
    using prego::get_result_t;

    atom missi = "Missi"s;

    // simple lambdas
    auto a1 = missi;
    auto a2 = [=] { return missi; };
    auto a3 = [=] { return missi(); };
    auto a4 = [=] { return "Missi"s; };

    static_assert(std::same_as<decltype(get_result_t(a1)), std::string>);
    static_assert(std::same_as<decltype(get_result_t(a2)), std::string>);
    static_assert(std::same_as<decltype(get_result_t(a3)), std::string>);
    static_assert(std::same_as<decltype(get_result_t(a4)), std::string>);

    // lambdas with get
    auto a5 = [=](auto) { return missi; };
    auto a6 = [=](auto) { return missi(); };
    auto a7 = [=](auto get) { return get(missi); };
    auto a8 = [=](auto) { return "Missi"s; };

    static_assert(std::same_as<decltype(get_result_t(a5)), std::string>);
    static_assert(std::same_as<decltype(get_result_t(a6)), std::string>);
    static_assert(std::same_as<decltype(get_result_t(a7)), std::string>);
    static_assert(std::same_as<decltype(get_result_t(a8)), std::string>);

    // simple calc
    calc c1 = missi;
    calc c2 = [=] { return missi; };
    calc c3 = [=] { return missi(); };
    calc c4 = [=] { return "Missi"s; };

    static_assert(std::same_as<decltype(get_result_t(c1)), std::string>);
    static_assert(std::same_as<decltype(get_result_t(c2)), std::string>);
    static_assert(std::same_as<decltype(get_result_t(c3)), std::string>);
    static_assert(std::same_as<decltype(get_result_t(c4)), std::string>);

    // calc with get
    calc c5 = [=](auto) { return missi; };
    calc c6 = [=](auto) { return missi(); };
    calc c7 = [=](auto get) { return get(missi); };
    calc c8 = [=](auto) { return "Missi"s; };

    static_assert(std::same_as<decltype(get_result_t(c5)), std::string>);
    static_assert(std::same_as<decltype(get_result_t(c6)), std::string>);
    static_assert(std::same_as<decltype(get_result_t(c7)), std::string>);
    static_assert(std::same_as<decltype(get_result_t(c7)), std::string>);
  };

  "get_result_t_calc"_test = [] {
    using prego::get_result_t;

    calc missi = [] { return "Missi"s; };
    calc faaiv = [](auto) { return "Faaiv"s; };

    auto a1 = missi;
    auto a2 = [=] { return missi; };
    auto a3 = [=] { return missi(); };
    auto a4 = [=] { return "Missi"s; };
    auto a5 = faaiv;
    auto a6 = [=] { return faaiv; };
    auto a7 = [=] { return faaiv(); };
    auto a8 = [=] { return "Faaiv"s; };

    static_assert(std::same_as<decltype(get_result_t(a1)), std::string>);
    static_assert(std::same_as<decltype(get_result_t(a2)), std::string>);
    static_assert(std::same_as<decltype(get_result_t(a3)), std::string>);
    static_assert(std::same_as<decltype(get_result_t(a4)), std::string>);
    static_assert(std::same_as<decltype(get_result_t(a5)), std::string>);
    static_assert(std::same_as<decltype(get_result_t(a6)), std::string>);
    static_assert(std::same_as<decltype(get_result_t(a7)), std::string>);
    static_assert(std::same_as<decltype(get_result_t(a8)), std::string>);

    auto a9 = [=](auto) { return missi; };
    auto aA = [=](auto get) { return get(missi); };
    auto aB = [=](auto) { return "Missi"s; };
    auto aC = [=](auto) { return faaiv; };
    auto aD = [=](auto get) { return get(faaiv); };
    auto aE = [=](auto) { return "Faaiv"s; };

    static_assert(std::same_as<decltype(get_result_t(a9)), std::string>);
    static_assert(std::same_as<decltype(get_result_t(aA)), std::string>);
    static_assert(std::same_as<decltype(get_result_t(aB)), std::string>);
    static_assert(std::same_as<decltype(get_result_t(aC)), std::string>);
    static_assert(std::same_as<decltype(get_result_t(aD)), std::string>);
    static_assert(std::same_as<decltype(get_result_t(aE)), std::string>);

    calc c1 = missi;
    calc c2 = [=] { return missi; };
    calc c3 = [=] { return missi(); };
    calc c4 = [=] { return "Missi"s; };
    calc c5 = faaiv;
    calc c6 = [=] { return faaiv; };
    calc c7 = [=] { return faaiv(); };
    calc c8 = [=] { return "Faaiv"s; };

    static_assert(std::same_as<decltype(get_result_t(c1)), std::string>);
    static_assert(std::same_as<decltype(get_result_t(c2)), std::string>);
    static_assert(std::same_as<decltype(get_result_t(c3)), std::string>);
    static_assert(std::same_as<decltype(get_result_t(c4)), std::string>);
    static_assert(std::same_as<decltype(get_result_t(c5)), std::string>);
    static_assert(std::same_as<decltype(get_result_t(c6)), std::string>);
    static_assert(std::same_as<decltype(get_result_t(c7)), std::string>);
    static_assert(std::same_as<decltype(get_result_t(c8)), std::string>);

    calc c9 = [=] { return missi; };
    calc cA = [=](auto get) { return get(missi); };
    calc cB = [=](auto) { return "Missi"s; };
    calc cC = [=] { return faaiv; };
    calc cD = [=](auto get) { return get(faaiv); };
    calc cE = [=](auto) { return "Faaiv"s; };

    static_assert(std::same_as<decltype(get_result_t(c9)), std::string>);
    static_assert(std::same_as<decltype(get_result_t(cA)), std::string>);
    static_assert(std::same_as<decltype(get_result_t(cB)), std::string>);
    static_assert(std::same_as<decltype(get_result_t(cC)), std::string>);
    static_assert(std::same_as<decltype(get_result_t(cD)), std::string>);
    static_assert(std::same_as<decltype(get_result_t(cE)), std::string>);
  };

  "derived_from_magix_mixin"_test = [] {
    static_assert(prego::derived_from<atom<int>, prego::magic_mixin>);
    static_assert(prego::derived_from<const atom<int>, prego::magic_mixin>);
    static_assert(prego::derived_from<atom<int> &, prego::magic_mixin>);
    static_assert(prego::derived_from<const atom<int> &, prego::magic_mixin>);
    static_assert(prego::derived_from<atom<int> &&, prego::magic_mixin>);
    static_assert(prego::derived_from<const atom<int> &&, prego::magic_mixin>);
  };

  "get_value_from_param"_test = [] {
    auto x = 42;
    atom y = 42;
    calc z = [] { return 42; };
    auto w = prego::magic_wrapper{[] { return 42; }};

    expect(x == 42_c);
    expect(prego::get_value_from_param(x) == 42_c);
    expect(prego::get_value_from_param(std::move(x)) == 42_c);

    expect(y() == 42_c);
    expect(prego::get_value_from_param(y) == 42_c);
    expect(prego::get_value_from_param(std::move(y)) == 42_c);

    expect(z() == 42_c);
    expect(prego::get_value_from_param(z) == 42_c);
    expect(prego::get_value_from_param(std::move(z)) == 42_c);

    expect(w() == 42_c);
    expect(prego::get_value_from_param(w) == 42_c);
    expect(prego::get_value_from_param(std::move(w)) == 42_c);

    static_assert(
        std::same_as<decltype(prego::get_value_from_param(x)), int &>);
    static_assert(
        std::same_as<decltype(prego::get_value_from_param(y)), const int &>);
    static_assert(
        std::same_as<decltype(prego::get_value_from_param(z)), const int &>);
    static_assert(std::same_as<decltype(prego::get_value_from_param(w)), int>);

    static_assert(
        std::same_as<decltype(prego::get_value_from_param(std::move(x))),
                     int &&>);
    static_assert(
        std::same_as<decltype(prego::get_value_from_param(std::move(y))),
                     const int &>);
    static_assert(
        std::same_as<decltype(prego::get_value_from_param(std::move(z))),
                     const int &>);
    static_assert(
        std::same_as<decltype(prego::get_value_from_param(std::move(w))), int>);
  };

  "string_concatenation"_test = [] {
    atom missi = "Missi"s;
    atom faaiv = "Faaiv"s;

    expect(missi + faaiv == "MissiFaaiv");
    expect(missi + "Faaiv" == "MissiFaaiv");
    expect("Missi" + faaiv == "MissiFaaiv");

    calc c1 = missi + faaiv;
    calc c2 = missi + "Faaiv";
    calc c3 = "Missi" + faaiv;
    expect(c1 == "MissiFaaiv");
    expect(c2 == "MissiFaaiv");
    expect(c3 == "MissiFaaiv");

    calc c4 = [=] { return missi + faaiv; };
    calc c5 = [=] { return missi + "Faaiv"; };
    calc c6 = [=] { return "Missi" + faaiv; };
    expect(c4 == "MissiFaaiv");
    expect(c5 == "MissiFaaiv");
    expect(c6 == "MissiFaaiv");
  };

  "string_size"_test = [] {
    atom missi = "Missi"s;
    atom faaiv = "Faaiv"s;

    calc c1 = [=] { return (missi() + faaiv()).size(); };
    calc c2 = [=] { return (missi + faaiv).size(); };
    calc c3 = (missi + faaiv).size();

    expect(c1 == 10);
    expect(c2 == 10);
    expect(c3 == 10);
  };

  "optional"_test = [] {
    atom x = std::optional<int>{42};
    atom y = std::optional<int>{};
    atom z = 1729;
    calc w = [] { return 1729; };

    expect(x.value_or(1729) == 42);
    expect(y.value_or(1729) == 1729);

    expect(y.value_or(z) == 1729);
    expect(y.value_or(w) == 1729);

    calc c = y.value_or(w);
    expect(c == 1729);

    x.reset();
    expect(x == std::nullopt);
  };

  "optional_string"_test = [] {
    atom x = std::optional{"Missi"s};
    atom y = std::optional<std::string>{};
    atom z = "Faaiv"s;
    calc w = [] { return "Faaiv"s; };

    expect(x.value_or("Faaiv"s) == "Missi"s);
    expect(y.value_or("Faaiv"s) == "Faaiv"s);

    expect(y.value_or(z) == "Faaiv"s);
    expect(y.value_or(w) == "Faaiv"s);

    calc c = y.value_or(w);
    expect(c == "Faaiv"s);
  };

  "transparent_comparisons"_test = [] {
    auto x = std::optional{"Missi"s};
    atom y = std::optional{"Missi"s};

    expect(x.value_or("Faaiv"s) == "Missi"s);
    expect(x.value_or("Faaiv") == "Missi");
    expect(y.value_or("Faaiv"s) == "Missi"s);
    expect(y.value_or("Faaiv") == "Missi");
  };

  "format"_test = [=] {
    atom missi = "Missi"s;
    expect(std::format("{}", missi) == "Missi");

    calc faaiv = [] { return "Faaiv"; };
    expect(std::format("{}", faaiv) == "Faaiv");

    expect(std::format("{}", missi + faaiv) == "MissiFaaiv");
  };

  "capture_by_value"_test = [] {
    bool destroyed = false;
    struct mock {
      mutable int copies = 0;
      bool *destroyed = nullptr;
      mock(bool *destroyed = nullptr) : destroyed{destroyed} {}

      mock(const mock &rhs) { ++rhs.copies; }
      mock(mock &&) = default;
      mock &operator=(const mock &) = default;
      mock &operator=(mock &&) = default;

      ~mock() {
        if (destroyed)
          *destroyed = true;
      }
      bool operator==(const mock &) const = default;
    };

    auto x = mock{};

    auto y = [&] {
      atom z = mock{&destroyed};
      destroyed = false;
      return x == z;
    }();

    expect(x.copies == 1_c);
    expect(not destroyed);
  };

  "calc_from_atom_lifetime_issue"_test = [] {
    atom x = 42;
    calc y = x;

    // Executing this autorun triggered a bug in the assignment of an atom to
    // the previous calc
    auto r = autorun([=] { y(); }, nullptr);
  };
};
