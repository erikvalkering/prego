#include "common.h"

static suite<"transparent syntax"> _ = [] {
  "test_comparisons"_test = [] {
    atom x = 42;

    calc y1 = x <=> 1729;
    calc y2 = x == 1729;
    calc y3 = x > 1729;
    calc y4 = x < 1729;
    calc y5 = x >= 1729;
    calc y6 = x <= 1729;

    auto y7 = bool{x > 1729};

    atom y = 1729;

    calc z1 = x <=> y;
    calc z2 = x == y;
    calc z3 = x > y;
    calc z4 = x < y;
    calc z5 = x >= y;
    calc z6 = x <= y;

    auto z7 = bool{x > y};
  };

  "test_implicit_conversions"_test = [] {
    atom missi = "Missi"s;
    calc faaiv = [] { return "Faaiv"s; };

    std::string m = missi;
    std::string f = faaiv;
  };

  "test_get_result_t_atom"_test = [] {
    using prego::get_result_t;

    atom missi = "Missi"s;

    auto a1 = missi;
    auto a2 = [=] { return missi; };
    auto a3 = [=] { return missi(); };
    auto a4 = [=] { return "Missi"s; };

    static_assert(std::same_as<decltype(get_result_t(a1)), std::string>);
    static_assert(std::same_as<decltype(get_result_t(a2)), std::string>);
    static_assert(std::same_as<decltype(get_result_t(a3)), std::string>);
    static_assert(std::same_as<decltype(get_result_t(a4)), std::string>);

    auto a5 = [=](auto) { return missi; };
    auto a6 = [=](auto get) { return get(missi); };
    auto a7 = [=](auto) { return "Missi"s; };

    static_assert(std::same_as<decltype(get_result_t(a5)), std::string>);
    static_assert(std::same_as<decltype(get_result_t(a6)), std::string>);
    static_assert(std::same_as<decltype(get_result_t(a7)), std::string>);

    calc c1 = missi;
    calc c2 = [=] { return missi; };
    calc c3 = [=] { return missi(); };
    calc c4 = [=] { return "Missi"s; };

    static_assert(std::same_as<decltype(get_result_t(c1)), std::string>);
    static_assert(std::same_as<decltype(get_result_t(c2)), std::string>);
    static_assert(std::same_as<decltype(get_result_t(c3)), std::string>);
    static_assert(std::same_as<decltype(get_result_t(c4)), std::string>);

    calc c5 = [=] { return missi; };
    calc c6 = [=](auto get) { return get(missi); };
    calc c7 = [=](auto) { return "Missi"s; };

    static_assert(std::same_as<decltype(get_result_t(c5)), std::string>);
    static_assert(std::same_as<decltype(get_result_t(c6)), std::string>);
    static_assert(std::same_as<decltype(get_result_t(c7)), std::string>);
  };

  "test_get_result_t_calc"_test = [] {
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

  "string"_test = [] {
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
};
