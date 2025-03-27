#include "common.h"

static suite<"transparent syntax"> _ = [] {
  "test_implicit_conversions"_test = [] {
    atom missi = "Missi"s;
    calc faaiv = [] { return "Faaiv"s; };

    std::string m = missi;
    std::string f = faaiv;
  };

  "test_get_result_t"_test = [] {
    using prego::get_result_t;

    atom missi = "Missi"s;

    auto a1 = missi;
    auto a2 = [=] { return missi; };
    auto a3 = [=] { return missi(); };
    auto a4 = [=] { return "Missi"s; };

    static_assert(std::same_as<decltype(get_result_t(a1)), std::string>);
    // static_assert(std::same_as<decltype(get_result_t(a2)), std::string>);
    static_assert(std::same_as<decltype(get_result_t(a3)), std::string>);
    static_assert(std::same_as<decltype(get_result_t(a4)), std::string>);

    auto a5 = [=](auto) { return missi; };
    auto a6 = [=](auto get) { return get(missi); };
    auto a7 = [=](auto) { return "Missi"s; };

    // static_assert(std::same_as<decltype(get_result_t(a5)), std::string>);
    static_assert(std::same_as<decltype(get_result_t(a6)), std::string>);
    static_assert(std::same_as<decltype(get_result_t(a7)), std::string>);

    calc c1 = missi;
    // calc c2 = [=] { return missi; };
    calc c3 = [=] { return missi(); };
    calc c4 = [=] { return "Missi"s; };

    static_assert(std::same_as<decltype(get_result_t(c1)), std::string>);
    // static_assert(std::same_as<decltype(get_result_t(c2)), std::string>);
    static_assert(std::same_as<decltype(get_result_t(c3)), std::string>);
    static_assert(std::same_as<decltype(get_result_t(c4)), std::string>);

    // calc c5 = [=] { return missi; };
    calc c6 = [=](auto get) { return get(missi); };
    calc c7 = [=](auto) { return "Missi"s; };

    // static_assert(std::same_as<decltype(get_result_t(c5)), std::string>);
    static_assert(std::same_as<decltype(get_result_t(c6)), std::string>);
    static_assert(std::same_as<decltype(get_result_t(c7)), std::string>);
  };
};
