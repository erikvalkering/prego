#include <boost/ut.hpp>

#include <prego/prego.h>

#include <format>
#include <optional>
#include <print>
#include <string>
#include <vector>

using namespace boost::ut;
using namespace std::string_literals;

using prego::atom;
using prego::autorun;
using prego::calc;

template <typename F> struct spy_t {
  F f;

  bool operator==(const spy_t &) const { return true; }

  friend auto operator+(const spy_t &self, auto &&other) {
    return std::forward<decltype(other)>(other);
  }

  friend auto operator+(auto &&other, const spy_t &self) {
    self.f();
    return std::forward<decltype(other)>(other);
  }
};

auto spy(auto f) {
  return calc{[=] { return spy_t{f}; }};
};

static suite<"integration_tests"> _ = [] {
  "example_from_readme"_test = [] {
    atom first_name = "John"s;
    atom last_name = "Doe"s;
    atom nick_name = std::optional{"Mr Unknown"s};

    calc full_name = first_name + " " + last_name;
    calc display_name = nick_name.value_or(full_name);

    atom enabled = true;
    autorun([=] {
      if (enabled)
        std::println("{}", display_name); // prints "Mr Unknown"
    });

    nick_name.reset();      // prints "John Doe"
    first_name = "Jane";    // prints "Jane Doe"
    nick_name = "Jane Doe"; // no change, nothing printed
    first_name = "John";    // no change, nothing printed
    enabled = false;        // autorun re-evaluated, nothing printed
    nick_name = "John Doe"; // autorun not re-evaluated, nothing printed
    enabled = true;         // autorun re-evaluated, prints "John Doe"
    nick_name.reset();      // no change, nothing printed
  };

  "business card"_test = [] {
    using msgs_t = std::vector<std::string>;
    auto msgs = msgs_t{};

    auto ship_via_dhl = [&](const std::string &msg) {
      msgs.push_back(std::format("Shipping via DHL: {}", msg));
    };
    auto email = [&](const std::string &msg) {
      msgs.push_back(std::format("Emailing: {}", msg));
    };

    atom first_name = "John"s;
    atom last_name = "Doe"s;
    calc full_name = first_name + " " + last_name +
                     spy([&] { msgs.push_back("Calculating full name"); });

    atom pseudonym = std::optional<std::string>{};
    calc display_name = pseudonym.value_or(full_name) + spy([&] {
                          msgs.push_back("Calculating display name");
                        });

    auto expensive_author_registry_lookup = [&](const std::string &name) {
      return name == "Jane Austen" or name == "J.K. Rowling";
    };
    calc is_writer = [=, &msgs] {
      msgs.push_back("Checking if author is a writer");
      return expensive_author_registry_lookup(display_name);
    };

    calc business_card = [=, &msgs] {
      msgs.push_back("Creating business card");
      return std::format("Business card of {}{}", display_name,
                         is_writer ? ", writer" : "");
    };

    expect(that % msgs == msgs_t{})
        << "None of the calculations should have run yet";

    atom opt_out_mail = false;
    autorun([=, &msgs] {
      msgs.push_back("Running autorun for shipping via DHL");
      if (!opt_out_mail)
        ship_via_dhl(business_card);
    });

    atom opt_out_email = false;
    autorun([=, &msgs] {
      msgs.push_back("Running autorun for emailing");
      if (!opt_out_email)
        email(business_card);
    });

    expect(that % msgs ==
           msgs_t{"Calculating full name", "Calculating display name",
                  "Running autorun for shipping via DHL",
                  "Creating business card", "Checking if author is a writer",
                  "Shipping via DHL: Business card of John Doe",
                  "Running autorun for emailing",
                  "Emailing: Business card of John Doe"});
  };
};
