#include <boost/ut.hpp>

#include <prego/prego.h>

#include <format>
#include <optional>
#include <print>
#include <string>

using namespace boost::ut;
using namespace std::string_literals;

using prego::atom;
using prego::autorun;
using prego::calc;

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
    atom first_name = "John"s;
    atom last_name = "Doe"s;
    calc full_name = first_name + " " + last_name;

    atom pseudonym = std::optional<std::string>{};
    calc display_name = pseudonym.value_or(full_name);

    auto expensive_author_registry_lookup = [](const std::string &name) {
      return name == "Jane Austen" or name == "J.K. Rowling";
    };
    calc is_writer = [=] {
      return expensive_author_registry_lookup(display_name);
    };

    calc business_card = [=] {
      return std::format("Business card of {}{}", display_name,
                         is_writer ? ", writer" : "");
    };

    atom opt_out_mail = false;
    autorun([=] {
      auto ship_via_dhl = [](const std::string &msg) {};
      if (!opt_out_mail)
        ship_via_dhl(business_card);
    });

    atom opt_out_email = false;
    autorun([=] {
      auto email = [](const std::string &msg) {};
      if (!opt_out_email)
        email(business_card);
    });
  };
};
