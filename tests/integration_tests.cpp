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
    calc display_name = [=, &msgs] {
      auto result = pseudonym.has_value() ? pseudonym.value()() : full_name;
      msgs.push_back("Calculating display name");
      return result;
    };
    // calc display_name = pseudonym.value_or(full_name) + spy([&] {
    //                       msgs.push_back("Calculating display name");
    //                     });

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

    expect(that % msgs == msgs_t{
                              "Running autorun for shipping via DHL",
                              "Creating business card",
                              "Checking if author is a writer",
                              "Calculating full name",
                              "Calculating display name",
                              "Shipping via DHL: Business card of John Doe",
                              "Running autorun for emailing",
                              "Emailing: Business card of John Doe",
                          });
    msgs.clear();

    // Make sure that setting first_name or last_name to the same values will
    // not trigger any calculations
    first_name = "John"s;
    last_name = "Doe"s;
    expect(that % msgs == msgs_t{});
    msgs.clear();

    // Setting the pseudonym to the same value as full_name should not trigger
    // further calculations
    pseudonym = "John Doe"s;
    expect(that % msgs == msgs_t{
                              "Calculating display name",
                          });
    msgs.clear();

    // Change the pseudonym to a different value, which should trigger
    // a calculation of display_name and all the dependent calculations
    pseudonym = "Jane Doe"s;
    expect(that % msgs == msgs_t{
                              "Calculating display name",
                              "Checking if author is a writer",
                              "Creating business card",
                              "Running autorun for shipping via DHL",
                              "Shipping via DHL: Business card of Jane Doe",
                              "Running autorun for emailing",
                              "Emailing: Business card of Jane Doe",
                          });
    msgs.clear();

    // Because pseudonym is set, display_name should not depend on full_name
    // So this should not trigger any calculations
    first_name = "Jane";
    expect(that % msgs == msgs_t{});
    msgs.clear();

    // Now display_name depends on full_name again,
    // but since the resulting value is the same,
    // no further calculations should be triggered
    pseudonym.reset();
    expect(that % msgs == msgs_t{
                              "Calculating full name",
                              "Calculating display name",
                          });
    msgs.clear();

    // This will make the display_name a writer
    last_name = "Austen";
    expect(that % msgs ==
           msgs_t{
               "Calculating full name",
               "Calculating display name",
               "Checking if author is a writer",
               "Creating business card",
               "Running autorun for shipping via DHL",
               "Shipping via DHL: Business card of Jane Austen, writer",
               "Running autorun for emailing",
               "Emailing: Business card of Jane Austen, writer",
           });
    msgs.clear();

    // This will not be recognized as a writer,
    // because the expensive lookup won't find this name
    first_name = "Joanna";
    msgs.clear(); // TODO: implement transactional mutations
    last_name = "Rowling";
    expect(that % msgs ==
           msgs_t{
               "Calculating full name",
               "Calculating display name",
               "Checking if author is a writer",
               "Creating business card",
               "Running autorun for shipping via DHL",
               "Shipping via DHL: Business card of Joanna Rowling",
               "Running autorun for emailing",
               "Emailing: Business card of Joanna Rowling",
           });
    msgs.clear();

    // This will be recognized as a writer,
    // since the expensive lookup will find this name
    pseudonym = "J.K. Rowling"s;
    expect(that % msgs ==
           msgs_t{
               "Calculating display name",
               "Checking if author is a writer",
               "Creating business card",
               "Running autorun for shipping via DHL",
               "Shipping via DHL: Business card of J.K. Rowling, writer",
               "Running autorun for emailing",
               "Emailing: Business card of J.K. Rowling, writer",
           });
    msgs.clear();

    // Disabling the mail notifications
    opt_out_mail = true;
    expect(that % msgs == msgs_t{
                              "Running autorun for shipping via DHL",
                          });
    msgs.clear();

    // Disabling the email notifications
    opt_out_email = true;
    expect(that % msgs == msgs_t{
                              "Running autorun for emailing",
                          });
    msgs.clear();

    // Changing the first_name or last_name should not trigger any
    // calculations, because the autoruns are not observing these
    // values right now
    first_name = "John"s;
    last_name = "Doe"s;
    pseudonym.reset();
    expect(that % msgs == msgs_t{});
    msgs.clear();

    // Turning on the email notifications
    // will trigger the calculations
    opt_out_email = false;
    expect(that % msgs == msgs_t{
                              "Running autorun for emailing",
                              "Creating business card",
                              "Checking if author is a writer",
                              "Calculating full name",
                              "Calculating display name",
                              "Emailing: Business card of John Doe",
                          });
    msgs.clear();
  };
};
