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

// This is a spy that can be used to
// observe a calculation without affecting its result.
template <typename F> struct spy_t {
  F f;

  bool operator==(const spy_t &) const { return true; }

  friend auto operator+(auto &&other, const spy_t &self) {
    self.f();
    if constexpr (std::invocable<decltype(other)>) {
      if constexpr (std::is_same_v<std::invoke_result_t<decltype(other)>,
                                   void>) {
        other();
        return 0;
      } else {
        return std::forward<decltype(other)>(other)();
      }
    } else {
      return std::forward<decltype(other)>(other);
    }
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
    auto tag = [&msgs](auto id) {
      return spy([id, &msgs] { msgs.push_back(id); });
    };

    enum class shipment_t {
      opt_out,
      dhl,
      print_at_home,
    };

    auto ship_via_dhl = [&](const std::string &msg) {
      msgs.push_back(std::format("Shipping via DHL: {}", msg));
    };
    auto email = [&](const std::string &msg) {
      msgs.push_back(std::format("Emailing: {}", msg));
    };

    atom first_name = "John"s;
    atom last_name = "Doe"s;
    calc full_name = first_name + " " + last_name + tag("full_name");

    atom pseudonym = std::optional<std::string>{};
    calc display_name = pseudonym.value_or(full_name) + tag("display_name");

    auto expensive_author_registry_lookup = [&](const std::string &name) {
      return name == "Jane Austen" or name == "J.K. Rowling";
    };
    calc is_writer = [=] {
      return expensive_author_registry_lookup(display_name);
    } + tag("is_writer");

    calc business_card = [=] {
      return std::format("Business card of {}{}", display_name,
                         is_writer ? ", writer" : "");
    } + tag("business_card");

    expect(that % msgs == msgs_t{})
        << "None of the calculations should have run yet";

    atom shipment = shipment_t::dhl;
    autorun([=] {
      if (shipment == shipment_t::dhl)
        ship_via_dhl(business_card);
    } + tag("autorun:dhl"));

    autorun([=] {
      if (shipment == shipment_t::print_at_home)
        email(business_card);
    } + tag("autorun:print_at_home"));

    expect(that % msgs == msgs_t{
                              "autorun:dhl",
                              "business_card",
                              "is_writer",
                              "full_name",
                              "display_name",
                              "Shipping via DHL: Business card of John Doe",
                              "autorun:print_at_home",
                          });
    msgs.clear();

    // Make sure that setting first_name or last_name to the same values will
    // not trigger any calculations
    first_name = "John"s;
    last_name = "Doe"s;
    expect(that % msgs == msgs_t{});
    msgs.clear();

    // Setting the pseudonym to the same value as full_name should not
    // trigger further calculations
    pseudonym = "John Doe"s;
    expect(that % msgs == msgs_t{
                              "display_name",
                          });
    msgs.clear();

    // Change the pseudonym to a different value, which should trigger
    // a calculation of display_name and all the dependent calculations
    pseudonym = "Jane Doe"s;
    expect(that % msgs == msgs_t{
                              "display_name",
                              "is_writer",
                              "business_card",
                              "autorun:dhl",
                              "Shipping via DHL: Business card of Jane Doe",
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
                              "full_name",
                              "display_name",
                          });
    msgs.clear();

    // This will make the display_name a writer
    last_name = "Austen";
    expect(that % msgs ==
           msgs_t{
               "full_name",
               "display_name",
               "is_writer",
               "business_card",
               "autorun:dhl",
               "Shipping via DHL: Business card of Jane Austen, writer",
           });
    msgs.clear();

    // This will not be recognized as a writer,
    // because the expensive lookup won't find this name
    first_name = "Joanna";
    msgs.clear(); // TODO: implement transactional mutations
    last_name = "Rowling";
    expect(that % msgs ==
           msgs_t{
               "full_name",
               "display_name",
               "is_writer",
               "business_card",
               "autorun:dhl",
               "Shipping via DHL: Business card of Joanna Rowling",
           });
    msgs.clear();

    // This will be recognized as a writer,
    // since the expensive lookup will find this name
    pseudonym = "J.K. Rowling"s;
    expect(that % msgs ==
           msgs_t{
               "display_name",
               "is_writer",
               "business_card",
               "autorun:dhl",
               "Shipping via DHL: Business card of J.K. Rowling, writer",
           });
    msgs.clear();

    // Disabling the mail notifications
    shipment = shipment_t::print_at_home;
    expect(that % msgs == msgs_t{
                              "autorun:dhl",
                              "autorun:print_at_home",
                              "Emailing: Business card of J.K. Rowling, writer",
                          });
    msgs.clear();

    // Disabling the email notifications
    shipment = shipment_t::opt_out;
    expect(that % msgs == msgs_t{
                              "autorun:dhl",
                              "autorun:print_at_home",
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
    shipment = shipment_t::print_at_home;
    expect(that % msgs == msgs_t{
                              "autorun:dhl",
                              "autorun:print_at_home",
                              "business_card",
                              "is_writer",
                              "full_name",
                              "display_name",
                              "Emailing: Business card of John Doe",
                          });
    msgs.clear();

    shipment = shipment_t::opt_out;
    expect(that % msgs == msgs_t{
                              "autorun:dhl",
                              "autorun:print_at_home",
                          });
    msgs.clear();

    pseudonym = "John Doe"s;
    first_name = "Jane"s;
    shipment = shipment_t::print_at_home;
    expect(that % msgs == msgs_t{
                              "display_name",
                              "is_writer",
                              "business_card",
                              "autorun:print_at_home",
                              "Emailing: Business card of John Doe",
                          });
    msgs.clear();
  };
};
