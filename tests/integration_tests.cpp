#include <boost/ut.hpp>

#include <prego/prego.h>

#include <format>
#include <optional>
#include <print>
#include <set>
#include <string>
#include <utility>
#include <vector>

using namespace boost::ut;
using namespace std::string_literals;

using prego::atom;
using prego::autorun;
using prego::calc;
using prego::spy;

enum class shipment_t {
  opt_out,
  dhl,
  print_at_home,
};

auto ship_via_dhl(auto &msgs, const std::string &msg) {
  msgs.insert(msgs.end(), std::format("Shipping via DHL: {}", msg));
}

auto email(auto &msgs, const std::string &msg) {
  msgs.insert(msgs.end(), std::format("Emailing: {}", msg));
}
auto expensive_author_registry_lookup(const std::string &name) {
  return name == "Jane Austen" or name == "Multatuli";
}

template <typename F> struct assigner {
  F f;
  decltype(auto) operator=(auto &&value) {
    return f(std::forward<decltype(value)>(value));
  }
  auto reset() { (*this) = std::nullopt; }
};

auto test_business_card(auto &msgs, auto &&first_name, auto &&last_name,
                        auto &&pseudonym, auto &&shipment) {
  using msgs_t = std::remove_cvref_t<decltype(msgs)>;

  expect(that % msgs == msgs_t{
                            "autorun:dhl",
                            "business_card",
                            "is_writer",
                            "display_name",
                            "full_name",
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
                            "display_name",
                            "full_name",
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
  first_name = "Eduard Douwes";
  msgs.clear();
  last_name = "Dekker";
  expect(that % msgs ==
         msgs_t{
             "full_name",
             "display_name",
             "is_writer",
             "business_card",
             "autorun:dhl",
             "Shipping via DHL: Business card of Eduard Douwes Dekker",
         });
  msgs.clear();

  // This will be recognized as a writer,
  // since the expensive lookup will find this name
  pseudonym = "Multatuli"s;
  expect(that % msgs ==
         msgs_t{
             "display_name",
             "is_writer",
             "business_card",
             "autorun:dhl",
             "Shipping via DHL: Business card of Multatuli, writer",
         });
  msgs.clear();

  // Disabling the mail notifications
  shipment = shipment_t::print_at_home;
  expect(that % msgs == msgs_t{
                            "autorun:dhl",
                            "autorun:print_at_home",
                            "Emailing: Business card of Multatuli, writer",
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
                            "display_name",
                            "full_name",
                            "is_writer",
                            "business_card",
                            "Emailing: Business card of John Doe",
                        });
  msgs.clear();

  shipment = shipment_t::opt_out;
  expect(that % msgs == msgs_t{
                            "autorun:dhl",
                            "autorun:print_at_home",
                        });
  msgs.clear();

  // NOTE: setting pseudonym and first_name will not trigger
  // a recalculation of display_name and full_name, because those are not
  // reactive.
  // Changing shipment does and will trigger the revaluation (only of
  // display_name).
  pseudonym = "John Doe"s;
  first_name = "Jane"s;
  shipment = shipment_t::print_at_home;
  expect(that % msgs == msgs_t{
                            "autorun:dhl",
                            "autorun:print_at_home",
                            "display_name",
                            "Emailing: Business card of John Doe",
                        });
  msgs.clear();
}

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

  "business card (prego)"_test = [] {
    auto msgs = std::vector<std::string>{};
    auto tag = [&msgs](auto id) {
      return spy([id, &msgs] { msgs.push_back(id); });
    };

    atom first_name = "John"s;
    atom last_name = "Doe"s;
    calc full_name = first_name + " " + last_name + tag("full_name");

    atom pseudonym = std::optional<std::string>{};
    calc display_name = pseudonym.value_or(full_name) + tag("display_name");

    calc is_writer = [=] {
      return expensive_author_registry_lookup(display_name);
    } + tag("is_writer");

    calc business_card = [=] {
      return std::format("Business card of {}{}", display_name,
                         is_writer ? ", writer" : "");
    } + tag("business_card");

    expect(that % msgs.empty())
        << "None of the calculations should have run yet";

    atom shipment = shipment_t::dhl;
    autorun([=, &msgs] {
      if (shipment == shipment_t::dhl)
        ship_via_dhl(msgs, business_card);
    } + tag("autorun:dhl"));

    autorun([=, &msgs] {
      if (shipment == shipment_t::print_at_home)
        email(msgs, business_card);
    } + tag("autorun:print_at_home"));

    test_business_card(msgs, first_name, last_name, pseudonym, shipment);

    // TODO: implement transactional mutations
  };

  "business card (naive)"_test = [=] {
    auto msgs = std::multiset<std::string>{};

    // atoms
    auto first_name = "John"s;
    auto last_name = "Doe"s;
    auto pseudonym = std::optional<std::string>{};
    auto shipment = shipment_t::dhl;

    // calcs
    auto full_name_dirty = true;
    auto full_name_cache = std::optional<std::string>{};

    auto display_name_dirty = true;
    auto display_name_cache = std::optional<std::string>{};

    auto is_writer_dirty = true;
    auto is_writer_cache = std::optional<bool>{};

    auto business_card_dirty = true;
    auto business_card_cache = std::optional<std::string>{};

    auto autorun_dhl_dirty = true;
    auto autorun_print_at_home_dirty = true;

    auto full_name_observers_display_name = false;
    auto update_full_name = [&] {
      if (not std::exchange(full_name_dirty, false))
        return false;

      msgs.insert("full_name");
      const auto value = first_name + " " + last_name;
      if (value == std::exchange(full_name_cache, value))
        return false;

      if (full_name_observers_display_name)
        display_name_dirty = true;

      return true;
    };
    auto full_name = [&] {
      update_full_name();
      return full_name_cache.value();
    };

    auto update_display_name = [&] {
      if (not display_name_dirty) {
        if (full_name_observers_display_name)
          update_full_name();
      }
      if (not std::exchange(display_name_dirty, false))
        return false;

      full_name_observers_display_name = false;

      msgs.insert("display_name");
      const auto value = [&] {
        if (pseudonym.has_value())
          return pseudonym.value();
        const auto res = full_name();
        full_name_observers_display_name = true;
        return res;
      }();
      if (value == std::exchange(display_name_cache, value))
        return false;

      is_writer_dirty = true;
      business_card_dirty = true;

      return true;
    };
    auto display_name = [&] {
      update_display_name();
      return display_name_cache.value();
    };

    auto update_is_writer = [&] {
      update_display_name();
      if (not std::exchange(is_writer_dirty, false))
        return false;

      msgs.insert("is_writer");
      const auto value = expensive_author_registry_lookup(display_name());
      if (value == std::exchange(is_writer_cache, value))
        return false;

      business_card_dirty = true;

      return true;
    };
    auto is_writer = [&] {
      update_is_writer();
      return is_writer_cache.value();
    };

    auto business_card_observers_autorun_dhl = false;
    auto business_card_observers_autorun_print_at_home = false;
    auto update_business_card = [&] {
      update_display_name();
      update_is_writer();
      if (not std::exchange(business_card_dirty, false))
        return false;

      msgs.insert("business_card");
      const auto value = std::format("Business card of {}{}", display_name(),
                                     is_writer() ? ", writer" : "");
      if (value == std::exchange(business_card_cache, value))
        return false;

      if (business_card_observers_autorun_dhl)
        autorun_dhl_dirty = true;
      if (business_card_observers_autorun_print_at_home)
        autorun_print_at_home_dirty = true;

      return true;
    };
    auto business_card = [&] {
      update_business_card();
      return business_card_cache.value();
    };

    auto autorun_dhl = [&] {
      if (business_card_observers_autorun_dhl)
        update_business_card();
      if (not std::exchange(autorun_dhl_dirty, false))
        return;

      business_card_observers_autorun_dhl = false;

      msgs.insert("autorun:dhl");
      if (shipment == shipment_t::dhl) {
        ship_via_dhl(msgs, business_card());
        business_card_observers_autorun_dhl = true;
      }
    };

    auto autorun_print_at_home = [&] {
      if (business_card_observers_autorun_print_at_home)
        update_business_card();
      if (not std::exchange(autorun_print_at_home_dirty, false))
        return;

      business_card_observers_autorun_print_at_home = false;

      msgs.insert("autorun:print_at_home");
      if (shipment == shipment_t::print_at_home) {
        email(msgs, business_card());
        business_card_observers_autorun_print_at_home = true;
      }
    };

    auto update = [&] {
      autorun_dhl();
      autorun_print_at_home();
    };

    auto set_first_name = [&](auto value) {
      if (value == std::exchange(first_name, value))
        return;

      full_name_dirty = true;

      update();
    };
    auto set_last_name = [&](auto value) {
      if (value == std::exchange(last_name, value))
        return;

      full_name_dirty = true;

      update();
    };
    auto set_pseudonym = [&](auto value) {
      if (value == std::exchange(pseudonym, value))
        return;

      display_name_dirty = true;

      update();
    };
    auto set_shipment = [&](auto value) {
      if (value == std::exchange(shipment, value))
        return;

      autorun_dhl_dirty = true;
      autorun_print_at_home_dirty = true;

      update();
    };

    update();

    test_business_card(msgs, assigner{set_first_name}, assigner{set_last_name},
                       assigner{set_pseudonym}, assigner{set_shipment});
  };
};
