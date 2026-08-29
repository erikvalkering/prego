#include <boost/ut.hpp>

#include <functional>
#include <prego/prego.h>

#include <format>
#include <optional>
#include <print>
#include <set>
#include <string>
#include <type_traits>
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

template<typename F> struct assigner {
  F &f;
  decltype(auto) operator=(auto &&value) {
    return f(std::forward<decltype(value)>(value));
  }
  auto reset() { (*this) = std::nullopt; }
};

template<typename tag> struct observer_ref_t {
  bool *dirty = nullptr;
};

template<typename tag> auto &observer_ref(auto &observers) {
  return std::get<observer_ref_t<tag>>(observers);
}

template<typename... observer_tags> auto atom3(auto value) {
  struct state {
    decltype(value) value;
    std::tuple<observer_ref_t<observer_tags>...> observers;

    auto operator()() const { return value; }
  };

  return state{value};
}

auto set_value(auto &atom, auto value, auto on_changed) {
  if (value == std::exchange(atom.value, value)) return;

  []<typename... tags>(std::tuple<observer_ref_t<tags>...> &observers) {
    ((observer_ref<tags>(observers).dirty &&
      (*observer_ref<tags>(observers).dirty = true)),
     ...);
  }(atom.observers);

  on_changed();
};

template<typename... observer_tags> auto calc3(auto f) {
  struct state {
    decltype(f) f;
    bool dirty = true;
    std::optional<decltype(f())> cache;
    std::tuple<observer_ref_t<observer_tags>...> observers;

    auto operator()() const { return f(); }
  };

  return state{f};
}

template<typename observer_tag> auto link(auto &dependency, bool &dirty_flag) {
  observer_ref<observer_tag>(dependency.observers).dirty = &dirty_flag;
}

template<typename observer_tag> auto unlink(auto &dependency) {
  observer_ref<observer_tag>(dependency.observers).dirty = nullptr;
}

auto test_business_card(auto &msgs,
                        auto &&first_name,
                        auto &&last_name,
                        auto &&pseudonym,
                        auto &&shipment,
                        auto &&enable_extra) {
  using msgs_t = std::remove_cvref_t<decltype(msgs)>;

  expect(that % msgs == msgs_t{
                          "autorun:dhl",
                          "business_card",
                          "is_writer",
                          "display_name",
                          "full_name",
                          "Shipping via DHL: Business card of John Doe",
                          "autorun:print_at_home",
                          "autorun:extra",
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

  pseudonym.reset();
  msgs.clear();

  first_name = "Jane";
  expect(that % msgs == msgs_t{
                          "full_name",
                          "display_name",
                          "is_writer",
                          "business_card",
                          "autorun:dhl",
                          "Shipping via DHL: Business card of Jane Doe",
                        });
  msgs.clear();

  pseudonym = "John Doe"s;
  first_name = "John"s;
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

  shipment = shipment_t::opt_out;
  enable_extra = true;
  msgs.clear();

  pseudonym = "Jane Doe";
  expect(that % msgs == msgs_t{
                          "display_name",
                          "is_writer",
                        });
  msgs.clear();

  first_name = "John";
  expect(that % msgs == msgs_t{"autorun:extra"});

  enable_extra = false;
  msgs.clear();

  first_name = "Jane";
  expect(that % msgs == msgs_t{});

  // check that if full_name is not reactive, changing its dependencies will
  // still trigger a recalculation once it becomes reactive again.
  shipment = shipment_t::dhl;
  pseudonym.reset();
  pseudonym = "Jane Doe";
  first_name = "John";
  msgs.clear();

  pseudonym.reset();
  expect(that % msgs == msgs_t{
                          "display_name",
                          "full_name",
                          "is_writer",
                          "business_card",
                          "autorun:dhl",
                          "Shipping via DHL: Business card of John Doe",
                        });
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
      if (enabled) std::println("{}", display_name); // prints "Mr Unknown"
    });

    nick_name.reset(); // prints "John Doe"
    first_name = "Jane"; // prints "Jane Doe"
    nick_name = "Jane Doe"; // no change, nothing printed
    first_name = "John"; // no change, nothing printed
    enabled = false; // autorun re-evaluated, nothing printed
    nick_name = "John Doe"; // autorun not re-evaluated, nothing printed
    enabled = true; // autorun re-evaluated, prints "John Doe"
    nick_name.reset(); // no change, nothing printed
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
      return std::format(
        "Business card of {}{}", display_name, is_writer ? ", writer" : "");
    } + tag("business_card");

    expect(that % msgs.empty())
      << "None of the calculations should have run yet";

    atom shipment = shipment_t::dhl;
    autorun([=, &msgs] {
      if (shipment == shipment_t::dhl) ship_via_dhl(msgs, business_card);
    } + tag("autorun:dhl"));

    autorun([=, &msgs] {
      if (shipment == shipment_t::print_at_home) email(msgs, business_card);
    } + tag("autorun:print_at_home"));

    atom enable_extra = false;
    autorun([=] {
      if (not enable_extra) return;
      is_writer();
      first_name();
    } + tag("autorun:extra"));

    test_business_card(
      msgs, first_name, last_name, pseudonym, shipment, enable_extra);

    // TODO: implement transactional mutations
    // TODO: implement true dynamic dependency graph: in a loop (with a
    // user-specified count at runtime (or even a atomic or calculated state)),
    // attach (a therefore variable number of) autoruns
  };

  "business card (naive)"_test = [=] {
    auto msgs = std::multiset<std::string>{};

    // atoms
    auto first_name = "John"s;
    auto last_name = "Doe"s;
    auto pseudonym = std::optional<std::string>{};
    auto shipment = shipment_t::dhl;
    auto enable_extra = false;

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
    auto autorun_extra_dirty = true;

    auto full_name_observers_display_name = false;
    auto update_full_name = [&] {
      if (not std::exchange(full_name_dirty, false)) return false;

      msgs.insert("full_name");
      const auto value = first_name + " " + last_name;
      if (value == std::exchange(full_name_cache, value)) return false;

      if (full_name_observers_display_name) display_name_dirty = true;

      return true;
    };
    auto full_name = [&] {
      update_full_name();
      return full_name_cache.value();
    };

    auto update_display_name = [&] {
      if (not display_name_dirty) {
        if (full_name_observers_display_name) update_full_name();
      }
      if (not std::exchange(display_name_dirty, false)) return false;

      full_name_observers_display_name = false;

      msgs.insert("display_name");
      const auto value = [&] {
        if (pseudonym.has_value()) return pseudonym.value();
        const auto res = full_name();
        full_name_observers_display_name = true;
        return res;
      }();
      if (value == std::exchange(display_name_cache, value)) return false;

      is_writer_dirty = true;
      business_card_dirty = true;

      return true;
    };
    auto display_name = [&] {
      update_display_name();
      return display_name_cache.value();
    };

    auto is_writer_observers_autorun_extra = false;
    auto update_is_writer = [&] {
      update_display_name();
      if (not std::exchange(is_writer_dirty, false)) return false;

      msgs.insert("is_writer");
      const auto value = expensive_author_registry_lookup(display_name());
      if (value == std::exchange(is_writer_cache, value)) return false;

      business_card_dirty = true;
      if (is_writer_observers_autorun_extra) autorun_extra_dirty = true;

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
      if (not std::exchange(business_card_dirty, false)) return false;

      msgs.insert("business_card");
      const auto value = std::format(
        "Business card of {}{}", display_name(), is_writer() ? ", writer" : "");
      if (value == std::exchange(business_card_cache, value)) return false;

      if (business_card_observers_autorun_dhl) autorun_dhl_dirty = true;
      if (business_card_observers_autorun_print_at_home)
        autorun_print_at_home_dirty = true;

      return true;
    };
    auto business_card = [&] {
      update_business_card();
      return business_card_cache.value();
    };

    auto autorun_dhl = [&] {
      if (business_card_observers_autorun_dhl) update_business_card();
      if (not std::exchange(autorun_dhl_dirty, false)) return;

      business_card_observers_autorun_dhl = false;

      msgs.insert("autorun:dhl");
      if (shipment == shipment_t::dhl) {
        ship_via_dhl(msgs, business_card());
        business_card_observers_autorun_dhl = true;
      }
    };

    auto autorun_print_at_home = [&] {
      if (business_card_observers_autorun_print_at_home) update_business_card();
      if (not std::exchange(autorun_print_at_home_dirty, false)) return;

      business_card_observers_autorun_print_at_home = false;

      msgs.insert("autorun:print_at_home");
      if (shipment == shipment_t::print_at_home) {
        email(msgs, business_card());
        business_card_observers_autorun_print_at_home = true;
      }
    };

    auto first_name_observers_autorun_extra = false;
    auto autorun_extra = [&] {
      if (is_writer_observers_autorun_extra) update_is_writer();
      if (not std::exchange(autorun_extra_dirty, false)) return;

      is_writer_observers_autorun_extra = false;
      first_name_observers_autorun_extra = false;
      msgs.insert("autorun:extra");
      if (enable_extra) {
        is_writer();
        is_writer_observers_autorun_extra = true;
        // TODO: why is the next line commented out?
        // first_name();
        first_name_observers_autorun_extra = true;
      }
    };

    auto update = [&] {
      autorun_dhl();
      autorun_print_at_home();
      autorun_extra();
    };

    auto set_first_name = [&](auto value) {
      if (value == std::exchange(first_name, value)) return;

      full_name_dirty = true;
      if (first_name_observers_autorun_extra) autorun_extra_dirty = true;

      update();
    };
    auto set_last_name = [&](auto value) {
      if (value == std::exchange(last_name, value)) return;

      full_name_dirty = true;

      update();
    };
    auto set_pseudonym = [&](auto value) {
      if (value == std::exchange(pseudonym, value)) return;

      display_name_dirty = true;

      update();
    };
    auto set_shipment = [&](auto value) {
      if (value == std::exchange(shipment, value)) return;

      autorun_dhl_dirty = true;
      autorun_print_at_home_dirty = true;

      update();
    };
    auto set_enable_extra = [&](auto value) {
      if (value == std::exchange(enable_extra, value)) return;

      autorun_extra_dirty = true;

      update();
    };

    update();

    test_business_card(msgs,
                       assigner{set_first_name},
                       assigner{set_last_name},
                       assigner{set_pseudonym},
                       assigner{set_shipment},
                       assigner{set_enable_extra});
  };

  "business card (encapsulated)"_test = [=] {
    auto msgs = std::multiset<std::string>{};

    // observer tags
    struct full_name_tag;
    struct display_name_tag;
    struct autorun_dhl_tag;
    struct autorun_print_at_home_tag;
    struct autorun_extra_tag;

    // atoms
    auto first_name = atom3<full_name_tag, autorun_extra_tag>("John"s);
    auto last_name = atom3<full_name_tag>("Doe"s);
    auto pseudonym = atom3<display_name_tag>(std::optional<std::string>{});
    auto shipment =
      atom3<autorun_dhl_tag, autorun_print_at_home_tag>(shipment_t::dhl);
    auto enable_extra = atom3<autorun_extra_tag>(false);

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
    auto autorun_extra_dirty = true;

    auto full_name_observers_display_name = false;
    auto update_full_name = [&] {
      if (not std::exchange(full_name_dirty, false)) return false;

      msgs.insert("full_name");
      const auto value = first_name() + " " + last_name();
      if (value == std::exchange(full_name_cache, value)) return false;

      if (full_name_observers_display_name) display_name_dirty = true;

      return true;
    };
    auto full_name = [&] {
      update_full_name();
      return full_name_cache.value();
    };

    auto update_display_name = [&] {
      if (not display_name_dirty) {
        if (full_name_observers_display_name) full_name();
      }
      if (not std::exchange(display_name_dirty, false)) return false;

      full_name_observers_display_name = false;

      msgs.insert("display_name");
      const auto value = [&] {
        if (pseudonym().has_value()) return pseudonym().value();
        const auto res = full_name();
        full_name_observers_display_name = true;
        return res;
      }();
      if (value == std::exchange(display_name_cache, value)) return false;

      is_writer_dirty = true;
      business_card_dirty = true;

      return true;
    };
    auto display_name = [&] {
      update_display_name();
      return display_name_cache.value();
    };

    auto is_writer_observers_autorun_extra = false;
    auto update_is_writer = [&] {
      update_display_name();
      if (not std::exchange(is_writer_dirty, false)) return false;

      msgs.insert("is_writer");
      const auto value = expensive_author_registry_lookup(display_name());
      if (value == std::exchange(is_writer_cache, value)) return false;

      business_card_dirty = true;
      if (is_writer_observers_autorun_extra) autorun_extra_dirty = true;

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
      if (not std::exchange(business_card_dirty, false)) return false;

      msgs.insert("business_card");
      const auto value = std::format(
        "Business card of {}{}", display_name(), is_writer() ? ", writer" : "");
      if (value == std::exchange(business_card_cache, value)) return false;

      if (business_card_observers_autorun_dhl) autorun_dhl_dirty = true;
      if (business_card_observers_autorun_print_at_home)
        autorun_print_at_home_dirty = true;

      return true;
    };
    auto business_card = [&] {
      update_business_card();
      return business_card_cache.value();
    };

    auto autorun_dhl = [&] {
      if (business_card_observers_autorun_dhl) update_business_card();
      if (not std::exchange(autorun_dhl_dirty, false)) return;

      business_card_observers_autorun_dhl = false;

      msgs.insert("autorun:dhl");
      if (shipment() == shipment_t::dhl) {
        ship_via_dhl(msgs, business_card());
        business_card_observers_autorun_dhl = true;
      }
    };

    auto autorun_print_at_home = [&] {
      if (business_card_observers_autorun_print_at_home) update_business_card();
      if (not std::exchange(autorun_print_at_home_dirty, false)) return;

      business_card_observers_autorun_print_at_home = false;

      msgs.insert("autorun:print_at_home");
      if (shipment() == shipment_t::print_at_home) {
        email(msgs, business_card());
        business_card_observers_autorun_print_at_home = true;
      }
    };

    auto autorun_extra = [&] {
      if (is_writer_observers_autorun_extra) update_is_writer();
      // TODO: what if first_name becomes a calc? shouldn't we check it here?
      // for example, if pseudonym is set, is_writer doesn't change, but this
      // autorun should still be called.
      if (not std::exchange(autorun_extra_dirty, false)) return;

      is_writer_observers_autorun_extra = false;
      unlink<autorun_extra_tag>(first_name);
      msgs.insert("autorun:extra");
      if (enable_extra()) {
        is_writer();
        is_writer_observers_autorun_extra = true;
        // first_name();
        link<autorun_extra_tag>(first_name, autorun_extra_dirty);
      }
    };

    auto update = [&] {
      autorun_dhl();
      autorun_print_at_home();
      autorun_extra();
    };

    link<full_name_tag>(first_name, full_name_dirty);
    auto set_first_name = [&](auto value) {
      set_value(first_name, value, update);
    };

    link<full_name_tag>(last_name, full_name_dirty);
    auto set_last_name = [&](auto value) {
      set_value(last_name, value, update);
    };

    link<display_name_tag>(pseudonym, display_name_dirty);
    auto set_pseudonym = [&](auto value) {
      set_value(pseudonym, value, update);
    };

    link<autorun_dhl_tag>(shipment, autorun_dhl_dirty);
    link<autorun_print_at_home_tag>(shipment, autorun_print_at_home_dirty);
    auto set_shipment = [&](auto value) { set_value(shipment, value, update); };

    link<autorun_extra_tag>(enable_extra, autorun_extra_dirty);
    auto set_enable_extra = [&](auto value) {
      set_value(enable_extra, value, update);
    };

    update();

    test_business_card(msgs,
                       assigner{set_first_name},
                       assigner{set_last_name},
                       assigner{set_pseudonym},
                       assigner{set_shipment},
                       assigner{set_enable_extra});
  };

  "business card (top-down)"_test = [=] {
    auto msgs = std::multiset<std::string>{};

    auto update = std::function<void()>{};

    // TODO: uncouple observers during creation: use slots nttp param
    // TODO: encapsulate value dirty and observers in state class
    // TODO: as in-between, use only state class but manual dependency
    // management. wire the state classes together, wheras atom2/calc2/autorun2
    // below is already more of a library solution
    // TODO: automatic registration
    auto atom2 = [&](auto value, auto &&...observers) {
      auto store = std::make_unique<decltype(value)>(value);

      auto getter = [p = store.get()] { return *p; };
      auto setter = [&, store = std::move(store)](auto value) {
        if (value == std::exchange(*store, value)) return;

        ((observers && (*observers = true)), ...);

        update();
      };

      return std::tuple{
        getter,
        std::move(setter),
      };
    };

    auto check_dep = [](auto &&dep) {
      if constexpr (std::invocable<decltype(dep)>) {
        dep();
      } else {
        if (dep) (*dep)();
      }
    };

    auto calc2 = [=](auto f, auto &dirty, auto &&...deps) {
      return [&, f](auto &&...observers) {
        auto cache = std::optional<decltype(f())>();

        auto updater = [&, f](auto &cache) {
          if (not dirty) { (check_dep(deps), ...); }

          if (not dirty) return false;

          const auto value = f();
          dirty = false;

          if (value == std::exchange(cache, value)) return false;

          ((observers && (*observers = true)), ...);

          return true;
        };

        auto getter = [updater, cache = std::move(cache)] mutable {
          updater(cache);
          return cache.value();
        };

        return getter;
      };
    };

    auto autorun2 = [&](auto f, auto &&...args) {
      return calc2(
        [=] {
          f();
          return 0;
        },
        std::forward<decltype(args)>(args)...);
    };

    // unconditional observers
    auto full_name_dirty = true;
    auto display_name_dirty = true;
    auto autorun_dhl_dirty = true;
    auto autorun_print_at_home_dirty = true;
    auto autorun_extra_dirty = true;
    auto is_writer_dirty = true;
    auto business_card_dirty = true;

    // conditional observers
    bool *first_name_observers_autorun_extra = nullptr;
    bool *full_name_observers_display_name = nullptr;
    bool *is_writer_observers_autorun_extra = nullptr;
    bool *business_card_observers_autorun_dhl = nullptr;
    bool *business_card_observers_autorun_print_at_home = nullptr;

    // atoms
    auto [first_name, set_first_name] =
      atom2("John"s, &full_name_dirty, first_name_observers_autorun_extra);
    auto [last_name, set_last_name] = atom2("Doe"s, &full_name_dirty);
    auto [pseudonym, set_pseudonym] =
      atom2(std::optional<std::string>{}, &display_name_dirty);
    auto [shipment, set_shipment] =
      atom2(shipment_t::dhl, &autorun_dhl_dirty, &autorun_print_at_home_dirty);
    auto [enable_extra, set_enable_extra] = atom2(false, &autorun_extra_dirty);

    // calcs
    auto full_name = calc2(
      [&] {
        msgs.insert("full_name");
        return first_name() + " " + last_name();
      },
      full_name_dirty)(full_name_observers_display_name);

    decltype(full_name) *display_name_dependencies_full_name = nullptr;
    auto display_name = calc2(
      [&] {
        full_name_observers_display_name = nullptr;
        display_name_dependencies_full_name = nullptr;

        msgs.insert("display_name");
        if (pseudonym().has_value()) return pseudonym().value();
        const auto res = full_name();
        full_name_observers_display_name = &display_name_dirty;
        display_name_dependencies_full_name = &full_name;
        return res;
      },
      display_name_dirty,
      pseudonym,
      display_name_dependencies_full_name)(&business_card_dirty,
                                           &is_writer_dirty);

    auto is_writer = calc2(
      [&] {
        msgs.insert("is_writer");
        return expensive_author_registry_lookup(display_name());
      },
      is_writer_dirty,
      display_name)(&business_card_dirty, is_writer_observers_autorun_extra);

    auto business_card = calc2(
      [&] {
        msgs.insert("business_card");
        return std::format("Business card of {}{}",
                           display_name(),
                           is_writer() ? ", writer" : "");
      },
      business_card_dirty,
      display_name,
      is_writer)(business_card_observers_autorun_dhl,
                 business_card_observers_autorun_print_at_home);

    // autoruns
    decltype(business_card) *autorun_dhl_dependencies_business_card = nullptr;
    auto autorun_dhl = autorun2(
      [&] {
        business_card_observers_autorun_dhl = nullptr;
        autorun_dhl_dependencies_business_card = nullptr;

        msgs.insert("autorun:dhl");
        if (shipment() == shipment_t::dhl) {
          ship_via_dhl(msgs, business_card());
          business_card_observers_autorun_dhl = &autorun_dhl_dirty;
          autorun_dhl_dependencies_business_card = &business_card;
        }
      },
      autorun_dhl_dirty,
      autorun_dhl_dependencies_business_card)();

    decltype(business_card) *autorun_print_at_home_dependencies_business_card =
      nullptr;
    auto autorun_print_at_home = autorun2(
      [&] {
        business_card_observers_autorun_print_at_home = nullptr;
        autorun_print_at_home_dependencies_business_card = nullptr;

        msgs.insert("autorun:print_at_home");
        if (shipment() == shipment_t::print_at_home) {
          email(msgs, business_card());
          business_card_observers_autorun_print_at_home =
            &autorun_print_at_home_dirty;
          autorun_print_at_home_dependencies_business_card = &business_card;
        }
      },
      autorun_print_at_home_dirty,
      autorun_print_at_home_dependencies_business_card)();

    decltype(is_writer) *autorun_extra_dependencies_is_writer = nullptr;
    auto autorun_extra = autorun2(
      [&] {
        is_writer_observers_autorun_extra = nullptr;
        autorun_extra_dependencies_is_writer = nullptr;
        first_name_observers_autorun_extra = nullptr;

        msgs.insert("autorun:extra");
        if (enable_extra()) {
          is_writer();
          is_writer_observers_autorun_extra = &autorun_extra_dirty;
          autorun_extra_dependencies_is_writer = &is_writer;

          first_name();
          first_name_observers_autorun_extra = &autorun_extra_dirty;
        }
      },
      autorun_extra_dirty,
      autorun_extra_dependencies_is_writer)();

    update = [&] {
      autorun_dhl();
      autorun_print_at_home();
      autorun_extra();
    };

    update();

    test_business_card(msgs,
                       assigner{set_first_name},
                       assigner{set_last_name},
                       assigner{set_pseudonym},
                       assigner{set_shipment},
                       assigner{set_enable_extra});

    // TODO: The manual and encapsulated versions encode a
    // _fixed_ dependency graph, which is not the case for
    // the prego version.
    // TODO: dep management is non-trivial, still insufficient and intrusive
    // TODO: Observers version
    // TODO: are unchanged atoms handled properly?
  };
};
