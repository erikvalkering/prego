#include "common.h"

using prego::spy;

// TODO: 3 levels of tests:
// 1. Consistency: values should always be up to date. If a calc is outdated, it
// should recalculate. Efficiency is not important.
// 2. Efficiency: values should be made up to date with the least amount of
// recalculations.
// 2.1. [Lazy] No repeated calculations: if a calc is up to date, it should not
// be recalculated anymore.
// 2.2. [Reactive] No unnecessary recalculations: if a calc is not reactive, it
// should not be recalculated.
// 3. Optimality: determine in as few steps as possible whether a calc is up to
// date.

static suite<"basics"> _ = [] {
  "atom"_test = [] {
    auto a = atom{42};

    expect(a() == 42_i) << "state should be accessible directly";

    a.set(1729);
    expect(a() == 1729_i) << "mutations should be allowed and observable";
  };

  "calc"_test = [] {
    auto a = atom{42};

    auto b = calc{[=](auto get) { return 2 * get(a); }};

    expect(b() == _i(2 * 42))
        << "calculated state should be accessible directly";

    a.set(1729);
    expect(b() == _i(2 * 1729)) << "mutations should be allowed and observable "
                                   "through calculated state";
  };

  "lazy_observing"_test = [] {
    auto a = atom{42};

    auto x = false;
    auto b = calc{[=, &x](auto get) {
      x = true;
      get(a);
      return 0;
    }};

    expect(not x) << "calculated state should not calculate when not requested";
    b();
    expect(x) << "calculated state should calculate the first time when "
                 "requested";

    x = false;
    b();
    expect(not x)
        << "calculated state should not recalculate when dependencies "
           "have not changed";

    x = false;
    a.set(1729);
    expect(not x) << "calculated state should not be reactive if not "
                     "reactively observed";
    b();
    expect(x) << "calculated state should recalculate when dependencies "
                 "have changed";
  };

  "dynamic_reactions"_test = [] {
    auto first_name = atom{"John"s};
    auto last_name = atom{"Doe"s};
    auto nick_name = atom{""s};

    auto x = false;
    auto full_name = calc{[=, &x](auto get) {
      x = true;
      auto value = get(first_name) + " " + get(last_name);
      return value;
    }};

    auto y = false;
    auto display_name = calc{[=, &y](auto get) {
      y = true;
      if (get(nick_name) != "") {
        auto value = get(nick_name);
        return value;
      } else {
        auto value = get(full_name);
        return value;
      }
    }};

    auto z = false;
    auto enabled = atom{true};
    autorun([=, &z](auto get) {
      z = true;
      if (get(enabled)) {
        auto value = get(display_name);
      }
    });

    expect(x) << "full_name should be calculated";
    expect(y) << "display_name should be calculated";
    expect(z) << "autorun should execute immediately";

    x = false;
    y = false;
    z = false;

    first_name.set("Jane");
    expect(x) << "full_name should react to change of first_name";
    expect(y) << "display_name should react to change of full_name";
    expect(z) << "autorun should react to change of display_name";

    x = false;
    y = false;
    z = false;

    nick_name.set("Jane Doe");
    expect(not x) << "full_name should not react to change of nick_name";
    expect(y) << "display_name should react to change of nick_name";
    expect(not z)
        << "autorun should not react because display_name did not change";

    x = false;
    y = false;
    z = false;

    first_name.set("John");
    expect(not x)
        << "full_name should not react to change of first_name, because "
           "full_name is not being observed";
    expect(not y) << "display_name should not react, because it is "
                     "not observing full_name";
    expect(not z) << "autorun should not react, because nothing changed";

    x = false;
    y = false;
    z = false;

    enabled.set(false);
    expect(not x) << "full_name should not react to change of enabled";
    expect(not y) << "display_name should not react, because "
                     "nick_name did not change";
    expect(z) << "autorun should react to change of enabled";

    x = false;
    y = false;
    z = false;

    nick_name.set("John Doe");
    expect(not x)
        << "full_name should not react to change of nick_name, because it "
           "does not observe it";
    expect(not y)
        << "display_name should not react to change of nick_name, because "
           "display_name is not being observed right now";
    expect(not z)
        << "autorun should not react to change of nick_name, because it is "
           "not being observed right now (through display_name)";

    x = false;
    y = false;
    z = false;

    // TODO: perhaps move to separate unit test:
    //   Reactivity triggered "from above",
    //   i.e. observers that were triggered and
    //   query observables that were not triggered.
    //   This is different from "from below",
    //   where observers react because one of
    //   their observables (directly or indirectly)
    //   got changed.
    //   It matters, because those observables should
    //   be invalidated even if they are not being observed
    enabled.set(true);
    expect(not x) << "full_name should not recalculate because it is still "
                     "not being observed";
    expect(y) << "display_name should recalculate, because it is "
                 "being observed again";
    expect(z) << "autorun should react to change of enabled, as well as "
                 "display_name that was changed";

    x = false;
    y = false;
    z = false;

    nick_name.set("");
    expect(x) << "full_name should be queried through display_name and "
                 "recalculate "
                 "because it was previously changed";
    expect(y) << "display_name should recalculate, because nick_name "
                 "changed, as well "
                 "as full_name";
    expect(not z)
        << "autorun should not react, because display_name did not change";
    // TODO: rename autorun to observe or observer or reaction or effect? or
    // reactive? or watch?

    enabled.set(false);
    x = false;
    y = false;
    z = false;

    enabled.set(true);
    expect(not x) << "full_name should not recalculate because nothing changed";
    expect(not y) << "display_name should recalculate, because nothing changed";
    expect(z) << "autorun should react to change of enabled";
  };

  "autorun"_test = [] {
    {
      auto a = atom{42};
      auto x = false;
      autorun([&x](auto get) { x = true; });
      expect(x) << "should execute immediately";

      x = false;
      a.set(42);
      expect(not x) << "should not react on mutations of a";

      a.set(1729);
      expect(not x) << "should not react on mutations of a";
    }

    {
      auto a = atom{42};
      auto x = false;
      autorun([=, &x](auto get) {
        get(a);
        x = true;
      });
      expect(x) << "should execute immediately";

      x = false;
      a.set(42);
      expect(not x) << "should not react if a did not change";

      a.set(1729);
      expect(x) << "should react on mutations of a";
    }

    {
      // tests capture by-value (shared ownership of atom)
      auto a = atom{42};
      auto x = false;
      autorun([=, &x](auto get) {
        get(a);
        x = true;
      });
      expect(x) << "should execute immediately";

      x = false;
      a.set(42);
      expect(not x) << "should not react if a did not change";
      a.set(1729);
      expect(x) << "should react on mutations of a";
    }
  };

  "mixed_observing"_test = [] {
    atom x = 42;
    calc y = [=] { return x(); };
    calc z = [=] { return y(); };

    auto b = false;
    autorun([=, &b] {
      b = true;
      z();
    });
    expect(b) << "autorun should execute immediately";

    b = false;
    x = 1729;
    expect(b) << "autorun should react to x";

    z();

    b = false;
    x = 42;
    expect(b) << "autorun should react to x";
  };

  "deterministic_ordering_observers"_test = [] {
    atom a = 42;
    atom b = 1729;
    atom x = true;
    atom y = true;
    auto order = ""s;
    calc c = [=, &order] {
      order += "c";
      return x() ? a() : b();
    };
    calc d = [=, &order] {
      order += "d";
      return y() ? a() : b();
    };
    autorun([=] {
      c();
      d();
    });

    expect(that % order == "cd"s);

    order = "";
    a = 0;
    expect(that % order == "cd"s);

    order = "";
    b = 0;
    expect(that % order == ""s);

    order = "";
    y = false;
    expect(that % order == "d"s);

    order = "";
    x = false;
    expect(that % order == "c"s);

    order = "";
    b = 1;
    expect(that % order == "dc"s);
  };

  "not_so_deterministic_ordering_observers"_test = [] {
    auto order = ""s;
    auto tag = [&](auto name) {
      return spy([name, &order] { order += name; });
    };

    atom a = 42;

    atom x = true;
    autorun([=] { x ? a() : 0; } + tag("x"));

    atom y = true;
    autorun([=] { y ? a() : 0; } + tag("y"));

    order = "";
    a = a + 1;
    expect(that % order == "xy"s);

    x = false;
    x = true;

    order = "";
    a = a + 1;
    expect(that % order == "yx"s);
  };

  "lazy"_test = [] {
    auto z = false;
    calc c = [&z] {
      z = true;
      return 42;
    };

    expect(that % c == 42_i);
    expect(that % z == true);

    z = false;
    expect(that % c == 42_i);
    expect(that % z == false);
  };

  "lazy_evaluating_reactive_calc"_test = [] {
    atom a = true;

    auto z = false;
    calc b = [=, &z] {
      z = true;
      // TODO: returning a doesn't observe a
      return a();
    };

    // observe a (reactively)
    autorun([=] { a() ? true : b(); });

    // observe a indirectly (reactively)
    autorun([=] { b(); });

    // now the first autorun will also observe b (reactively)
    a = false;

    // because b is reactive, it should be up to date and no recalculate should
    // be needed.
    z = false;
    b();
    expect(that % z == false);
  };

  "lazy_evaluating_nonreactive_calc"_test = [] {
    atom a = true;

    calc b = [=] {
      a();
      return 42;
    };

    auto z = false;
    calc c = [=, &z] {
      z = true;
      return b();
    };

    // observe a (reactively)
    autorun([=] { a() ? 1729 : b(); });
    expect(that % z == false);

    // Make sure c is calculated and observes b
    c();
    expect(that % z == true);

    // Sanity check: c should be up to date
    z = false;
    c();
    expect(that % z == false);

    // now the first autorun will also observe b (reactively)
    a = false;
    expect(that % z == false);

    // because c was calculated before and b did not change,
    // it should not recalculate.
    z = false;
    c();
    expect(that % z == false);
  };

  "stale_count_nonzero_requires_uptodate_traversal"_test = [] {
    atom a = true;

    auto enabled = false;
    auto x = false;
    calc b = [&, a] {
      if (enabled)
        x = true;
      return a();
    };

    autorun([&, a, b] {
      enabled = true;
      a() ? true : b();
      enabled = false;
    });

    autorun([=] { b(); });

    expect(that % x == false);
    a = false;

    // The first autorun should calculate b because it might be changed
    // (stale_count is 1, so we don't yet know whether a changed).
    expect(that % x == true);
  };

  "changed_notification_should_not_mark_previously_calculated_calcs_as_maybe_changed"_test =
      [] {
        atom a = true;

        auto counter_idx = 0;
        auto counters = std::array{0, 0};
        calc b = [&, a] {
          ++counters[counter_idx];
          return a();
        };

        autorun([&, a, b] {
          counter_idx = 1;
          a() ? true : b();
          counter_idx = 0;
        });
        autorun([=] { b(); });

        counters = {};
        a = false;

        // The first autorun should calculate b because it might be changed
        // (stale_count is 1, so we don't yet know whether a changed).
        expect(counters[1] == 1_i);

        // However, because the second autorun is also indirectly
        // observing a, it will notify b that a changed.
        // This should however *not* result in b being marked maybe_changed,
        // because it was just calculated.
        expect(counters[0] == 0_i);
      };

  "is_up_to_date_not_changed"_test = [] {
    atom a = 42;
    calc b = [=] {
      a();
      return 0;
    };

    auto x = false;
    calc c = [=, &x] {
      x = true;
      return b();
    };

    c(); // Make sure b and c are calculated

    a = 1729; // invalidate b

    x = false;
    c(); // b gets recalculated but doesn't change. Therefore, c doesn't need to
         // be recalculated.
    expect(that % x == false);
  };
};
