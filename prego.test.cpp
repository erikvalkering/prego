#include "prego.h"

#include <fmt/core.h>
#include <fmt/ranges.h>

#include <boost/ut.hpp>

#include <string>
#include <tuple>
#include <utility>
#include <vector>

template <> struct fmt::formatter<std::vector<bool>> {
  constexpr auto parse(format_parse_context &ctx) { return ctx.begin(); }

  template <typename FormatContext>
  auto format(const std::vector<bool> &v, FormatContext &ctx) const {
    return fmt::format_to(ctx.out(), "[{}]", fmt::join(v, ", "));
  }
};

using namespace std::string_literals;
using namespace boost::ut;

using prego::atom;
using prego::autorun;
using prego::calc;
using prego::global_scope_manager;
using prego::insertion_order_map;
using prego::make_atom;
using prego::scope_manager_t;

auto to_vector(auto &&rng) {
  using T = std::ranges::range_value_t<decltype(rng)>;

  auto r = std::vector<T>{};
  for (auto &&x : rng)
    r.push_back(x);

  return r;
}

auto sandbox() {
  auto first_name = atom{"Anita"s};
  auto last_name = atom{"Laera"s};
  auto nick_name = atom{""s};

  auto full_name = calc{[=](auto get) {
    std::cout << "calc full_name\n";
    if (get(nick_name) != "")
      return get(nick_name);
    else
      return get(first_name) + " " + get(last_name);
  }};

  auto display_full = atom{true};
  autorun([=](auto get) {
    std::cout << "calc autorun\n";
    if (get(display_full)) {
      const auto n = get(full_name);
      std::cout << ">> " << n << std::endl;
    } else
      std::cout << "disable autorun\n";
  });

  // Anita Laera
  first_name.set("Missi");
  // full_name >> autorun >> Missi Laera
  last_name.set("Valkering");
  // full_name >> autorun >> Missi Valkering
  first_name.set("Erik");
  // full_name >> autorun >> Erik Valkering
  nick_name.set("Erik Valkering");
  // full_name
  nick_name.set("Erik Engelbertus Johannes Valkering");
  // full_name >> autorun >> Erik Engelbertus Johannes Valkering
  display_full.set(false);
  // autorun >> disable
  nick_name.set("Ciri");
}

class lifetime_tracker {
  std::weak_ptr<int> p;

public:
  auto alive() const { return !p.expired(); }

  auto track() {
    auto sp = std::make_shared<int>();
    p = sp;
    return sp;
  }
};

struct immovable {
  int x = 42;
  int y = 1729;

  immovable() = default;
  immovable(int x) : x{x} {}
  immovable(int x, int y) : x{x}, y{y} {}

  explicit immovable(bool) {}
  explicit immovable(bool, bool) {}

  immovable(const immovable &) = delete;
  immovable(immovable &&) = delete;

  auto operator<=>(const immovable &) const = default;

  friend auto &operator<<(std::ostream &os, const immovable &o) {
    return os << "immovable{" << o.x << ", " << o.y << "}";
  }
};

// struct Person_ {
//   atom<std::string> first_name;
//   atom<std::string> last_name;
//
//   auto full_name(auto get) const {
//     return get(first_name) + " " + get(last_name);
//   }
// };
//
// struct Person : prego::Calculatable<Person_> {
//   /*    using full_name_thunk = Thunk<Class, [](auto &self, auto get) {
//       return self.full_name(get);
//       }>;
//
//       calc<full_name_thunk> full_name{this};*/
//   // PREGO_CALCULATED(full_name);
// };

#define CONCAT2(a, b) a##b
#define CONCAT(a, b) CONCAT2(a, b)
#define _ CONCAT(placeholder_, __LINE__)

suite<"basics"> _ = [] {
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
    expect(x)
        << "calculated state should calculate the first time when requested";

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
    expect(x)
        << "calculated state should recalculate when dependencies have changed";
  };

  "dynamic_reactions"_test = [] {
    auto first_name = atom{"John"s};
    auto last_name = atom{"Doe"s};
    auto nick_name = atom{""s};

    auto x = false;
    auto full_name = calc{[=, &x](auto get) {
      x = true;
      auto value = get(first_name) + " " + get(last_name);
      prego::log(1, "full_name = ", value);
      return value;
    }};

    auto y = false;
    auto display_name = calc{[=, &y](auto get) {
      y = true;
      if (get(nick_name) != "") {
        auto value = get(nick_name);
        prego::log(1, "display_name = ", value);
        return value;
      } else {
        auto value = get(full_name);
        prego::log(1, "display_name = ", value);
        return value;
      }
    }};

    auto z = false;
    auto enabled = atom{true};
    autorun([=, &z](auto get) {
      z = true;
      if (get(enabled)) {
        auto value = get(display_name);
        prego::log(1, "autorun = ", value);
      } else
        prego::log(1, "autorun = disabled");
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
    expect(x)
        << "full_name should be queried through display_name and recalculate "
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
};

suite<"lifetimes"> _ = [] {
  "scope_manager"_test = [] {
    auto a = atom{42};

    auto x = false;
    auto y = false;
    auto z = false;
    auto w = false;

    {
      {
        auto scope_manager = scope_manager_t{};

        {
          autorun([=, &x](auto get) {
            get(a);
            x = true;
          });
          autorun(
              [=, &y](auto get) {
                get(a);
                y = true;
              },
              &scope_manager);
          auto _ = autorun(
              [=, &z](auto get) {
                get(a);
                z = true;
              },
              nullptr);
          std::ignore = autorun(
              [=, &w](auto get) {
                get(a);
                w = true;
              },
              nullptr);

          expect(x) << "sanity check";
          expect(y) << "sanity check";
          expect(z) << "sanity check";
          expect(w) << "sanity check";

          x = false;
          y = false;
          z = false;
          w = false;
          a.set(1729);
          expect(x) << "autorun should be kept alive by global_scope_manager";
          expect(y) << "autorun should be kept alive by local scope_manager";
          expect(z) << "autorun should be kept alive by local variable";
          expect(not w) << "autorun should be destroyed immediately";
        }

        x = false;
        y = false;
        z = false;
        w = false;
        a.set(42);
        expect(x) << "autorun should be kept alive by global_scope_manager";
        expect(y) << "autorun should be kept alive by local scope_manager";
        expect(not z) << "autorun should be destroyed by local variable";
        expect(not w) << "autorun should be destroyed immediately";
      }

      x = false;
      y = false;
      z = false;
      w = false;
      a.set(1729);
      expect(x) << "autorun should be kept alive by global_scope_manager";
      expect(not y) << "autorun should be destroyed by local scope_manager";
      expect(not z) << "autorun should be destroyed by local variable";
      expect(not w) << "autorun should be destroyed immediately";
    }

    global_scope_manager.clear();
    x = false;
    y = false;
    z = false;
    w = false;
    a.set(42);
    expect(not x) << "autorun should be destroyed by global_scope_manager";
    expect(not y) << "autorun should be destroyed by local scope_manager";
    expect(not z) << "autorun should be destroyed by local variable";
    expect(not w) << "autorun should be destroyed immediately";
  };

  "auto_unobserve"_test = [] {
    auto a = atom{42};
    auto x = false;
    {
      // TODO: autorun's lifetime should be managed by atom a
      // TODO: a's lifetime should also be managed by autorun
      // TODO: this way, we don't need the global scope_manager
      // TODO: how to resolve this cyclic ownership dependency?
      auto _ = autorun(
          [=, &x](auto get) {
            get(a);
            x = true;
          },
          nullptr);
    }

    x = false;
    a.set(1729);
    expect(not x) << "autorun should definitely not be invoked";
    expect(std::size(a.observers()) == 0_i)
        << "autorun should not be referenced anymore";
  };

  "lifetimes"_test = [] {
// TODO: full gc-like lifetime management without need for global scope manager
#if 0
    auto a_lt = lifetime_tracker{};
    auto autorun_lt = lifetime_tracker{};
    {
        auto a = atom{a_lt.track()};
        {
            autorun([=, x = autorun_lt.track()](auto get) { get(a); });
        }
        expect(autorun_lt.alive())    << "autorun should be kept alive by a";
    }
    expect(not a_lt.alive()) << "a should be destroyed";
    expect(not autorun_lt.alive()) << "autorun should be destroyed";
#endif
    auto b_lt = lifetime_tracker{};
    auto c_lt = lifetime_tracker{};
    {
      auto c = [&] {
        expect(not b_lt.alive()) << "b should not be alive";
        auto b = atom{b_lt.track()};
        expect(b_lt.alive()) << "b should be alive";

        return calc{[=, x = c_lt.track()](auto get) { return get(b); }};
      }();

      expect(b_lt.alive()) << "b should be kept alive by c";
    }
    expect(not b_lt.alive()) << "b should be destroyed";
    expect(not c_lt.alive()) << "c should be destroyed";
  };
};

suite<"type support"> _ = [] {
  "moveonly_types"_test = [] {
    struct moveonly {
      moveonly() = default;
      moveonly(const moveonly &) = delete;
      moveonly(moveonly &&) = default;

      auto operator<=>(const moveonly &) const = default;
    };

    atom a = moveonly{};

    calc b = [=] {
      a();
      return moveonly{};
    };

    calc c = [=](auto get) {
      get(a);
      return moveonly{};
    };

    autorun([=] {
      a();
      b();
      c();
    });

    autorun([=](auto get) {
      get(a);
      get(b);
      get(c);
    });
  };

  "immovable_atom_types"_test = [] {
    immovable a = immovable{};
    // not allowed: atom b = immovable{};

    atom<immovable> c = atom<immovable>{};
    atom d = atom<immovable>{};
    auto e = atom<immovable>{};

    // Implicit constructors
    atom f = atom<immovable>{42};
    atom g = atom<immovable>{std::in_place, 42, 1729};

    // Explicit constructors
    atom h = atom<immovable>{true};
    atom i = atom<immovable>{std::in_place, true, true};

    // Concise syntx
    atom<immovable> j = {std::in_place, 42, 1729};
    atom k = {std::in_place_type<immovable>, 42, 1729};
    atom l = {std::in_place_type<immovable>, 42, 1729};

    // Factory function
    atom m = make_atom<immovable>(42, 1729);

    atom n = make_atom<immovable>(1729, 42);
    expect(n().x == 1729_i);
    expect(n().y == 42_i);

    atom o = make_atom<immovable>(42, 1729);
    auto triggered = false;
    autorun([=, &triggered] {
      o();
      triggered = true;
    });
    triggered = false;

    o.emplace(42, 1729);
    expect(not triggered)
        << "equal immovable types should not trigger recalculation";

    o.emplace(1729, 42);
    expect(triggered) << "unequal immovable types should trigger recalculation";
  };

  "immovable_calc_types"_test = [] {
    calc a = [] { return immovable{42, 1729}; };

    atom b = 42;
    calc c = [=] { return immovable{b(), 1729}; };

    atom d = atom<immovable>{};
    calc e = [=] { return immovable{d().x, 1729}; };

    calc f = [=](auto get) {
      get(e);
      return immovable{};
    };

    calc g = [] { return immovable{1729, 42}; };
    expect(g().x == 1729_i);
    expect(g().y == 42_i);

    atom h = 0;
    calc i = [=] { return immovable{h() < 2 ? 42 : 1729}; };
    auto triggered = false;
    autorun([=, &triggered] {
      i();
      triggered = true;
    });
    triggered = false;

    h = 1;
    expect(not triggered)
        << "equal immovable types should not trigger recalculation";

    h = 2;
    expect(triggered) << "unequal immovable types should trigger recalculation";
  };

  "nondefault_constructible_types"_test = [] {
    struct Foo {
      Foo(int) {}
      auto operator<=>(const Foo &) const = default;
    };

    atom a = Foo{42};

    calc b = [=] {
      a();
      return Foo{1729};
    };

    autorun([=] {
      a();
      b();
    });
  };
};

suite<"syntaxes"> _ = [] {
  "atom_syntaxes"_test = [] {
    auto a = atom{42};
    atom b = 42;
    // alternative syntax atom: function instead of class
    //   which might be more flexible in the design space of the
    //   implementation. Same for calc.
    auto c = atom(42);

    // mutation syntax 1: set
    a.set(1729);

    // mutation syntax 2: direct assignment
    a = 1729;
  };

  "calc_syntaxes"_test = [] {
    auto a = atom{42};
    auto b = atom{42};

    // calc syntax 1: get as parameter
    //   - thread-safe
    //   - may allow for optimizations
    auto c = calc{[=](auto get) { return get(a) + get(b); }};
    calc d = [=](auto get) { return get(a) + get(b); };

    // calc syntax 2: parameterless function
    //   - not thread-safe
    //   - maybe more difficult to optimize
    //   - less boilerplate
    auto e = calc{[=] { return a() + b(); }};

    // calc syntax 3: parameterless function + smartref
    //   - not thread-safe
    //   - maybe more difficult to optimize
    //   - minimal boilerplate (except for user defined types)
    //   - maybe not worth the effort, considering only 2 chars per
    //   observable
    // auto f = calc{[=] { return a + b; }};

    // calc syntax 4: static get
    //   - thread-safe
    //   - may allow for more optimization
    //   - every atom/calc should be declared globally and becomes stateless
    //   - does every observable have to be unique?
    // auto g = calc{[](auto get) { return get(a) + get(b); }};
  };

  // "oo"_test = [] {
  //   {
  //     auto john = Person{"John"s, "Doe"s};
  //     auto jane = Person{"Jane", "Doe"};

  //     // assert_eq(john.full_name(), "John Doe", "John and Jane should not
  //     // share state"); assert_eq(jane.full_name(), "Jane Doe", "John and
  //     // Jane should not share state");
  //   }

  //   {
  //     using Person = decltype([] {
  //       atom first_name{"John"s};
  //       atom last_name{"Doe"s};

  //       calc full_name = [=](auto get) {
  //         return get(first_name) + " " + get(last_name);
  //       };

  //       struct Person {
  //         decltype(first_name) first_name{first_name};
  //         decltype(last_name) last_name{last_name};
  //         // decltype(full_name) full_name{full_name};
  //       };

  //       return Person{};
  //     }());

  //     auto john = Person{"John"s, "Doe"s};
  //     auto jane = Person{"Jane", "Doe"};
  //   }

  //   class atom_vector {
  //     std::vector<int> v;

  //   public:
  //     void push_back(int x) {}
  //   };
  // };

  "simple_syntax"_test = [] {
    atom x = 42;
    calc y = [=] { return x(); };
    autorun([=] { y(); });

    x = 1729;
  };
};

suite<"graph traversal efficiency"> _ = [] {
  "graph_traversal_efficiency_basics"_test = [] {
    atom a = 42;
    a();
    expect(a.state->is_up_to_date_counter == 0_i)
        << "accessing a should directly return the value, without checking "
           "if "
           "it's up to date (because it always is)";

    calc b = [=] { return a(); };
    b();
    expect(a.state->is_up_to_date_counter == 0_i)
        << "b is initially not calculated yet, so we know it is not up to "
           "date and don't need to check a whether it is up to date";
  };

  "graph_traversal_efficiency_lazy"_test = [] {
    atom a = 42;
    calc b = [=] { return a(); };

    calc c = [=] { return b(); };
    calc d = [=] { return b(); };
    calc e = [=] {
      c();
      d();
      return 0;
    };

    a.state->is_up_to_date_counter = 0;
    e();
    expect(a.state->is_up_to_date_counter == 0_i)
        << "b is initially not calculated yet, so we know it is not up to "
           "date and don't need to check a whether it is up to date";

    e();
    expect(a.state->is_up_to_date_counter == 1_i)
        << "b should only check a once for determining whether it is up to "
           "date";
  };

  skip / "graph_traversal_efficiency_reactive_bottom_up"_test = [] {
    atom a = 42;
    calc b = [=] { return a(); };

    atom f = true;
    calc g = [=] { return f() ? b() : 0; };
    calc h = [=] { return f() ? 0 : b(); };

    // Note: this autorun is only to ensure that g and h are observed.
    //   We don't really care whether the autorun itself gets
    //   re-evaluated or not. So, if g or h after being
    //   re-evaluated did not change, it's not a problem that this
    //   won't propagate upwards.
    autorun([=] {
      g();
      h();
    });

    // TODO: two tests:
    // 2. making b unreactive wrt g, does that require a full is_up_to_date
    // traversal for b through h?

    // Here we are testing what happens if g and h are being observed (through
    // autorun), but initially only g observes b. After changing f to false,
    // depending on the order of registration, b might no longer become
    // observed, and therefore not reactive anymore. Because of that, if h is
    // evaluated after g, it will start depending on b, but cannot immediately
    // determine it is up to date, due to it not being reactive anymore. We
    // actually want to make sure that b is still up-to-date, even if at some
    // intermediate state, it is not being observed.
    a.state->is_up_to_date_counter = 0;
    f = false;
    expect(a.state->is_up_to_date_counter == 0_i)
        << "b should not need to check a for determining whether it is up to "
           "date, because it was reactive after all";
  };

  skip / "graph_traversal_efficiency_reactive_bottom_up_top_down"_test = [] {
    atom a = 42;
    calc b = [=] { return a(); };

    atom f = true;
    calc g = [=] { return f() ? b(), true : false; };

    autorun([=] {
      if (!g())
        b();
    });

    a.state->is_up_to_date_counter = 0;
    f = false;
    expect(a.state->is_up_to_date_counter == 0_i)
        << "b should not need to check a for determining whether it is up "
           "to date, because it was reactive after all";
  };

  // This unit test makes sure that calls to unobserve() don't trigger
  // unnecessary reactive state propagation (through observe()).
  "unobserve_efficiency"_test = [] {
    atom a = 42;

    calc b = [=] { return a(); };

    {
      auto autorun_1 = autorun([=] { b(); }, nullptr);

      {
        auto autorun_2 = autorun([=] { b(); }, nullptr);

        {
          calc c = [=] { return b(); };
          c();

          a.state->observe_counter = 0;
        }

        expect(a.state->observe_counter == 0_i)
            << "[true, true, false] => [true, true] => no propagation";

        a.state->observe_counter = 0;
      }

      expect(a.state->observe_counter == 0_i)
          << "[true, true] => [true] => no propagation";
    }

    expect(a.state->observe_counter == 1_i) << "[true] => [] => propagation";

    {
      calc c = [=] { return b(); };
      c();

      {
        auto autorun_1 = autorun([=] { b(); }, nullptr);
        a.state->observe_counter = 0;
      }

      expect(a.state->observe_counter == 1_i)
          << "[false, true] => [false] => propagation";

      a.state->observe_counter = 0;
    }

    expect(a.state->observe_counter == 0_i)
        << "[false] => [] => no propagation";
  };
};

int main() {
  auto x = std::make_shared<int>(42);
  auto y = std::make_shared<int>(1729);

  "insertion_order_map"_test =
      [](auto data) {
        auto [key0, key1, cmp] = data;
        using key_t = decltype(key0);
        using cmp_t = decltype(cmp);

        auto eq = [=](auto lhs, auto rhs) {
          return not cmp(lhs, rhs) and not cmp(rhs, lhs);
        };

        auto m = insertion_order_map<key_t, int, cmp_t>{};

        expect(m.size() == 0_i);
        expect(m.begin() == m.end());
        expect(not m.contains(key0));
        expect(that % m[key0] == 0);
        expect(m.size() == 1_i);
        expect(m.begin() != m.end());
        expect(m.contains(key0));
        expect(that % (m[key0] = 42) == 42);

        auto n = m.extract(key0);
        expect(not n.empty());
        expect(that % eq(n.key(), key0));
        expect(that % n.mapped() == 42);
        expect(m.size() == 0_i);

        n = m.extract(key0);
        expect(n.empty());
        expect(m.size() == 0_i);

        expect(that % (m[key0] = 42) == 42);
        expect(that % (m[key1] = 1729) == 1729);
        expect(m.size() == 2_i);
        expect(m.begin() != m.end());

        auto keys = to_vector(m | std::views::keys);
        expect(that % eq(keys[0], key0));
        expect(that % eq(keys[1], key1));
      } |
      std::tuple{
          std::tuple{42, 1729, std::less{}},
          std::tuple{std::weak_ptr{x}, std::weak_ptr{y}, std::owner_less{}},
      };
}
