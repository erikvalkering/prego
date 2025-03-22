#include <fmt/core.h>
#include <fmt/ranges.h>

#include <string>
#include <utility>

#include "common.h"

template <> struct fmt::formatter<std::vector<bool>> {
  constexpr auto parse(format_parse_context &ctx) { return ctx.begin(); }

  template <typename FormatContext>
  auto format(const std::vector<bool> &v, FormatContext &ctx) const {
    return fmt::format_to(ctx.out(), "[{}]", fmt::join(v, ", "));
  }
};

using namespace std::string_literals;

using prego::make_atom;

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

int main() {}
