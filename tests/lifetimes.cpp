#include "common.h"

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
