#include "common.h"

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
