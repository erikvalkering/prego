#include "common.h"

#include <type_traits>

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

template <class C> struct class_template_traits;
template <template <typename> class C, typename T>
struct class_template_traits<C<T>> {
  static constexpr bool is_reference_v = std::is_reference_v<T>;
};

static suite<"type support"> _ = [] {
  "atom ctad"_test = [] {
    auto c = 1729;

    atom x = 42;
    atom y = c;

    static_assert(std::same_as<decltype(x), atom<int>>);
    static_assert(std::same_as<decltype(y), atom<int>>);
  };

  "calc ctad"_test = [] {
    auto l = [] { return 1729; };

    calc x = [] { return 42; };
    calc y = l;

    static_assert(not class_template_traits<decltype(x)>::is_reference_v);
    static_assert(not class_template_traits<decltype(y)>::is_reference_v);
  };

  "moveonly_types"_test = [] {
    struct moveonly {
      moveonly() = default;

      moveonly(const moveonly &) = delete;
      moveonly(moveonly &&) = default;
      moveonly &operator=(const moveonly &) = default;
      moveonly &operator=(moveonly &&) = default;

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

  "optional atom"_test = [] {
    atom<std::optional<int>> a = std::nullopt;
    atom<std::optional<int>> b = 42;
    atom c = std::optional<int>{};
    atom d = std::optional<int>{42};

    static_assert(std::same_as<decltype(auto{a()}), std::optional<int>>);
    static_assert(std::same_as<decltype(auto{b()}), std::optional<int>>);
    static_assert(std::same_as<decltype(auto{c()}), std::optional<int>>);
    static_assert(std::same_as<decltype(auto{d()}), std::optional<int>>);

    expect(a() == std::nullopt);
    expect(b() == 42);
    expect(c() == std::nullopt);
    expect(d() == 42);
  };

  "optional calc"_test = [] {
    calc a = [] { return std::optional<int>{42}; };
    static_assert(std::same_as<decltype(auto{a()}), std::optional<int>>);
    expect(a() == 42);
  };

  "unique_ptr atom"_test = [] {
    atom<std::unique_ptr<int>> a = nullptr;
    atom<std::unique_ptr<int>> b = std::make_unique<int>(42);
    atom c = std::unique_ptr<int>{};
    atom d = std::make_unique<int>(42);

    static_assert(std::same_as<decltype(a()), const std::unique_ptr<int> &>);
    static_assert(std::same_as<decltype(b()), const std::unique_ptr<int> &>);
    static_assert(std::same_as<decltype(c()), const std::unique_ptr<int> &>);
    static_assert(std::same_as<decltype(d()), const std::unique_ptr<int> &>);

    expect(a() == nullptr);
    expect(*b() == 42);
    expect(c() == nullptr);
    expect(*d() == 42);
  };

  "unique_ptr calc"_test = [] {
    calc a = [] { return std::make_unique<int>(42); };
    static_assert(std::same_as<decltype(a()), const std::unique_ptr<int> &>);
    expect(*a() == 42);
  };

  "function atom"_test = [] {
    auto l = [] { return 0; };
    atom<decltype(l)> a = {};
    atom<decltype(l)> b = decltype(l){l};
    atom<decltype(l)> c = l;
    atom d = decltype(l){};
    atom e = decltype(l){l};
    atom f = l;

    static_assert(std::same_as<decltype(auto{a()}), decltype(l)>);
    static_assert(std::same_as<decltype(auto{b()}), decltype(l)>);
    static_assert(std::same_as<decltype(auto{c()}), decltype(l)>);
    static_assert(std::same_as<decltype(auto{d()}), decltype(l)>);
    static_assert(std::same_as<decltype(auto{e()}), decltype(l)>);
    static_assert(std::same_as<decltype(auto{f()}), decltype(l)>);

    expect(a() == decltype(l){});
    expect(b() == decltype(l){l});
    expect(c() == l);
    expect(d() == decltype(l){});
    expect(e() == decltype(l){l});
    expect(f() == l);
  };

  "function calc"_test = [] {
    auto l = [] { return 42; };
    calc a = [=] { return l; };
    static_assert(std::same_as<decltype(auto{a()}), decltype(l)>);
    expect(a()() == 42);
  };
};
