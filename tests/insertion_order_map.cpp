#include "common.h"

suite<"insertion_order_map"> _ = [] {
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
};
