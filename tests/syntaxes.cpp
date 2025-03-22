#include "common.h"

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
