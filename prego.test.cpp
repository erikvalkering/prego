#include "prego.h"

#include <string>

using namespace std::string_literals;

using prego::atom;
using prego::autorun;
using prego::calc;
using prego::global_scope_manager;
using prego::log;
using prego::scope_manager_t;

auto join(auto... args) { return (""s + ... + args); }

auto all_tests_passed = true;
#define assert_eq(x, y, ...)                                                             \
    {                                                                                    \
        auto $$ = x;                                                                     \
        if ($$ != y) {                                                                   \
            std::cout << "ASSERTION FAILED(" << __FUNCTION__ << "@" << __LINE__ << ")\n" \
                      << std::boolalpha << "\t" << #x << " == " << y << "\n"             \
                      << "\t" << $$ << " != " << y << "\n"                               \
                      << "\t" << join(__VA_ARGS__) << "\n";                              \
            all_tests_passed = false;                                                    \
        }                                                                                \
    }

auto sandbox() {
    auto first_name = atom{ "Anita"s };
    auto last_name = atom{ "Laera"s };
    auto nick_name = atom{ ""s };

    auto full_name = calc{ [=](auto get) {
        std::cout << "calc full_name\n";
        if (get(nick_name) != "")
            return get(nick_name);
        else
            return get(first_name) + " " + get(last_name);
    } };

    auto display_full = atom{ true };
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

auto test_atom() {
    auto a = atom{ 42 };

    assert_eq(a.get(), 42, "state should be accessible directly");

    a.set(1729);
    assert_eq(a.get(), 1729, "mutations should be allowed and observable");
}

auto test_calc() {
    auto a = atom{ 42 };

    auto b = calc{ [=](auto get) { return 2 * get(a); } };

    assert_eq(b.get(), 2 * 42, "computed state should be accessible directly");

    a.set(1729);
    assert_eq(b.get(), 2 * 1729, "mutations should be allowed and observable through computed state");
}

auto test_lazy_observing() {
    auto a = atom{ 42 };

    auto x = false;
    auto b = calc{ [=, &x](auto get) {
        x = true;
        get(a);
        return 0;
    } };

    assert_eq(x, false, "computed state should not compute when not requested");
    b.get();
    assert_eq(x, true, "computed state should compute the first time when requested");

    x = false;
    b.get();
    assert_eq(x, false, "computed state should not recompute when dependencies have not changed");

    x = false;
    a.set(1729);
    assert_eq(x, false, "computed state should not be reactive if not reactively observed");
    b.get();
    assert_eq(x, true, "computed state should recompute when dependencies have changed");
}

auto test_dynamic_reactions() {
    auto first_name = atom{ "John"s };
    auto last_name = atom{ "Doe"s };
    auto nick_name = atom{ ""s };

    auto x = false;
    auto full_name = calc{ [=, &x](auto get) {
        x = true;
        auto value = get(first_name) + " " + get(last_name);
        log(1, "full_name = ", value);
        return value;
    } };

    auto y = false;
    auto display_name = calc{ [=, &y](auto get) {
        y = true;
        if (get(nick_name) != "") {
            auto value = get(nick_name);
            log(1, "display_name = ", value);
            return value;
        } else {
            auto value = get(full_name);
            log(1, "display_name = ", value);
            return value;
        }
    } };

    auto z = false;
    auto enabled = atom{ true };
    autorun([=, &z](auto get) {
        z = true;
        if (get(enabled)) {
            auto value = get(display_name);
            log(1, "autorun = ", value);
        } else
            log(1, "autorun = disabled");
    });

    assert_eq(x, true, "full_name should be computed");
    assert_eq(y, true, "display_name should be computed");
    assert_eq(z, true, "autorun should execute immediately");

    x = false;
    y = false;
    z = false;

    first_name.set("Jane");
    assert_eq(x, true, "full_name should react to change of first_name");
    assert_eq(y, true, "display_name should react to change of full_name");
    assert_eq(z, true, "autorun should react to change of display_name");

    x = false;
    y = false;
    z = false;

    nick_name.set("Jane Doe");
    assert_eq(x, false, "full_name should not react to change of nick_name");
    assert_eq(y, true, "display_name should react to change of nick_name");
    assert_eq(z, false, "autorun should not react because display_name did not change");

    x = false;
    y = false;
    z = false;

    first_name.set("John");
    assert_eq(x, false, "full_name should not react to change of first_name, because full_name is not being observed");
    assert_eq(y, false, "display_name should not react, because it is not observing full_name");
    assert_eq(z, false, "autorun should not react, because nothing changed");

    x = false;
    y = false;
    z = false;

    enabled.set(false);
    assert_eq(x, false, "full_name should not react to change of enabled");
    assert_eq(y, false, "display_name should not react, because nick_name did not change");
    assert_eq(z, true, "autorun should react to change of enabled");

    x = false;
    y = false;
    z = false;

    nick_name.set("John Doe");
    assert_eq(x, false, "full_name should not react to change of nick_name, because it does not observe it");
    assert_eq(y, false, "display_name should not react to change of nick_name, because display_name is not being observed right now");
    assert_eq(z, false, "autorun should not react to change of nick_name, because it is not being observed right now (through display_name)");

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
    assert_eq(x, false, "full_name should not recompute because it is still not being observed");
    assert_eq(y, true, "display_name should recompute, because it is being observed again");
    assert_eq(z, true, "autorun should react to change of enabled, as well as display_name that was changed");

    x = false;
    y = false;
    z = false;

    nick_name.set("");
    assert_eq(x, true, "full_name should be queried through display_name and recompute because it was previously changed");
    assert_eq(y, true, "display_name should recompute, because nick_name changed, as well as full_name");
    assert_eq(z, false, "autorun should not react, because display_name did not change");
    // TODO: rename computed to derived?
    // TODO: rename autorun to observe or observer or reaction or effect? or reactive?

    enabled.set(false);
    x = false;
    y = false;
    z = false;

    enabled.set(true);
    assert_eq(x, false, "full_name should not recompute because nothing changed");
    assert_eq(y, false, "display_name should recompute, because nothing changed");
    assert_eq(z, true, "autorun should react to change of enabled");
}

auto test_autorun() {
    {
        auto a = atom{ 42 };
        auto x = false;
        autorun([&x](auto get) { x = true; });
        assert_eq(x, true, "should execute immediately");

        x = false;
        a.set(42);
        assert_eq(x, false, "should not react on mutations of a");

        a.set(1729);
        assert_eq(x, false, "should not react on mutations of a");
    }

    {
        auto a = atom{ 42 };
        auto x = false;
        autorun([=, &x](auto get) { get(a); x = true; });
        assert_eq(x, true, "should execute immediately");

        x = false;
        a.set(42);
        assert_eq(x, false, "should not react if a did not change");

        a.set(1729);
        assert_eq(x, true, "should react on mutations of a");
    }

    {
        // tests capture by-value (shared ownership of atom)
        auto a = atom{ 42 };
        auto x = false;
        autorun([=, &x](auto get) { get(a); x = true; });
        assert_eq(x, true, "should execute immediately");

        x = false;
        a.set(42);
        assert_eq(x, false, "should not react if a did not change");
        a.set(1729);
        assert_eq(x, true, "should react on mutations of a");
    }
}

auto test_scope_manager() {
    auto a = atom{ 42 };

    auto x = false;
    auto y = false;
    auto z = false;
    auto w = false;

    {
        {
            auto scope_manager = scope_manager_t{};

            {
                autorun([=, &x](auto get) { get(a); x = true; });
                autorun([=, &y](auto get) { get(a); y = true; }, &scope_manager);
                auto _ = autorun([=, &z](auto get) { get(a); z = true; }, nullptr);
                std::ignore = autorun([=, &w](auto get) { get(a); w = true; }, nullptr);

                assert_eq(x, true, "sanity check");
                assert_eq(y, true, "sanity check");
                assert_eq(z, true, "sanity check");
                assert_eq(w, true, "sanity check");

                x = false;
                y = false;
                z = false;
                w = false;
                a.set(1729);
                assert_eq(x, true, "autorun should be kept alive by global_scope_manager");
                assert_eq(y, true, "autorun should be kept alive by local scope_manager");
                assert_eq(z, true, "autorun should be kept alive by local variable");
                assert_eq(w, false, "autorun should be destroyed immediately");
            }

            x = false;
            y = false;
            z = false;
            w = false;
            a.set(42);
            assert_eq(x, true, "autorun should be kept alive by global_scope_manager");
            assert_eq(y, true, "autorun should be kept alive by local scope_manager");
            assert_eq(z, false, "autorun should be destroyed by local variable");
            assert_eq(w, false, "autorun should be destroyed immediately");
        }

        x = false;
        y = false;
        z = false;
        w = false;
        a.set(1729);
        assert_eq(x, true, "autorun should be kept alive by global_scope_manager");
        assert_eq(y, false, "autorun should be destroyed by local scope_manager");
        assert_eq(z, false, "autorun should be destroyed by local variable");
        assert_eq(w, false, "autorun should be destroyed immediately");
    }

    global_scope_manager.clear();
    x = false;
    y = false;
    z = false;
    w = false;
    a.set(42);
    assert_eq(x, false, "autorun should be destroyed by global_scope_manager");
    assert_eq(y, false, "autorun should be destroyed by local scope_manager");
    assert_eq(z, false, "autorun should be destroyed by local variable");
    assert_eq(w, false, "autorun should be destroyed immediately");
}

auto test_auto_unobserve() {
    auto a = atom{ 42 };
    auto x = false;
    {
        // TODO: autorun's lifetime should be managed by atom a
        // TODO: a's lifetime should also be managed by autorun
        // TODO: this way, we don't need the global scope_manager
        // TODO: how to resolve this cyclic ownership dependency?
        auto _ = autorun([=, &x](auto get) { get(a); x = true; }, nullptr);
    }

    x = false;
    a.set(1729);
    assert_eq(x, false, "autorun should definitely not be invoked");
    assert_eq(std::size(a.observers()), 0, "autorun should not be referenced anymore");
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

auto test_lifetimes() {
// TODO: full gc-like lifetime management without need for global scope manager
#if 0
    auto a_lt = lifetime_tracker{};
    auto autorun_lt = lifetime_tracker{};
    {
        auto a = atom{a_lt.track()};
        {
            autorun([=, x = autorun_lt.track()](auto get) { get(a); });
        }
        assert_eq(autorun_lt.alive(), true, "autorun should be kept alive by a");
    }
    assert_eq(a_lt.alive(), false, "a should be destroyed");
    assert_eq(autorun_lt.alive(), false, "autorun should be destroyed");
#endif

    auto b_lt = lifetime_tracker{};
    auto c_lt = lifetime_tracker{};
    {
        auto c = [&] {
            assert_eq(b_lt.alive(), false, "b should not be alive");
            auto b = atom{ b_lt.track() };
            assert_eq(b_lt.alive(), true, "b should be alive");

            return calc{ [=, x = c_lt.track()](auto get) { return get(b); } };
        }();

        assert_eq(b_lt.alive(), true, "b should be kept alive by c");
    }
    assert_eq(b_lt.alive(), false, "b should be destroyed");
    assert_eq(c_lt.alive(), false, "c should be destroyed");
}

auto test_noncopyable_types() { assert_eq(true, false, "not implemented yet"); }
auto test_immovable_types() { assert_eq(true, false, "not implemented yet"); }

auto test_atom_syntaxes() {
    auto a = atom{ 42 };
    atom b = 42;
    // alternative syntax atom: function instead of class
    //   which might be more flexible in the design space of the
    //   implementation. Same for calc.
    auto c = atom(42);

    // mutation syntax 1: set
    a.set(1729);

    // mutation syntax 2: direct assignment
    a = 1729;
}

auto test_calc_syntaxes() {
    auto a = atom{ 42 };
    auto b = atom{ 42 };

    // calc syntax 1: get as parameter
    //   - thread-safe
    //   - may allow for optimizations
    auto c = calc{ [=](auto get) { return get(a) + get(b); } };
    calc d = [=](auto get) { return get(a) + get(b); };

    // calc syntax 2: parameterless function
    //   - not thread-safe
    //   - maybe more difficult to optimize
    //   - less boilerplate
    auto e = calc{ [=] { return a() + b(); } };

    // calc syntax 3: parameterless function + smartref
    //   - not thread-safe
    //   - maybe more difficult to optimize
    //   - minimal boilerplate (except for user defined types)
    //   - maybe not worth the effort, considering only 2 chars per observable
    // auto f = calc{[=] { return a + b; }};

    // calc syntax 4: static get
    //   - thread-safe
    //   - may allow for more optimization
    //   - every atom/calc should be declared globally and becomes stateless
    //   - does every observable have to be unique?
    // auto g = calc{[](auto get) { return get(a) + get(b); }};
}

struct Person_ {
    atom<std::string> first_name;
    atom<std::string> last_name;

    auto full_name(auto get) const { return get(first_name) + " " + get(last_name); }
};

struct Person : prego::Computable<Person_> {
    /*    using full_name_thunk = Thunk<Class, [](auto &self, auto get) {
        return self.full_name(get);
        }>;

        calc<full_name_thunk> full_name{this};*/
    // PREGO_COMPUTED(full_name);
};

auto test_oo() {
    {
        auto john = Person{ "John"s, "Doe"s };
        auto jane = Person{ "Jane", "Doe" };

        // assert_eq(john.full_name.get(), "John Doe", "John and Jane should not share state");
        // assert_eq(jane.full_name.get(), "Jane Doe", "John and Jane should not share state");
    }

    {
        using Person = decltype([] {
            atom first_name{ "John"s };
            atom last_name{ "Doe"s };

            calc full_name = [=](auto get) { return get(first_name) + " " + get(last_name); };

            struct Person {
                decltype(first_name) first_name{ first_name };
                decltype(last_name) last_name{ last_name };
                // decltype(full_name) full_name{full_name};
            };

            return Person{};
        }());

        auto john = Person{ "John"s, "Doe"s };
        auto jane = Person{ "Jane", "Doe" };
    }

    class atom_vector {
        std::vector<int> v;

    public:
        void push_back(int x) {}
    };
}

auto test_simple_syntax() {
    atom x = 42;
    calc y = [=] { return x(); };
    autorun([=] { y(); });

    x = 1729;
}

template<typename T>
struct atom_state_mock : prego::observable_state_t {
    T value;
    mutable int is_up_to_date_counter = 0;

    atom_state_mock(auto &&value) : value{ FWD(value) } {}

    virtual bool is_up_to_date() const final {
	++is_up_to_date_counter;
        return true;
    }
};

auto test_graph_traversal_efficiency() {
    atom<int, atom_state_mock> a{42};
    a();
    assert_eq(a.state->is_up_to_date_counter, 0, "accessing a should directly return the value, without checking if it's up to date (because it always is)");

    calc b = [=] { return a(); };
    b();
    assert_eq(a.state->is_up_to_date_counter, 0, "b is initially not calculated yet, so we know it is not up to date and don't need to check a");

    calc c = [=] { return b(); };
    calc d = [=] { return b(); };
    calc e = [=] { return c() + d(); };

    a.state->is_up_to_date_counter = 0;
    e();
    assert_eq(a.state->is_up_to_date_counter, 1, "b should only check a once for determining whether it is up to date");
}

auto test() {
    /*test_atom();
    test_autorun();
    test_scope_manager();
    test_calc();
    test_dynamic_reactions();
    test_lazy_observing();
    test_auto_unobserve();
    test_lifetimes();
    test_oo();
    // test_noncopyable_types();
    // test_immovable_types();
    test_atom_syntaxes();
    test_calc_syntaxes();
    test_simple_syntax();*/
    test_graph_traversal_efficiency();

    if (all_tests_passed)
        std::cout << "all tests passed\n";
    else
        std::cout << "some tests failed\n";
}

int main() {
    test();
}
