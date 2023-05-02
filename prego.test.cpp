#include "prego.h"

#include <string>

using namespace std::string_literals;

using prego::observable;
using prego::computed;
using prego::autorun;
using prego::scope_manager_t;
using prego::global_scope_manager;
using prego::log;

auto join(auto ...args) {
    return (""s + ... + args);
}

auto all_tests_passed = true;
#define assert_eq(x, y, ...) { \
    auto $$ = x; \
    if ($$ != y) { \
        std::cout << "ASSERTION FAILED(" << __FUNCTION__ << "@" << __LINE__ << ")\n" \
	     << std::boolalpha \
             << "\t" << #x << " == " << y << "\n" \
             << "\t" << $$ << " != " << y << "\n" \
	     << "\t" << join(__VA_ARGS__ ) << "\n"; \
	all_tests_passed = false; \
    } \
}

auto sandbox() {
    auto first_name = observable{"Anita"s};
    auto last_name = observable{"Laera"s}; 
    auto nick_name = observable{""s};

    auto full_name = computed{[=](auto get) {
	std::cout << "calc full_name\n";
        if (get(nick_name) != "")
    	    return get(nick_name);
        else
	    return get(first_name) + " " + get(last_name);
    }};

    auto display_full = observable{true};
    autorun([=](auto get) {
	std::cout << "calc autorun\n";
	if (get(display_full)) {
	    const auto n = get(full_name);
            std::cout << ">> " << n << std::endl;
	}
	else
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

auto test_observable() {
    auto a = observable{42};

    assert_eq(a.get(), 42, "state should be accessible directly");

    a.set(1729);
    assert_eq(a.get(), 1729, "mutations should be allowed and observable");
}

auto test_computed() {
    auto a = observable{42};

    auto b = computed{[=](auto get) {
        return 2 * get(a);
    }};

    assert_eq(b.get(), 2 * 42, "computed state should be accessible directly");

    a.set(1729);
    assert_eq(b.get(), 2 * 1729, "mutations should be allowed and observable through computed state");
}

auto test_lazy_observing() {
    auto a = observable{42};

    auto x = false;
    auto b = computed{[=, &x](auto get) {
	x = true;
        get(a);
	return 0;
    }};

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
    auto first_name = observable{"John"s};
    auto last_name = observable{"Doe"s};
    auto nick_name = observable{""s};

    auto x = false;
    auto full_name = computed{[=, &x](auto get) {
	x = true;
	auto value = get(first_name) + " " + get(last_name);
	log(1, "full_name = ", value);
	return value;
    }};

    auto y = false;
    auto display_name = computed{[=, &y](auto get) {
	y = true;
        if (get(nick_name) != "") {
    	    auto value = get(nick_name);
	    log(1, "display_name = ", value);
	    return value;
	}
        else {
	    auto value = get(full_name);
	    log(1, "display_name = ", value);
	    return value;
	}
    }};

    auto z = false;
    auto enabled = observable{true};
    autorun([=, &z](auto get) {
	z = true;
	if (get(enabled)) {
	    auto value = get(display_name);
	    log(1, "autorun = ", value);
	}
	else
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
        auto a = observable{42};
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
        auto a = observable{42};
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
        // tests capture by-value (shared ownership of observable)
        auto a = observable{42};
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
    auto a = observable{42};

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
    auto a = observable{42};
    auto x = false;
    {
	// TODO: autorun's lifetime should be managed by observable a
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
    auto alive() const {
	return !p.expired();
    }

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
        auto a = observable{a_lt.track()};
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
	    auto b = observable{b_lt.track()};
            assert_eq(b_lt.alive(), true, "b should be alive");

	    return computed{[=, x = c_lt.track()](auto get) { return get(b); }};
	}();

        assert_eq(b_lt.alive(), true, "b should be kept alive by c");
    }
    assert_eq(b_lt.alive(), false, "b should be destroyed");
    assert_eq(c_lt.alive(), false, "c should be destroyed");
}

auto test_noncopyable_types() {
    assert_eq(true, false, "not implemented yet");
}

auto test_immovable_types() {
    assert_eq(true, false, "not implemented yet");
}

auto test_observable_syntaxes() {
    auto a = observable{42};
    observable b = 42;
    // alternative syntax observable: function instead of class
    //   which might be more flexible in the design space of the
    //   implementation. Same for computed.
    auto c = observable(42);

    // mutation syntax 1: set
    a.set(1729);

    // mutation syntax 2: direct assignment
    // a = 1729;
}

auto test_computed_syntaxes() {
    auto a = observable{42};
    auto b = observable{42};

    // computed syntax 1: get as parameter
    //   - thread-safe
    //   - may allow for optimizations
    auto c = computed{[=](auto get) { return get(a) + get(b); }};
    computed d = [=](auto get) { return get(a) + get(b); };
    computed<int> e = [=](auto get) { return get(a) + get(b); }

    // computed syntax 2: parameterless function
    //   - not thread-safe
    //   - maybe more difficult to optimize
    //   - less boilerplate
    // auto f = computed{[=] { return a() + b(); }};

    // computed syntax 3: parameterless function + smartref
    //   - not thread-safe
    //   - maybe more difficult to optimize
    //   - minimal boilerplate (except for user defined types)
    //   - maybe not worth the effort, considering only 2 chars per observable
    // auto g = computed{[=] { return a + b; }};

    // computed syntax 4: static get
    //   - thread-safe
    //   - may allow for more optimization
    //   - every observable/computed should be declared globally and becomes stateless
    //   - does every observable have to be unique?
    // auto h = computed{[](auto get) { return get(a) + get(b); }};
}

auto test_oo() {
    struct Person {
	observable<std::string> first_name;
	observable<std::string> last_name;

	//computed<decltype(&full_name_)> full_name{&full_name_};
	/*auto full_name_(auto get) const { 
	    return get(first_name) + " " + get(last_name);
	}*/
    };

    auto john = Person{"John"s, "Doe"s};
    auto jane = Person{"Jane", "Doe"};
/*
    [] {
	observable first_name{"John"s};
	observable last_name{"Doe"s};

	computed full_name = [=](auto get) {
	    return get(first_name) + " " + get(last_name);
	};
    };
*/
    class observable_vector {
        std::vector<int> v;

    public:
	void push_back(int x) {
	}
    };
}

auto test() {
    test_observable();
    test_autorun();
    test_scope_manager();
    test_computed();
    test_dynamic_reactions();
    test_lazy_observing();
    test_auto_unobserve();
    test_lifetimes();
    test_oo();
    //test_noncopyable_types();
    //test_immovable_types();
    test_observable_syntaxes();
    test_computed_syntaxes();

    if (all_tests_passed)
        std::cout << "all tests passed\n";
    else
	std::cout << "some tests failed\n";
}

int main() {
    test();
}
