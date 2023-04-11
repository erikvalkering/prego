//TODO: are all the todos tested for breaking the app
//TODO: split calc() into a void func and was_changed() func
#include <algorithm>
#include <cassert>
#include <compare>
#include <functional>
#include <iostream>
#include <memory>
#include <numeric>
#include <optional>
#include <set>
#include <utility>
#include <variant>
#include <vector>

using namespace std;

#define RNG(x) std::begin(x), std::end(x)
#define FWD(x) std::forward<decltype(x)>(x)

struct observable;
struct computed;
struct reaction;

using dependent_t = std::variant<computed, reaction>;
using dependency_t = std::variant<observable, computed>;

// TODO: idea - store counter in each atom and store a map in each node: atom -> copy of last seen counter value. If the values don't match ==> node is dirty. Drawback is that it might theoretically overflow such that it compares two identical values, while they are actually different.
// TODO: idea2 - instead of counter, store shared_ptr with counter in atom and weak_ptr in nodes. If weak_count == 0, nothing needs to be done (no one needs to be notified). If increasing counter doesn't overflow, just increase. Otherwise, otherwise create new shared_ptr holding 0. Might not be efficient, but doesn't overflow.

enum class dirty_state {
    no,
    maybe,
    yes,
    updated,
};

auto to_string(auto x) { return x; }

auto to_string(dirty_state is_dirty) {
    switch (is_dirty) {
        default: return "unknown";
        case dirty_state::no: return "no";
        case dirty_state::maybe: return "maybe";
        case dirty_state::yes: return "yes";
        case dirty_state::updated: return "updated";
    }
}

auto log_(int id, auto ...args) {
    switch (id) {
    default: break;
    case 0:
    case 1:
    case 2:
    case 3:
    case 4:
    case 5:
    case 6:
    break;
    }

    ((std::cout << id << ": " << to_string(args)), ...);
}

auto log(auto ...args) {
    log_(args..., '\n');
}

// Conditionally updates dirty_state:
//   yes->maybe is ignored
// If value changed returns true, otherwise false
auto set_dirty_state(auto &state, auto is_dirty) {
    if (state->is_dirty == dirty_state::yes
            && is_dirty == dirty_state::maybe) {
        log(0,
            state->name,
            ":dirty_state ",
             state->is_dirty,
             " => ",
             is_dirty,
             " => ",
             state->is_dirty
        );
        
        return false;
    }
    
    if (state->is_dirty == is_dirty) {
        log(0,
            state->name,
            ":dirty_state unchanged"
        );
        
        return false;
    }
    
    log(0,
        state->name,
        ":dirty_state ",
         state->is_dirty,
         " => ",
         is_dirty
    );

    state->is_dirty = is_dirty;

    return true;
}

auto calc(const dependency_t &dep) -> bool;
auto invalidate(
    const dependent_t &dep,
    dirty_state is_dirty
) -> void;

// calculates all dependencies
// returns true if at least one of them returns true
//   true indicates that dependents maybe affected
//   and need to be recalculated
auto calc_all(const auto &deps) -> bool {
    return std::accumulate(
        RNG(deps),
        false,
        [](auto acc, auto &dep) {
            return calc(dep) or acc;
        }
    );
}

auto dependents_names(const dependency_t &dep) -> string;

auto name(const dependency_t &) -> string;
auto name(const dependent_t &) -> string;

auto names(const auto &deps, auto f) -> string {
    return std::accumulate(
        RNG(deps),
        ""s,
        [=](auto acc, auto &dep) {
            if (acc != "") acc += ",";
            return acc + f(dep);
        }
    );
}

auto print_dependencies(auto dep) {
    log(7, "dependencies for ",
        dep.state->name, ": ", names(
            dep.state->dependencies,
            [](auto &dep) {
                return name(dep)
                     + "->{" + dependents_names(dep) + "}";
            })
        );
}

auto invalidate_all(
    auto n, auto v,
    const std::vector<dependent_t> &deps,
    auto is_dirty)
-> void;

auto remove_dependencies(auto &dependent) -> void;

// Regular void:
//   T -> T
//   void -> monostate
auto wrap(auto f, auto ...args) {
    if constexpr (is_void_v<decltype(f(args...))>) {
        f(args...);
        return monostate{};
    }
    else {
        return f(args...);
    }
}

auto unwrap(monostate) {}
auto unwrap(auto value) { return value; }

// clears dependencies and unconditionally runs function
auto run(const auto &dependent) {
    log(7, "before removal (", dependent.state->name, ")");
    print_dependencies(dependent);
    remove_dependencies(dependent);
    log(7, "after removal (", dependent.state->name, ")");
    print_dependencies(dependent);
    auto result = wrap(dependent.state->f, dependent);
    log(7, "after calc (", dependent.state->name, ")");
    print_dependencies(dependent);
    return unwrap(result);
}

auto depends(auto &dependent, auto &dependency) -> void;

struct observable {
    struct state_t {
        std::string name;
        int value;
        static constexpr auto is_dirty = dirty_state::no;
        std::vector<dependent_t> dependents;
    };

    std::shared_ptr<state_t> state = std::make_shared<state_t>();

public:
    explicit observable(std::string name, int value) {
        state->name = name;
        state->value = value;
    }

    auto operator==(const observable &rhs) const {
        return state == rhs.state;
    }

    auto calc() const { return false; }
    auto operator()(auto dependent) const {
        depends(dependent, *this);
        log(3, state->name, "() == ", state->value);
        return state->value;
    }

    auto operator()() const {
        log(3, state->name, "() == ", state->value);
        return state->value;
    }

    auto set(int value) -> void;
};

struct computed {
    struct state_t {
        std::string name;
        std::function<int (const computed &)> f;
        mutable std::optional<int> cache;
        mutable dirty_state is_dirty = dirty_state::yes;
        std::vector<dependency_t> dependencies;
        std::vector<dependent_t> dependents;
    };

    std::shared_ptr<state_t> state = std::make_shared<state_t>();

public:
    auto operator==(const computed &rhs) const {
        return state == rhs.state;
    }

    explicit computed(
        std::string name,
        std::function<int (const computed &)> f
    ) {
        state->name = name;
        state->f = f;
    }

    auto invalidate(dirty_state is_dirty) const -> void;

    auto calc() const {
        if (state->is_dirty == dirty_state::no) {
            log(1,
                state->name,
                " is up to date, skip calc..."
            );
            return false;
        }

        if (state->is_dirty == dirty_state::updated) {
            log(1,
                state->name,
                " was previously updated, skip calc..."
            );
            return true;
        }

        if (state->is_dirty == dirty_state::yes) {
            log(1,
                state->name,
                " must be calculated, checking deps..."
            );
        }

        if (state->is_dirty == dirty_state::maybe) {
            log(1,
                state->name,
                " may have changed, checking deps..."
            );
        }
       
	// Make sure dependencies are calculated first.
        const auto did_any_dep_change = calc_all(state->dependencies);

	// Recalculate if any dependency changed
	// or if we were forced to recalculate.
	auto should_recalculate = did_any_dep_change
		               or state->is_dirty == dirty_state::yes;

        if (!should_recalculate) {
            log(1,
                state->name,
                " deps unchanged, skip calc..."
            );
            set_dirty_state(this->state, dirty_state::no);
            return false;
        }

        log(2, "calculating ", state->name, "...");

        const auto old = std::exchange(
            state->cache, run(*this)
        );

        log(2, state->name
                 , ":calculated "
                 , (old ? to_string(*old) : "null")
                 , " => "
                 , *state->cache
        );

        should_recalculate = state->cache != old;
            
        set_dirty_state(
            this->state,
            should_recalculate ? dirty_state::updated
                               : dirty_state::no
        );
        
        return should_recalculate;
    }

    auto operator()(auto dependent) const {
	// register dependent => dependency
        depends(dependent, *this);
        calc();
        log(3, state->name, "() == ", *state->cache);
        return *state->cache;
    }

    auto operator()() const {
        calc();
        log(3, state->name, "() == ", *state->cache);
        return *state->cache;
    }
};

struct reaction {
    struct state_t {
        std::string name;
        std::function<void (const reaction &)> f;
        mutable dirty_state is_dirty = dirty_state::yes;
        std::vector<dependency_t> dependencies;
    };
    
    auto operator==(const reaction &rhs) const {
        return state == rhs.state;
    }

    explicit reaction(
        std::string name, 
        std::function<void (const reaction &)> f
    ) {
        state->name = name;
        state->f = f;
        run(*this);
    }

    std::shared_ptr<state_t> state = std::make_shared<state_t>();

    auto invalidate(dirty_state is_dirty) const {
        set_dirty_state(this->state, is_dirty);
    }
};

auto calc(const dependency_t &dep) -> bool {
    return std::visit(
        [](auto &dep) {
            return dep.calc();
        }, dep);
}

auto name(const dependency_t &dep) -> string {
    return std::visit(
        [](auto &dep) {
            return dep.state->name;
        }, dep);
}
auto name(const dependent_t &dep) -> string {
    return std::visit(
        [](auto &dep) {
            return dep.state->name;
        }, dep);
}

auto dependents_names(const computed &dep) -> string {
    return names(dep.state->dependents, [](auto &dep) {
        return name(dep);
    });
}

auto dependents_names(const reaction &dep) -> string {
    return "";
}

auto dependents_names(const dependency_t &dep) -> string {
    return std::visit(
        [](auto &dep) {
            return dependents_names(dep);
        }, dep);
}

auto computed::invalidate(dirty_state is_dirty) const -> void {
    if (set_dirty_state(this->state, is_dirty))
        invalidate_all(
            state->name,
            0,
            state->dependents,
            dirty_state::maybe
        );
}
    
auto invalidate(
    const dependent_t &dep,
    dirty_state is_dirty
) -> void {
    return std::visit(
        [=](auto &dep) {
            dep.invalidate(is_dirty);
        },
        dep
    );
}

auto invalidate_all(
    auto n, auto v,
    const std::vector<dependent_t> &deps,
    auto is_dirty
) -> void {
//    if (n == "a" and v == 23) return;
   // log(8, "foobar");return ;
    std::for_each(
        RNG(deps),
        [=](const auto &dep) { invalidate(dep, is_dirty); }
    );
}

auto set_add(auto &container, const auto &e) {
    const auto it = std::find(RNG(container), e);
    if (it != std::end(container)) {
        return false;
    }
   
    container.push_back(e);
    return true;
}

auto depends(auto &dependent, auto &dependency) -> void {
    // dependent -> dependency
    if (!set_add(
        dependent.state->dependencies, 
        dependency_t{dependency}
    )) {
        log(4,
            dependent.state->name,
            " -> ",
            dependency.state->name,
            " not added"
        );
        return;
    }

    // dependency -> dependent
    set_add(
        dependency.state->dependents, 
        dependent_t{dependent}
    );
    
    log(4,
        dependent.state->name,
        " -> ",
        dependency.state->name,
        " added"
    );
}

auto set_remove(
    auto &container,
    const dependent_t &dependent
) {
   // const auto it = std::find(RNG(container), dependent);
  //  log(7, "erik")
    container.erase(
       std::remove(RNG(container), dependent),     
       container.end()
    );
}

auto set_remove(dependency_t &dependency, const auto &dependent) {
    std::visit(
        [=](auto &dependency) {
            set_remove(
                dependency.state->dependents,
                dependent_t{dependent}
            );
        },
        dependency
    );
}

auto remove_dependencies(auto &dependent) -> void {
    for (auto &dep : dependent.state->dependencies)
        set_remove(dep, dependent);
    dependent.state->dependencies.clear();
}

auto observable::set(int value) -> void {
    log(5, state->name, ":set ",
        state->value,
        " => ",
        value
    );

    state->value = value;
    //const auto reactions = 
    
    invalidate_all(state->name, value, state->dependents, dirty_state::yes);
    //for_each(RNG(reactions), reaction::run);
}

namespace v2 {

enum class notification_t {
    stale,
    changed,
    unchanged,
};

struct observer_t {
    virtual void notify(const notification_t) = 0;
};

struct observable_state_t {
    std::set<std::weak_ptr<observer_t>, std::owner_less<>> observers = {};

    void observe(const std::weak_ptr<observer_t> &observer) { 
	observers.insert(observer);
    }

    void unobserve(const std::weak_ptr<observer_t> &observer) {
	assert(observers.contains(observer));
	observers.erase(observer);
    }
};

using scope_manager_t = std::vector<std::shared_ptr<observable_state_t>>;
auto global_scope_manager = scope_manager_t{};

auto notify(auto &observers, const notification_t notification) {
    //std::cout << "notify " << observers.size() << " observers\n";
    for (auto &observer : observers) {
	if (auto p = observer.lock()) {
	    p->notify(notification);
	}
	else std::cout << "err\n";
    }
}

char id_counter = 'a';

template<typename T>
struct observable {
    struct state_t : observable_state_t {
	T value;
	char id = id_counter++;

	explicit state_t(const T &value) : value{value} {}
    };

    std::shared_ptr<state_t> state;

public:
    explicit observable(T value)
	: state{std::make_shared<state_t>(value)} {}

    auto set(auto &&value) {
	//std::cout << state->id << ".set(" << value << ") [" << state->value << "]\n";
	const auto old_value = std::exchange(state->value, value);
	if (state->value == old_value)
	   return;
	//std::cout << state->id << ": changed\n";

        // First sweep: mark all as stale
        notify(state->observers, notification_t::stale);

        // Second sweep: update observers
        notify(state->observers, notification_t::changed);
    }

    auto &get() const {
	return state->value;
    }

    auto observers() const {
	return state->observers;
    }
};

template<typename F>
struct computed {
    using T = decltype(std::declval<F>()([](auto o) { return o.get(); }));

    struct state_t : observable_state_t
	           , observer_t
		   , std::enable_shared_from_this<state_t> {
	char id = id_counter++;
	F f;
	std::optional<T> value; 
        std::set<std::weak_ptr<observable_state_t>, std::owner_less<>> observables;
	int stale_count = 0;
	bool maybe_changed = false;

	explicit state_t(const F &f) : f{f} {}
        ~state_t() {
	    const auto observer = this->weak_from_this();
	    for (auto &observable : observables)
		if (auto p = observable.lock())
		    p->unobserve(observer);
	}

	virtual void notify(const notification_t notification) final {
            //std::cout << id << ": notify: " << int(notification) << "\n";
	    using v2::notify;
	    switch (notification) {
		default: assert(false);

	        case notification_t::stale: {
		    // Mark as stale and propagate only if
		    // we were visited for the first time
		    if (stale_count++ == 0)
		    	notify(observers, notification);

		    break;
		}

		case notification_t::changed:
		case notification_t::unchanged:
	        {
		    // If an observable was changed,
		    // we need to recompute as well
		    maybe_changed |= notification == notification_t::changed;
	            //std::cout << id << ": maybe_changed: " << maybe_changed << "\n";

		    // Only continue when all observables have been updated
		    if (--stale_count != 0) break;

		    // Recompute (if necessary) and determine
		    // whether we actually changed
		    const auto changed = maybe_changed
			              && value != std::exchange(value, compute());
		    maybe_changed = false;
	            //std::cout << id << ": changed: " << changed << "\n";

		    // Finally notify all the observers that we are up to date
	            //std::cout << id << ": observers: " << observers.size() << "\n";
		    notify(observers, changed ? notification_t::changed
				              : notification_t::unchanged);
	            //std::cout << id << "---\n";
		    break;
		}
	    }
	}

	auto compute() {
	    const auto observer = this->shared_from_this();
	    for (auto &observable : observables)
		if (auto p = observable.lock())
		    p->unobserve(observer);
            observables.clear();

	    //std::cout << id << ": recompute\n";
	    return f([=, this](auto observable) {
		observables.insert(observable.state);
		observable.state->observe(observer);
		return observable.get();
	    });
	}
    };

    std::shared_ptr<state_t> state;
    friend auto autorun(auto f, scope_manager_t *);

public:
    explicit computed(F f)
	: state{std::make_shared<state_t>(f)} {}

    auto &get() const {
	if (!state->value)
	    state->value = state->compute();

	return *state->value;
    }

    auto observers() const {
	return state->observers;
    }
};

auto autorun(auto f, scope_manager_t *scope_manager = &global_scope_manager) {
    // TODO: optimize away the unnecessary int
    auto c = computed{[=](auto get) { f(get); return 0; }};
    c.get();

    if (scope_manager) scope_manager->push_back(c.state);

    return c;
}

[[nodiscard]] auto autorun(auto f, std::nullptr_t) {
    return autorun(f, static_cast<scope_manager_t *>(nullptr));
}

auto join(auto ...args) {
    return (""s + ... + args);
}

#define assert_eq(x, y, ...) { \
    auto $$ = x; \
    if ($$ != y) { \
        cout << "ASSERTION FAILED(" << __LINE__ << ")\n" \
	     << std::boolalpha \
             << "\t" << #x << " == " << y << "\n" \
             << "\t" << $$ << " != " << y << "\n" \
	     << "\t" << v2::join(__VA_ARGS__ ) << "\n"; \
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
    //
    // TODO: last set of nickname should not propagate to fullname, because fullname is not observed anymore
    // TODO: it should preserve the state though in fullname, such that when it gets observed again, it does not have to recompute
    // TODO: but in this case it should recompute because nickname changed
    // TODO: rename computed to derived?
    // TODO: rename autorun to observe or observer or reaction or effect? or reactive?
}

auto test_observable() {
    auto a = observable{42};

    assert_eq(a.get(), 42, "state should be accessible directly");

    a.set(1729);
    assert_eq(a.get(), 1729, "mutations should be allowed and observable");
}

auto test_autorun() {
    {
        auto a = observable{42};
        auto x = false;
        autorun([&](auto get) { x = true; });
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
        autorun([&](auto get) { get(a); x = true; });
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
	        autorun([&, a](auto get) { get(a); x = true; });
	        autorun([&, a](auto get) { get(a); y = true; }, &scope_manager);
	        auto _ = autorun([&, a](auto get) { get(a); z = true; }, nullptr);
	        std::ignore = autorun([&, a](auto get) { get(a); w = true; }, nullptr);

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

struct destructor {
    bool *p;
    ~destructor() { *p = true; }

    auto operator<=>(const destructor &) const = default;
};

auto test_gc_lifetimes() {
    auto a_dead = false;
    auto autorun_dead = false;
    {
        auto a = observable{destructor{&a_dead}};
        {
	    autorun([=, x = destructor{&autorun_dead}](auto get) { get(a); });
        }
        assert_eq(autorun_dead, false, "autorun should be kept alive by a");
    }
    assert_eq(a_dead, true, "a should be destroyed");
    assert_eq(autorun_dead, true, "autorun should be destroyed");

    auto b_dead = false;
    auto c_dead = false;
    {
	auto c = [&] {
	    auto b = observable{destructor{&b_dead}};

	    return computed{[=, x = destructor{&c_dead}](auto get) { return get(b); }};
	}();

        assert_eq(b_dead, false, "b should be kept alive by c");
    }
    assert_eq(b_dead, true, "b should be destroyed");
    assert_eq(c_dead, true, "c should be destroyed");
}

auto test() {
    test_observable();
    test_autorun();
    test_scope_manager();
    test_auto_unobserve();
    //test_gc_lifetimes();

    std::cout << "all tests passed\n";
}

} // namespace v2

/*
/////////////////////////////////
// HELL2
/////////////////////////////////
// a---c---reaction2
//  \ / \
//   b---d---reaction1
//  /   /
// e---f
*/

/*
What about a dependent that after a state change starts depending on part of the graph that already depends on the changed observable, but now can only be computed after that part of the graph has been fully computed? It might incorrectly conclude that the dependent and sub graph can be calculated concurrently.
*/

    /*
    // a(21)---c(a>21?22:b)---r2(=)
    //      \ /    \
    //       b(+)---d(+)---r1(=)
    //      /      /
    //    e(0)---f(=)
    */
// TODO: c depends on a and b (in that order), a sets c to dirty:yes, whereas b sets c to dirty:maybe (incorrect)
// TODO: c depends on b and a (in that order), b sets c to dirty:maybe, whereas a sets c to dirty:yes (correct)
// TODO: c depends on b, b depends on a; d depends on a and c; updating a should not trigger d before b and c

int main() {
    //v2::sandbox();
    v2::test();
}

int main2() {
    auto a = observable{"a", 21};
    /*
    // a(21)
    */

    auto e = observable{"e", 0};
    /*
    // a(21)
    //
    //    e(0)
    */

    auto b = computed{"b", [=](auto self) {
        return a(self) + e(self);
    }};
   
    /*
    // a(21)
    //      \
    //       b(?)
    //      /
    //    e(0)
    */

    auto c = computed{"c", [=](auto self) {
        return a(self) > 21 ? 22 : b(self);
    }};
    /*
    // a(21)---c(?)
    //      \ /
    //       b(?)
    //      /
    //    e(0)
    */

    auto f = computed{"f", [=](auto self) {
        return e(self);
    }};
    /*
    // a(21)---c(?)
    //      \ /
    //       b(?)
    //      /
    //    e(0)---f(?)
    */

    auto d = computed{"d", [=](auto self) {
        return c(self) + b(self) + f(self);
    }};
    /*
    // a(21)---c(?)
    //      \ /    \
    //       b(?)---d(?)
    //      /      /
    //    e(0)---f(?)
    */
    log(6, "eager 1...");
    assert_eq(d(), 42);
    /*
    // a(21)===c(21)
    //    \\  //   \\
    //     b(21)===d(42)
    //    //      //
    //  e(0)====f(0)
    */

    log(6);
    log(6, "eager 2...");
    assert_eq(d(), 42);
    /*
    // a(21)---c(21)
    //      \ /     \
    //       b(21)---d(42)
    //      /       /
    //    e(0)---f(0)
    */
    
    log(6);
    log(6, "eager 3...");
    a.set(20);
    assert_eq(d(), 40);
    /*
    // a(20)---c(20)
    //      \ /     \
    //       b(20)---d(40)
    //      /       /
    //    e(0)---f(0)
    */

    log(6);
    log(6, "eager 4...");
    a.set(22);
    assert_eq(d(), 44);
    /*
    // a(22)---c(22)
    //      \       \
    //       b(22)---d(44)
    //      /       /
    //    e(0)---f(0)
    */

    log(6);
    log(6, "eager 5...");
    a.set(23);
    assert_eq(d(), 45);
    /*
    // a(23)---c(22)
    //      \       \
    //       b(23)---d(45)
    //      /       /
    //    e(0)---f(0)
    */

    log(6);
    log(6, "eager 6...");
    e.set(1);
    assert_eq(d(), 47);
    /*
    // a(23)---c(22)
    //      \       \
    //       b(24)---d(47)
    //      /       /
    //    e(1)---f(1)
    */

/*    auto q = observable{0};
    auto r = computed{[=](auto self) { return q(self); return 0; }};
    auto s = computed{[=](auto self) { q(self); r(self);  }};
    
    */
    return 0;
  //  a.set(21);
    /*
    // a(21)---c(21)
    //      \ /     \
    //       b(21)---d(42)
    //      /       /
    //    e(0)---f(0)
    */

    auto r1 = reaction{"r1", [=](auto self) {
        d(self);
    }};
   /* depends(b, a);
    depends(b, e);
    depends(c, a);
    depends(c, b);
    depends(d, c);
    depends(d, b);
    depends(f, e);
    depends(d, f);
    depends(r1, d);*/
    /*
    // a(21)===c(21)
    //    \\  //   \\
    //     b(21)===d(42)===r1(!)
    //    //      //
    //  e(0)====f(0)
    */

/*    auto r2 = reaction{"r2", [=](auto self) {
        c(self);
    }};*/
   // depends(r2, c);

    /*
    // a(21)===c(21)===r2(!)
    //    \\  //   \\
    //     b(21)===d(42)===r1()
    //    //      //
    //  e(0)====f(0)
    */

//    a.set(22);
    // c no longer depends on b
    // d can be triggered by b or c
    /*
    // a(22)===c(22)===r2(!)
    //    \\  /    \\
    //     b(22)===d(44)===r1(!)
    //    //      //
    //  e(0)====f(0)
    */

  //  a.set(23);
    // d can be triggered only by b (c didn't change)
    /*
    // a(23)===c(22)===r2()
    //    \\  /    \\
    //     b(23)===d(45)===r1(!)
    //    //      //
    //  e(0)====f(0)
    */

   // e.set(1);
    // d can be triggered only by b (c didn't change)
    /*
    // a(23)===c(22)===r2()
    //    \\  /    \\
    //     b(24)===d(47)===r1(!)
    //    //      //
    //  e(1)====f(1)
    */

    return 0;
}
