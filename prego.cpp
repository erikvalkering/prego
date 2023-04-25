//TODO: are all the todos tested for breaking the app
//TODO: split calc() into a void func and was_changed() func
#include <algorithm>
#include <cassert>
#include <compare>
#include <functional>
#include <iostream>
#include <map>
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
    case 6: return;
    break;
    }

    std::cout << std::boolalpha << id << ": ";
    ((std::cout << to_string(args)), ...);
    std::cout << std::endl;
}

auto log(auto ...args) {
    log_(args...);
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

auto to_string(const notification_t notification) {
    switch (notification) {
	default: throw 0;
	case notification_t::stale: return "stale";
	case notification_t::changed: return "changed";
	case notification_t::unchanged: return "unchanged";
    }
}

struct observer_t {
    virtual void notify(const notification_t) = 0;
};

char id_counter = 'a';

struct observable_state_t {
    char id = id_counter++;
    std::map<std::weak_ptr<observer_t>, bool, std::owner_less<>> observers = {};

    virtual void on_observers_changed() {}
    virtual bool is_up_to_date() const = 0;

    auto is_reactive() const {
	for (auto &[_, reactive] : observers)
	    if (reactive)
		return true;

	return false;

	//return std::ranges::any_of(observers | std::views::values);
    }

    void observe(
	const std::weak_ptr<observer_t> &observer,
	const bool reactive
    ) {
	log(1, id, ".observe(", std::dynamic_pointer_cast<observable_state_t>(observer.lock())->id, ", ", reactive, ")");
	if (reactive != std::exchange(observers[observer], reactive))
	    on_observers_changed();
    }

    void unobserve(const std::weak_ptr<observer_t> &observer) {
	assert(observers.contains(observer));
	observers.erase(observer);
	on_observers_changed();
	// TODO: make on change more clever
	//       by comparing is reactive before with after
    }
};

using scope_manager_t = std::vector<std::shared_ptr<observable_state_t>>;
auto global_scope_manager = scope_manager_t{};

auto notify_all(auto id, auto &observers, const notification_t notification) {
    //log(1, "notify ", observers.size(), " observers");
    for (auto &[observer, _] : observers) {
	if (auto p = observer.lock()) {
	    p->notify(notification);
	}
	else {
	    log(1, id, ": err - notify_all");
	}
    }
}

template<typename T>
struct observable {
    struct state_t : observable_state_t {
	T value;

	explicit state_t(const T &value) : value{value} {}
	~state_t() {
	    log(1, "~", id);
	}

        virtual bool is_up_to_date() const final {
	    // We will end up here only if a direct
	    // observer was not able to determine whether
	    // it was up to date by looking at its own state.
	    // In that case, it means that it was not notified
	    // by any observable that it was changed
	    // (using the stale_count).
	    // Therefore, we know that this observable
	    // did not change with respect to the previous time
	    // that the observer was computed, and
	    // must therefore be considered up-to-date.
            return true;
        }
    };

    std::shared_ptr<state_t> state;

public:
    explicit observable(T value)
	: state{std::make_shared<state_t>(value)} {}

    auto set(auto &&value) {
	log(1, state->id, ".set(", value, ") [", state->value, "]");
	const auto old_value = std::exchange(state->value, value);
	if (state->value == old_value)
	   return;
	log(1, state->id, ": changed");

        // First sweep: mark all as stale
        notify_all(state->id, state->observers, notification_t::stale);

        // Second sweep: update observers
        notify_all(state->id, state->observers, notification_t::changed);
    }

    auto &get(bool reactive = false) const {
	return state->value;
    }

    auto observers() const {
	return state->observers;
    }
};

// TODO: change into class
template<typename F>
struct computed {
    using T = decltype(std::declval<F>()([](auto o) { return o.get(); }));

    struct state_t : observable_state_t
	           , observer_t
		   , std::enable_shared_from_this<state_t> {
	F f;
	std::optional<T> value; 
        std::set<std::weak_ptr<observable_state_t>, std::owner_less<>> observables;
	int stale_count = 0;
	bool maybe_changed = false;

	explicit state_t(const F &f) : f{f} {}
        ~state_t() {
	    log(1, "~computed::state_t(", id, ")");
	    const auto observer = this->weak_from_this();
	    for (auto &observable : observables) {
		if (auto p = observable.lock()) {
		    log(1, p->id, ".unobserve(", id, ")");
		    p->unobserve(observer);
		}
	        else
		    log(1, ": err - ~computed::state_t");
	    }
	}

	virtual void notify(const notification_t notification) final {
            log(1, id, "(reactive=", is_reactive(), "): notify: ", notification);
	    switch (notification) {
		default: assert(false);

	        case notification_t::stale: {
		    // Mark as stale and propagate only if
		    // we were visited for the first time
		    // and only if we are reactive
		    if (stale_count++ == 0 and is_reactive())
		    	notify_all(id, observers, notification);

		    break;
		}

		case notification_t::changed:
		case notification_t::unchanged: {
		    // If an observable was changed,
		    // we need to recompute as well
		    maybe_changed |= notification == notification_t::changed;
	            log(1, id, ": maybe_changed: ", maybe_changed);

		    // Only continue when all observables have been updated
		    if (--stale_count != 0) break;

		    // If we are not reactive,
		    // don't propagate nor recompute
		    if (!is_reactive()) break;

		    // If all observables are up to date 
		    // and unchanged, propagate and early-exit
		    if (!maybe_changed) {
			notify_all(id, observers, notification_t::unchanged);
		        break;
		    }

		    // Some observables have changed,
		    // so we must recompute
		    recompute(true);
		    break;
		}
	    }
	}

        virtual void on_observers_changed() final {
	    if (!is_reactive()) {
	        const auto observer = this->weak_from_this();
		for (auto &observable : observables)
		    if (auto p = observable.lock())
		        p->observe(observer, false);
	            else
		        log(1, id, ": err - on_observers_changed");
	    }
        }

        virtual bool is_up_to_date() const final {
            if (!value) return false;
            if (is_reactive()) return true;
	    if (maybe_changed) return false;
            if (stale_count != 0) return false;

	    // TODO: like this we get a lot of redundant is_up_to_date() checks
	    //       for non-reactive branches. Each time we visit a previously
	    //       calculated observable, we need to redetermine this.
	    //       Maybe pass a set with visited observables, which we can use
	    //       as an additional criterion.
	    // TODO: implement two-pass algorithm like with stale_count

	    // TODO: check correctness of stale_count check
	    // TODO: check correctness of this for-loop
	    for (auto &observable : observables)
		if (auto p = observable.lock()) {
		    if (!p->is_up_to_date())
			return false;
		}
	        else
		    log(1, "err - is_up_to_date");

	    return true;
        }

	auto compute(bool reactive) {
	    log(1, id, ": compute");

	    // Continue reactively if we are either called
	    // reactively (via observable::set()) or if we
	    // are lazily get'ting the value out of an
	    // already-reactive observable.
	    reactive = reactive or is_reactive();

	    // Clear the old observables by moving them
	    // into a temporary variable, such that we can
	    // later compute the diff between them.
	    auto previous_observables = std::move(observables);

	    // Create a reference to the current observer,
	    // which we will pass later while linking
	    // together the observer and observable.
	    const auto observer = this->weak_from_this();

	    return f([&](auto observable) {
		// Before linking together the observer
		// and observable, get the value,
		// because the computed::get() function
		// uses the link to determine whether
		// it should recompute (via is_reactive).
		auto value = observable.get(reactive);

		// After having obtained the value,
		// we can finally link them.
		observables.insert(observable.state);
	    	observable.state->observe(observer, reactive);

		return value;
	    });

	    // Set any previously-observed observables to
	    // non-reactive. NOTE: they should not be
	    // removed, because they might become reactive
	    // again, and we don't want to loose their cache.
	    for (auto &observable : previous_observables) {
		if (observables.contains(observable)) continue;

		if (auto p = observable.lock())
		    p->observe(observer, false);
		else
		    log(1, "err - compute");
	    }
	}

	auto recompute(bool reactive) {
	    // Recompute and determine whether we actually changed
	    const auto changed = value != std::exchange(value, compute(reactive));
	   
	    // Reset these data, otherwise this observable
	    // will be incorrectly considered as outdated.
	    stale_count = 0;
	    maybe_changed = false;

	    log(1, id, ": changed: ", changed);

	    // Finally notify all the observers that we are up to date
	    notify_all(id, observers, changed ? notification_t::changed
					      : notification_t::unchanged);
	}
    };

    std::shared_ptr<state_t> state;
    friend auto autorun(auto f, scope_manager_t *);

public:
    explicit computed(F f)
	: state{std::make_shared<state_t>(f)} {}

    // TODO: reactive == true should not be accessible publicly,
    //       because there's no way to unsubscribe
    auto &get(bool reactive = false) const {
	if (!state->is_up_to_date())
	    state->recompute(reactive);

	return *state->value;
    }

    auto observers() const {
	return state->observers;
    }
};

auto autorun(auto f, scope_manager_t *scope_manager = &global_scope_manager) {
    auto c = computed{[=](auto get) { f(get); return 0; }};

    auto reaction = computed{[=](auto get) { get(c); return 0; }};
    reaction.get(true);

    if (scope_manager) scope_manager->push_back(reaction.state);

    return reaction;
}

[[nodiscard]] auto autorun(auto f, std::nullptr_t) {
    return autorun(f, static_cast<scope_manager_t *>(nullptr));
}

auto join(auto ...args) {
    return (""s + ... + args);
}

auto all_tests_passed = true;
#define assert_eq(x, y, ...) { \
    auto $$ = x; \
    if ($$ != y) { \
        cout << "ASSERTION FAILED(" << __FUNCTION__ << "@" << __LINE__ << ")\n" \
	     << std::boolalpha \
             << "\t" << #x << " == " << y << "\n" \
             << "\t" << $$ << " != " << y << "\n" \
	     << "\t" << v2::join(__VA_ARGS__ ) << "\n"; \
	v2::all_tests_passed = false; \
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
	return get(first_name) + " " + get(last_name);
    }};

    auto y = false;
    auto display_name = computed{[=, &y](auto get) {
	y = true;
	return get(full_name);
        if (get(nick_name) != "")
    	    return get(nick_name);
        else
	    return get(full_name);
    }};

    auto z = false;
    auto enabled = observable{true};
    autorun([=, &z](auto get) {
	z = true;
	if (get(enabled)) {
	    get(display_name);
	}
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
    assert_eq(y, false, "display_name should not react, because full_name  did not change");
    assert_eq(z, true, "autorun should react to change of enabled");

    x = false;
    y = false;
    z = false;

    nick_name.set("John Doe");
    assert_eq(x, false, "full_name should not react to change of nick_name, because it does not observe it");
    assert_eq(y, false, "display_name should not react to change of nick_name, because it is not being observed right now");
    assert_eq(y, false, "autorun should not react to change of nick_name, because it is not being observed right now (through display_name)");

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
    assert_eq(y, true, "autorun should react to change of enabled");

    x = false;
    y = false;
    z = false;

    nick_name.set("");
    assert_eq(x, true, "full_name should recompute because it is being observed again");
    assert_eq(y, true, "display_name should recompute, because full_name changed, as well as nick_name");
    assert_eq(y, false, "autorun should not react, because display_name did not change");
    // TODO: rename computed to derived?
    // TODO: rename autorun to observe or observer or reaction or effect? or reactive?
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

auto test_syntaxes() {
    auto a = observable{42};

    // alternative syntax observable: function instead of class
    //   which might be more flexible in the design space of the
    //   implementation. Same for computed.
    auto b = observable(42);

    // mutation syntax 1: set
    a.set(1729);

    // mutation syntax 2: direct assignment
    // a = 1729;

    // computed syntax 1: get as parameter
    //   - thread-safe
    //   - may allow for optimizations
    auto c = computed{[=](auto get) { return get(a) + get(b); }};

    // computed syntax 2: parameterless function
    //   - not thread-safe
    //   - maybe more difficult to optimize
    //   - less boilerplate
    // auto d = computed{[=] { return a() + b(); }};

    // computed syntax 3: parameterless function + smartref
    //   - not thread-safe
    //   - maybe more difficult to optimize
    //   - minimal boilerplate (except for user defined types)
    //   - maybe not worth the effort, considering only 2 chars per observable
    // auto e = computed{[=] { return a + b; }};

    // computed syntax 4: static get
    //   - thread-safe
    //   - may allow for more optimization
    //   - every observable/computed should be declared globally and becomes stateless
    //   - does every observable have to be unique?
    // auto f = computed{[](auto get) { return get(a) + get(b); }};
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
    //test_noncopyable_types();
    //test_immovable_types();

    if (all_tests_passed)
        std::cout << "all tests passed\n";
    else
	std::cout << "some tests failed\n";
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
