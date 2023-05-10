#include <cassert>
#include <concepts>
#include <iostream>
#include <map>
#include <memory>
#include <set>
#include <vector>

#define FWD(x) std::forward<decltype(x)>(x)

namespace prego {

auto to_string(auto x) { return x; }

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
    std::string id = {1, id_counter++};
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

using observables_t = std::set<std::weak_ptr<observable_state_t>, std::owner_less<>>;

auto active_observers = std::vector<std::tuple<observables_t *, bool>>{};

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

template<typename From, typename To>
concept convertible_to = 
    std::is_convertible_v<From, To> &&
    requires {
        static_cast<To>(std::declval<From>());
    };

// TODO: define concept
template<typename T>
concept observable = false;

template<typename T>
struct atom_state : observable_state_t {
    T value;

    atom_state(auto &&value) : value{FWD(value)} {}
    ~atom_state() {
	log(1, "~", id);
    }

    virtual bool is_up_to_date() const final {
	// We will end up here only if a direct
	// observer was not able to determine whether
	// it was up to date by looking at its own state.
	// In that case, it means that it was not notified
	// by any atom that it was changed
	// (using the stale_count).
	// Therefore, we know that this atom
	// did not change with respect to the previous time
	// that the observer was computed, and
	// must therefore be considered up-to-date.
	log(6, id, ".is_up_to_date() [true]");
	return true;
    }
};

template<typename T>
struct atom;

template<typename T>
atom(T &&) -> atom<T>;

template<typename T>
struct atom {
    using state_t = atom_state<T>;
    std::shared_ptr<state_t> state;

public:
    atom(const atom &) = default;
    atom &operator=(const atom &) = default;

    atom(atom &&) = default;
    atom &operator=(atom &&) = default;

    atom(convertible_to<T> auto &&value)
	: state{std::make_shared<state_t>(FWD(value))} {}

    template<typename U> requires convertible_to<U, T>
    atom(atom<U> &&src)
      : atom{std::move(src.state->value)} {}

    auto set(auto &&value) {
	log(1, "");
	log(1, state->id, ".set(", value, ") [", state->value, "]");
	const auto old_value = std::exchange(state->value, FWD(value));
	if (state->value == old_value)
	   return;
	log(1, state->id, ": changed");

        // First sweep: mark all as stale
        notify_all(state->id, state->observers, notification_t::stale);

        // Second sweep: update observers
        notify_all(state->id, state->observers, notification_t::changed);
    }

    auto &operator=(auto &&value) {
	set(value);
	return *this;
    }

    auto &internal_get(bool reactive = false) const {
	return state->value;
    }

    auto &get() const {
	if (active_observers.empty()) return internal_get();

	auto [observables, reactive] = active_observers.back();
	auto &value = internal_get(reactive);
	observables->insert(state);
	return value;
    }

    auto &operator()() const { return get(); }

    auto observers() const {
	return state->observers;
    }
};

template<typename F>
struct calc;

template<typename F>
calc(F &&) -> calc<F>;

inline constexpr auto noop_get = [](const auto &o) { return o.internal_get(); };
using noop_get_t = decltype(noop_get);

template<typename F, typename ...Args>
concept invocable = std::is_invocable_v<F, Args...>;

auto get_result_t(invocable<noop_get_t> auto f) -> decltype(f(noop_get));
auto get_result_t(invocable auto f) -> decltype(f());

auto get_value(invocable<noop_get_t> auto &f, auto &observables, bool reactive) {
    return f([&](auto observable) {
	// Before linking together the observer
	// and observable, get the value,
	// because the calc::internal_get() function
	// uses the link to determine whether
	// it should recompute (via is_reactive).
	// TODO: fix linking behaviour (should be done immediately, because of the above)
	auto value = observable.internal_get(reactive);

	// Register this observable
	observables.insert(observable.state);

	return value;
    });
}

auto get_value(invocable auto &f, auto &observables, bool reactive) {
    active_observers.emplace_back(&observables, reactive);
    auto value = f();
    active_observers.pop_back();
    return value;
}

// TODO: change into class instead of struct
template<typename F>
struct calc {
private:
    using T = decltype(get_result_t(std::declval<F>()));

public:
    struct state_t : observable_state_t
	           , observer_t
		   , std::enable_shared_from_this<state_t> {
	F f;
	std::optional<T> value; 
        observables_t  observables;
	int stale_count = 0;
	bool maybe_changed = false;

	explicit state_t(auto &&f) : f{FWD(f)} {}
        ~state_t() {
	    log(1, "~calc::state_t(", id, ")");
	    const auto observer = this->weak_from_this();
	    for (auto &observable : observables) {
		if (auto p = observable.lock()) {
		    log(1, p->id, ".unobserve(", id, ")");
		    p->unobserve(observer);
		}
	        else
		    log(1, ": err - ~calc::state_t");
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

	    for (auto &observable : observables)
		if (auto p = observable.lock())
		    log(1, "observable - ", p->id);
	    // Clear the old observables by moving them
	    // into a temporary variable, such that we can
	    // later compute the diff between them.
	    auto previous_observables = std::move(observables);

	    for (auto &observable : observables)
		if (auto p = observable.lock())
		    log(1, "moved observable - ", p->id);
	    for (auto &observable : previous_observables)
		if (auto p = observable.lock())
		    log(1, "prev observable - ", p->id);
	    // Create a reference to the current observer,
	    // which we will pass later while linking
	    // together the observer and observable.
	    const auto observer = this->weak_from_this();

	    auto value = get_value(f, observables, reactive);

	    // Now link them.
	    // TODO: this used to be done right after adding adding to the set
	    //       of observables. Did we change behaviour?
	    //       Maybe it was just because of the .lock()
	    //       (observables are weak_ptrs and observable.state
	    //       was shared_ptr)
	    for (auto &observable : observables)
		if (auto p = observable.lock())
		   p->observe(observer, reactive);
    
	    for (auto &observable : observables)
		if (auto p = observable.lock())
		    log(1, "after observable - ", p->id);
	    for (auto &observable : previous_observables)
		if (auto p = observable.lock())
		    log(1, "prev observable - ", p->id);

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

	    return value;
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
    calc(const calc &) = default;
    calc &operator=(const calc &) = default;

    calc(calc &&) = default;
    calc &operator=(calc &&) = default;

    calc(convertible_to<F> auto &&f)
	: state{std::make_shared<state_t>(FWD(f))} {}

    // TODO: reactive == true should not be accessible publicly,
    //       because there's no way to unsubscribe
    auto &internal_get(bool reactive = false) const {
	if (!state->is_up_to_date())
	    state->recompute(reactive);

	return *state->value;
    }

    auto &get() const {
	if (active_observers.empty()) return internal_get();

	auto [observables, reactive] = active_observers.back();
	auto &value = internal_get(reactive);
	observables->insert(state);
	return value;
    }

    auto &operator()() const { return get(); }

    auto observers() const {
	return state->observers;
    }
};

auto autorun(auto f, scope_manager_t *scope_manager = &global_scope_manager) {
    // TODO: void support (currently even missing get-less autorun support because of this)
    auto c = calc{[=](auto get) { f(get); return 0; }};

    auto reaction = calc{[=](auto get) { get(c); return 0; }};
    reaction.internal_get(true);

    if (scope_manager) scope_manager->push_back(reaction.state);

    return reaction;
}

[[nodiscard]] auto autorun(auto f, std::nullptr_t) {
    return autorun(f, static_cast<scope_manager_t *>(nullptr));
}

} // namespace prego
