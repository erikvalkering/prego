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

template<typename T>
struct observable;

template<typename T>
observable(T &&) -> observable<T>;

// TODO: rename to atom to free up name for observable concept
template<typename T>
struct observable {
    struct state_t : observable_state_t {
	T value;

	state_t(auto &&value) : value{FWD(value)} {}
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
    observable(const observable &) = default;
    observable &operator=(const observable &) = default;

    observable(observable &&) = default;
    observable &operator=(observable &&) = default;

    observable(convertible_to<T> auto &&value)
	: state{std::make_shared<state_t>(FWD(value))} {}

    template<typename U> requires convertible_to<U, T>
    observable(observable<U> &&src)
      : observable{std::move(src.state->value)} {}

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

    auto &get(bool reactive = false) const {
	return state->value;
    }

    auto observers() const {
	return state->observers;
    }
};

template<typename F>
struct computed;

template<typename F>
computed(F &&) -> computed<F>;

// TODO: change into class instead of struct
template<typename F>
struct computed {
private:
    using get_t = decltype([](const auto &o) { return o.get(); });
    using T = std::invoke_result_t<F, get_t>;

public:
    struct state_t : observable_state_t
	           , observer_t
		   , std::enable_shared_from_this<state_t> {
	F f;
	std::optional<T> value; 
        std::set<std::weak_ptr<observable_state_t>, std::owner_less<>> observables;
	int stale_count = 0;
	bool maybe_changed = false;

	explicit state_t(auto &&f) : f{FWD(f)} {}
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

	    auto value = f([&](auto observable) {
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
    computed(const computed &) = default;
    computed &operator=(const computed &) = default;

    computed(computed &&) = default;
    computed &operator=(computed &&) = default;

    computed(std::same_as<F> auto &&f)
	: state{std::make_shared<state_t>(FWD(f))} {}

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

} // namespace prego
