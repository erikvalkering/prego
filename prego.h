#include <algorithm>
#include <cassert>
#include <concepts>
#include <iostream>
#include <map>
#include <memory>
#include <ranges>
#include <optional>
#include <set>
#include <utility>
#include <vector>

#define FWD(x) std::forward<decltype(x)>(x)

namespace prego {

auto to_string(auto x) { return x; }

auto log_(int id, auto... args) {
    switch (id) {
        default: break;
        case 0:
        case 1:
        case 2:
        case 3:
        case 4:
        case 5:
        case 6: {
            return;
            break;
        }
    }

    std::cout << std::boolalpha << id << ": ";
    ((std::cout << to_string(args)), ...);
    std::cout << std::endl;
}

auto log(auto... args) { log_(args...); }

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

inline constexpr auto contains = [](auto &&rng, auto value) {
    return std::ranges::find(rng, value)
	!= std::ranges::end(rng);
};

struct observable_state_t {
    std::string id = { 1, id_counter++ };
    std::map<std::weak_ptr<observer_t>, bool, std::owner_less<>> observers = {};

    virtual void on_observers_changed() {}
    virtual bool is_up_to_date(bool reactive) = 0;

    auto is_reactive() const {
        return contains(observers | std::views::values, true);
    }

    void observe(const std::weak_ptr<observer_t> &observer, const bool reactive) {
        const auto observer_id = std::dynamic_pointer_cast<observable_state_t>(observer.lock())->id;
        log(1, id, ".observe(", observer_id, ", ", reactive, ")");
        if (reactive != std::exchange(observers[observer], reactive))
	    // TODO: Only if reactive == false, on_observers_changed() could potentially do something
	    // (if this was the last remaining reactive observer).
	    // TODO: Also, on_reactive_changed() might be a better name
            on_observers_changed();
    }

    void unobserve(const std::weak_ptr<observer_t> &observer) {
        assert(observers.contains(observer));
        observers.erase(observer);
	// TODO: if we repeatedly call unobserve(), and this node remains unreactive,
	// the on_observers_changed will do unneccesary repeated work
	// Better would be to determine if the is_reactive() condition changed,
	// and only call if it did.
        on_observers_changed();
    }
};

using observables_t = std::set<std::weak_ptr<observable_state_t>, std::owner_less<>>;

auto active_observers = std::vector<
    std::tuple<std::weak_ptr<observer_t>, observables_t *, bool>
>{};

using scope_manager_t = std::vector<std::shared_ptr<observable_state_t>>;
auto global_scope_manager = scope_manager_t{};

auto notify_all(auto id, auto &observers, const notification_t notification) {
    // log(1, "notify ", observers.size(), " observers");
    for (auto &[observer, _] : observers) {
        if (auto p = observer.lock()) {
            p->notify(notification);
        } else {
            log(1, id, ": err - notify_all");
        }
    }
}

template<typename From, typename To>
concept convertible_to =
    std::is_convertible_v<From, To>
    && requires { static_cast<To>(std::declval<From>()); };

// TODO: define concept
template<typename T>
concept observable = false;

template<typename T>
struct atom_state : observable_state_t {
    T value;

    atom_state(auto &&value) : value{ FWD(value) } {}
    ~atom_state() { log(1, "~", id); }

    virtual bool is_up_to_date(bool reactive) final {
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

auto &get(auto &observable) {
    if (active_observers.empty()) {
        auto reaction = autorun([=](auto get) { get(observable); }, nullptr);	
	return observable.internal_get();
    }

    auto [observer, observables, reactive] = active_observers.back();
    auto &value = observable.internal_get(reactive);
    observables->insert(observable.state);
    observable.state->observe(observer, reactive);

    return value;
}

template<typename T, template<typename> class state_t = atom_state>
struct atom;

template<typename T>
atom(T &&) -> atom<T>;

template<typename T, template<typename > class state_t>
struct atom {
    std::shared_ptr<state_t<T>> state;

public:
    atom(const atom &) = default;
    atom &operator=(const atom &) = default;

    atom(atom &&) = default;
    atom &operator=(atom &&) = default;

    atom(convertible_to<T> auto &&value)
        : state{ std::make_shared<state_t<T>>(FWD(value)) } {}

    template<convertible_to<T> U>
    atom(atom<U> &&src)
        : atom{ std::move(src.state->value) } {}

    auto set(auto &&value) {
        log(1, "");
        log(1, state->id, ".set(", value, ") [", state->value, "]");
        const auto old_value = std::exchange(state->value, FWD(value));
        if (state->value == old_value) return;
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

    auto &operator()() const { return get(*this); }

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

template<typename F, typename... Args>
concept invocable = std::is_invocable_v<F, Args...>;

template<typename F>
concept invocable_with_get = invocable<F, noop_get_t>;

auto get_result_t(invocable_with_get auto f) -> decltype(f(noop_get));
auto get_result_t(invocable auto f) -> decltype(f());

auto get_value(invocable_with_get auto &f, auto observer, auto &observables, bool reactive) {
    return f([&](auto observable) {
        // Before linking together the observer
        // and observable, get the value,
        // because the calc::internal_get() function
        // uses the link to determine whether
        // it should recompute (via is_reactive).
	// So linking them before retrieving the value,
	// might incorrectly flag the observable as
	// reactive and therefore up-to-date,
	// which would skip a recomputation and
	// return an outdated value instead.
        auto value = observable.internal_get(reactive);

        // Register this observable
        observables.insert(observable.state);
	observable.state->observe(observer, reactive);

        return value;
    });
}

auto get_value(invocable auto &f, auto observer, auto &observables, bool reactive) {
    active_observers.emplace_back(observer, &observables, reactive);
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
    struct state_t
        : observable_state_t
        , observer_t
        , std::enable_shared_from_this<state_t> {
        F f;
        std::optional<T> value;
        observables_t observables;
        int stale_count = 0;
        bool maybe_changed = false;

        explicit state_t(auto &&f) : f{ FWD(f) } {}
        ~state_t() {
            log(1, "~calc::state_t(", id, ")");
            const auto observer = this->weak_from_this();
            for (auto &observable : observables) {
                if (auto p = observable.lock()) {
                    log(1, p->id, ".unobserve(", id, ")");
                    p->unobserve(observer);
                } else
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
	    else {
		auto is_reactive = [](auto &o) { return o.lock()->is_reactive(); };
		auto reactive = observables | std::views::transform(is_reactive);
		assert(!contains(reactive, false));
	    }
	    // TODO: check if the opposite is already established:
	    //       if we become reactive, does that mean that all
	    //       observables were already reactive?
        }

        virtual bool is_up_to_date(bool reactive) final {
            if (!value) return false;
            if (maybe_changed) return false;
            if (stale_count != 0) return false;
            if (is_reactive()) return true;

            // TODO: check correctness of stale_count check
            // TODO: check correctness of this for-loop
            const auto observer = this->weak_from_this();
            for (auto &observable : observables)
                if (auto p = observable.lock()) {
                    if (!p->is_up_to_date(reactive))
			// TODO: this hierarchy is still traversed twice in case an unobserved
			//       atom was changed. Create a test for this
                        return false;
		    else if (reactive) {
			// if this subtree was up to date and we are currently reactive,
			// mark it as reactive, such that the next time the subtree is
			// checked, it immediately returns true
			p->observe(observer, reactive);
		    }
                } else
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

            auto value = get_value(f, observer, observables, reactive);

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
            notify_all(id, observers, changed ? notification_t::changed : notification_t::unchanged);
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
        : state{ std::make_shared<state_t>(FWD(f)) } {}

    // TODO: reactive == true should not be accessible publicly,
    //       because there's no way to unsubscribe
    auto &internal_get(bool reactive = false) const {
        if (!state->is_up_to_date(reactive))
            state->recompute(reactive);

        return *state->value;
    }

    auto &operator()() const { return get(*this); }

    auto observers() const { return state->observers; }
};

auto with_return(invocable_with_get auto f) {
    return [=](auto get) { f(get); return 0; };
}

auto with_return(invocable auto f) {
    return [=] { f(); return 0; };
}

auto autorun(auto f, scope_manager_t *scope_manager = &global_scope_manager) {
    auto c = calc{ with_return(f) };

    auto reaction = calc{ [=](auto get) { get(c); return 0; } };
    reaction.internal_get(true);

    if (scope_manager) scope_manager->push_back(reaction.state);

    return reaction;
}

[[nodiscard]] auto autorun(auto f, std::nullptr_t) {
    return autorun(f, static_cast<scope_manager_t *>(nullptr));
}

template<typename Class, auto thunk>
struct Thunk {
    const Class *obj;
    Thunk(Class *obj) : obj{ obj } {}

    auto operator()(auto get) const
        requires prego::invocable_with_get<decltype(thunk)>
    {
        return thunk(*obj, get);
    }

    auto operator()() const
        requires prego::invocable<decltype(thunk)>
    {
        return thunk(*obj);
    }
};

template<typename Base>
struct Computable : Base {
    using Class = Base;
};

#define PREGO_COMPUTED(name)                                                                                   \
    using name##_thunk_get =                                                                                   \
        Thunk<Class, [](auto &self, auto get) -> decltype(self.name(get)) { return self.name(get); }>;         \
    using name##_thunk_simple = Thunk<Class, [](auto &self) -> decltype(self.name()) { return self.name(); }>; \
    struct name##_thunk                                                                                        \
        : name##_thunk_get                                                                                     \
        , name##_thunk_simple {                                                                                \
        using name##_thunk_get::operator();                                                                    \
    };                                                                                                         \
                                                                                                               \
    calc<name##_thunk> name{ this };

} // namespace prego
