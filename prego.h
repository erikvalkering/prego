#include <algorithm>
#include <cassert>
#include <iostream>
#include <map>
#include <memory>
#include <optional>
#include <ranges>
#include <set>
#include <utility>
#include <vector>

#define FWD(x) std::forward<decltype(x)>(x)

namespace prego {

auto to_string(auto x) { return x; }

auto log_(int level, auto... args) {
  switch (level) {
  default:
    break;
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

  std::cout << std::boolalpha << level << ": ";
  ((std::cout << to_string(args)), ...);
  std::cout << std::endl;
}

auto log(auto... args) { log_(args...); }

enum class notification_t {
  stale,
  changed,
  unchanged,
};

inline auto to_string(const notification_t notification) {
  switch (notification) {
  default:
    throw 0;
  case notification_t::stale:
    return "stale";
  case notification_t::changed:
    return "changed";
  case notification_t::unchanged:
    return "unchanged";
  }
}

struct observer_t {
  virtual void notify(const notification_t) = 0;
};

struct id_mixin {
  static inline int id_counter = 0;
  std::string id = std::to_string(id_counter++);
  ~id_mixin() { --id_counter; }
};

struct observable_t;
auto get_id(const std::weak_ptr<observer_t> &) -> std::string;
auto get_id(const observable_t &) -> std::string;

using observers_t =
    std::map<std::weak_ptr<observer_t>, bool, std::owner_less<>>;

inline decltype(auto) get_reactive(observers_t &observers,
                                   const std::weak_ptr<observer_t> &observer) {
  return observers[observer];
}

inline decltype(auto) contains(observers_t &observers,
                               const std::weak_ptr<observer_t> &observer) {
  return observers.contains(observer);
}

inline decltype(auto) extract(observers_t &observers,
                              const std::weak_ptr<observer_t> &observer) {
  const auto node = observers.extract(observer);
  return std::pair{node.key(), node.mapped()};
}

struct observable_t : id_mixin {
  observers_t observers = {};

  // hooks
  virtual void before_is_reactive() const {}
  virtual void before_observe(const std::weak_ptr<observer_t> &observer,
                              bool reactive) const {}
  virtual void before_is_up_to_date(bool reactive) const {}

  virtual void on_observers_changed() {}
  virtual bool is_up_to_date(bool reactive) = 0;

  auto is_reactive() const {
    before_is_reactive();
    auto is_true = [](auto x) { return x; };
    return std::ranges::any_of(observers | std::views::values, is_true);
  }

  void observe(const std::weak_ptr<observer_t> &observer, const bool reactive) {
    before_observe(observer, reactive);

    log(1, get_id(*this), ".observe(", get_id(observer), ", ", reactive, ")");
    if (reactive !=
        std::exchange(get_reactive(observers, observer), reactive)) {
      // TODO: Only if reactive == false, on_observers_changed() could
      // potentially do something (if this was the last remaining reactive
      // observer).
      // TODO: Also, on_reactive_changed() might be a better name
      on_observers_changed();
    }
  }

  // Called when an observer (e.g. a calc_state) gets destroyed
  // and needs to remove itself from the observable.
  void unobserve(const std::weak_ptr<observer_t> &observer) {
    log(1, get_id(*this), ".unobserve(<observer>)");

    assert(contains(observers, observer));
    const auto [_, reactive] = extract(observers, observer);

    // If the observer was not reactive, removing it won't affect the overall
    // reactive state of this observable, so we can early-exit.
    if (!reactive)
      return;

    // Now propagate the (potentially) new reactive state
    on_observers_changed();
  }
};

using observables_t = std::set<std::weak_ptr<observable_t>, std::owner_less<>>;

inline auto active_observers =
    std::vector<std::tuple<std::weak_ptr<observer_t>, observables_t *, bool>>{};

using scope_manager_t = std::vector<std::shared_ptr<observable_t>>;
inline auto global_scope_manager = scope_manager_t{};

inline auto notify_observers(observable_t &state,
                             const notification_t notification) {
  // log(1, "notify ", state.observers.size(), " observers");
  for (auto &observer : state.observers | std::views::keys) {
    if (auto p = observer.lock()) {
      p->notify(notification);
    } else {
      log(1, get_id(state), ": err - notify_observers");
    }
  }
}

inline auto get_id(const std::weak_ptr<observer_t> &observer) -> std::string {
  return get_id(*std::dynamic_pointer_cast<observable_t>(observer.lock()));
}

inline auto get_id(const observable_t &observable) -> std::string {
  return dynamic_cast<const id_mixin &>(observable).id;
}

template <typename From, typename To>
concept convertible_to = std::is_convertible_v<From, To> &&
                         requires { static_cast<To>(std::declval<From>()); };

template <typename T> struct atom_state : observable_t {
  T value;

  atom_state(auto &&value) : value{FWD(value)} {}
  ~atom_state() { log(1, "~", get_id(*this)); }

  virtual bool is_up_to_date(bool reactive) override final {
    before_is_up_to_date(reactive);

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
    log(6, get_id(*this), ".is_up_to_date() [true]");
    return true;
  }
};

auto &get_value(auto &state) { return state.value; }

auto &get(auto &observable) {
  if (active_observers.empty()) {
    // We temporarily create an autorun to force this entire
    // subgraph to become reactive. A side effect of being reactive
    // is that we can cheaply check whether nodes are up to date.
    auto reaction = autorun([=](auto get) { get(observable); }, nullptr);
    return observable.internal_get();
  }

  auto [observer, observables, reactive] = active_observers.back();
  auto &value = observable.internal_get(reactive);
  observables->insert(observable.state);
  observable.state->observe(observer, reactive);

  return value;
}

template <typename T, template <typename> class state_t = atom_state>
struct atom {
  std::shared_ptr<state_t<T>> state;

public:
  atom(const atom &) = default;
  atom &operator=(const atom &) = default;

  atom(atom &&) = default;
  atom &operator=(atom &&) = default;

  atom(convertible_to<T> auto &&value)
      : state{std::make_shared<state_t<T>>(FWD(value))} {}

  template <convertible_to<T> U>
  atom(atom<U> &&src) : atom{std::move(get_value(*src.state))} {}

  auto set(auto &&value) {
    log(1, "");
    log(1, get_id(*state), ".set(", value, ") [", get_value(*state), "]");
    const auto old_value = std::exchange(get_value(*state), FWD(value));
    if (get_value(*state) == old_value)
      return;
    log(1, get_id(*state), ": changed");

    // First sweep: mark all as stale
    notify_observers(*state, notification_t::stale);

    // Second sweep: update observers
    notify_observers(*state, notification_t::changed);
  }

  auto &operator=(auto &&value) {
    set(value);
    return *this;
  }

  auto &internal_get(bool reactive = false) const { return get_value(*state); }

  auto &operator()() const { return get(*this); }

  auto observers() const { return state->observers; }
};

template <typename T> atom(T &&) -> atom<T>;

inline constexpr auto noop_get = [](const auto &o) { return o.internal_get(); };
using noop_get_t = decltype(noop_get);

template <typename F, typename... Args>
concept invocable = std::is_invocable_v<F, Args...>;

template <typename F>
concept invocable_with_get = invocable<F, noop_get_t>;

auto get_result_t(invocable_with_get auto f) -> decltype(f(noop_get));
auto get_result_t(invocable auto f) -> decltype(f());

auto get_value(invocable_with_get auto &f, auto observer, auto &observables,
               bool reactive) {
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

auto get_value(invocable auto &f, auto observer, auto &observables,
               bool reactive) {
  active_observers.emplace_back(observer, &observables, reactive);
  auto value = f();
  active_observers.pop_back();
  return value;
}

template <typename F>
struct calc_state : observable_t,
                    observer_t,
                    std::enable_shared_from_this<calc_state<F>> {
private:
  using T = decltype(get_result_t(std::declval<F>()));

public:
  F f;
  std::optional<T> value;
  observables_t observables;
  int stale_count = 0;
  bool maybe_changed = false;

  explicit calc_state(auto &&f) : f{FWD(f)} {}
  ~calc_state() {
    log(1, "~calc::state_t(", get_id(*this), ")");
    const auto observer = this->weak_from_this();
    for (auto &observable : observables) {
      if (auto p = observable.lock()) {
        p->unobserve(observer);
      } else
        log(1, ": err - ~calc::state_t");
    }
  }

  virtual void notify(const notification_t notification) override final {
    log(1, get_id(*this), "(reactive=", is_reactive(),
        "): notify: ", notification);
    switch (notification) {
    default:
      assert(false);

    case notification_t::stale: {
      // Mark as stale and propagate only if
      // we were visited for the first time
      // and only if we are reactive
      if (stale_count++ == 0 and is_reactive())
        notify_observers(*this, notification);

      break;
    }

    case notification_t::changed:
    case notification_t::unchanged: {
      // If an observable was changed,
      // we need to recompute as well
      maybe_changed |= notification == notification_t::changed;
      log(1, get_id(*this), ": maybe_changed: ", maybe_changed);

      // Only continue when all observables have been updated
      if (--stale_count != 0)
        break;

      // If we are not reactive,
      // don't propagate nor recompute
      if (!is_reactive())
        break;

      // If all observables are up to date
      // and unchanged, propagate and early-exit
      if (!maybe_changed) {
        notify_observers(*this, notification_t::unchanged);
        break;
      }

      // Some observables have changed,
      // so we must recompute
      recompute(true);
      break;
    }
    }
  }

  virtual void on_observers_changed() override final {
    // TODO: can we move is_reactive() check to caller?
    if (!is_reactive()) {
      const auto observer = this->weak_from_this();
      for (auto &observable : observables)
        if (auto p = observable.lock())
          p->observe(observer, false);
        else
          log(1, get_id(*this), ": err - on_observers_changed");
    } else {
      // TODO: check if the opposite is already established:
      //       if we become reactive, does that mean that all
      //       observables were already reactive?
      // Assert that if we are reactive, all observables are also reactiveive.
      auto is_reactive = [](auto &o) { return o.lock()->is_reactive(); };
      assert(std::ranges::all_of(observables, is_reactive));
    }
  }

  virtual bool is_up_to_date(bool reactive) override final {
    before_is_up_to_date(reactive);

    if (!value)
      return false;
    if (maybe_changed)
      return false;
    if (stale_count != 0)
      return false;
    if (is_reactive())
      return true;

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
          p->observe(observer, true);
        }
      } else
        log(1, "err - is_up_to_date");

    return true;
  }

  auto compute(bool reactive) {
    log(1, get_id(*this), ": compute");

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
    // non-reactive.
    // NOTE: they should *not* be removed, for two reasons:
    // - they might become reactive again, and we don't
    //   want to loose their cache
    // - they might be lazily observed again,
    //   in which case the is_up_to_date() function
    //   will traverse the observables to figure out
    //   if it is still up to date.
    for (auto &observable : previous_observables) {
      if (observables.contains(observable))
        continue;

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

    log(1, get_id(*this), ": changed: ", changed);

    // Finally notify all the observers that we are up to date
    notify_observers(*this, changed ? notification_t::changed
                                    : notification_t::unchanged);
  }
};

// TODO: change into class instead of struct
template <typename F> struct calc {
public:
  std::shared_ptr<calc_state<F>> state;
  friend auto autorun(auto f, scope_manager_t *);

public:
  calc(const calc &) = default;
  calc &operator=(const calc &) = default;

  calc(calc &&) = default;
  calc &operator=(calc &&) = default;

  calc(convertible_to<F> auto &&f)
      : state{std::make_shared<calc_state<F>>(FWD(f))} {}

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

template <typename F> calc(F &&) -> calc<F>;

auto with_return(invocable_with_get auto f) {
  return [=](auto get) {
    f(get);
    return 0;
  };
}

auto with_return(invocable auto f) {
  return [=] {
    f();
    return 0;
  };
}

auto autorun(auto f, scope_manager_t *scope_manager = &global_scope_manager) {
  auto c = calc{with_return(f)};

  auto reaction = calc{[=](auto get) {
    get(c);
    return 0;
  }};
  reaction.internal_get(true);

  if (scope_manager)
    scope_manager->push_back(reaction.state);

  return reaction;
}

[[nodiscard]] auto autorun(auto f, std::nullptr_t) {
  return autorun(f, static_cast<scope_manager_t *>(nullptr));
}

template <typename Class, auto thunk> struct Thunk {
  const Class *obj;
  Thunk(Class *obj) : obj{obj} {}

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

template <typename Base> struct Computable : Base {
  using Class = Base;
};

#define PREGO_COMPUTED(name)                                                   \
  using name##_thunk_get =                                                     \
      Thunk<Class, [](auto &self, auto get) -> decltype(self.name(get)) {      \
        return self.name(get);                                                 \
      }>;                                                                      \
  using name##_thunk_simple =                                                  \
      Thunk<Class,                                                             \
            [](auto &self) -> decltype(self.name()) { return self.name(); }>;  \
  struct name##_thunk : name##_thunk_get, name##_thunk_simple {                \
    using name##_thunk_get::operator();                                        \
  };                                                                           \
                                                                               \
  calc<name##_thunk> name{this};

} // namespace prego
