#pragma once

#include <algorithm>
#include <cassert>
#include <concepts>
#include <functional>
#include <iostream>
#include <memory>
#include <optional>
#include <ranges>
#include <set>
#include <string>
#include <tuple>
#include <type_traits>
#include <utility>
#include <vector>

#define FWD(x) std::forward<decltype(x)>(x)

namespace prego {

const auto &to_string(const auto &x) { return x; }

auto log_(int level, const auto &...args) {
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

auto log(const auto &...args) { log_(args...); }

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

struct hooks_mixin {
  mutable int is_up_to_date_counter = 0;
  mutable int observe_counter = 0;
  mutable std::vector<bool> observe_calls;

  void before_observe(const std::weak_ptr<prego::observer_t> &observer,
                      bool reactive) const {
    ++observe_counter;
    observe_calls.push_back(reactive);
  }

  void before_is_up_to_date(bool reactive) const { ++is_up_to_date_counter; }
};

struct observable_t;
auto get_id(const std::weak_ptr<observer_t> &) -> std::string;
auto get_id(const observable_t &) -> std::string;

template <typename Key, typename Value, typename Comparator = std::less<Key>>
class insertion_order_map {
  std::vector<std::pair<Key, Value>> nodes;

  inline auto find_node(this auto &&self, const auto &key) {
    return std::ranges::find_if(
        FWD(self).nodes,
        [&](auto &k) {
          auto cmp = Comparator{};
          return not cmp(k, key) and not cmp(key, k);
        },
        &std::pair<Key, Value>::first);
  }

public:
  auto size() const { return nodes.size(); }
  decltype(auto) begin(this auto &&self) { return FWD(self).nodes.begin(); }
  decltype(auto) end(this auto &&self) { return FWD(self).nodes.end(); }

  inline auto &operator[](const auto &key) {
    auto it = find_node(key);
    if (it != nodes.end()) {
      return it->second;
    }

    return nodes.emplace_back(key, Value{}).second;
  }

  inline decltype(auto) contains(const auto &key) const {
    return find_node(key) != nodes.end();
  }

  inline decltype(auto) extract(const auto &key) {
    struct node_handle {
      bool _empty;
      Key _key;
      Value _mapped;

      auto empty() const { return _empty; }
      auto &key() const { return _key; }
      auto &mapped() const { return _mapped; }
    };

    auto it = find_node(key);
    if (it == nodes.end())
      return node_handle{true};

    auto result = std::move(*it);
    nodes.erase(it);

    return node_handle{
        false,
        std::move(result.first),
        std::move(result.second),
    };
  }
};

struct observable_t : id_mixin, hooks_mixin {
  insertion_order_map<std::weak_ptr<observer_t>, bool, std::owner_less<>>
      observers = {};
  size_t reactive_observers_count = 0;

  virtual void on_nonreactive() {}
  virtual bool is_up_to_date(bool reactive) = 0;

  auto is_reactive() const { return reactive_observers_count != 0; }

  void observe(const std::weak_ptr<observer_t> &observer, const bool reactive) {
    before_observe(observer, reactive);

    log(1, get_id(*this), ".observe(", get_id(observer), ", ", reactive, ")");
    if (reactive != std::exchange(observers[observer], reactive)) {
      reactive_observers_count += (reactive ? +1 : -1);
      if (!is_reactive())
        on_nonreactive();
    }
  }

  // Called when an observer (e.g. a calc_state) gets destroyed
  // and needs to remove itself from the observable.
  template <std::derived_from<observer_t> T>
  void unobserve(const std::weak_ptr<T> &observer) {
    log(1, get_id(*this), ".unobserve(<observer>)");

    assert(observers.contains(observer));
    const auto node = observers.extract(observer);
    assert(not node.empty());

    // If the observer was not reactive, removing it won't affect the overall
    // reactive state of this observable, so we can early-exit.
    if (!node.mapped())
      return;

    --reactive_observers_count;

    // Now propagate the (potentially) new reactive state
    if (!is_reactive())
      on_nonreactive();
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
      assert(false);
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

template <typename T>
concept immovable = not(std::movable<T> or std::copyable<T>);

template <typename T> struct immovable_holder_t {
  std::unique_ptr<T> value;
  operator bool() const { return bool{value}; }
  decltype(auto) operator*(this auto &&self) { return *FWD(self).value; }
};

template <typename> constexpr auto is_immovable_holder = false;

template <typename T>
constexpr auto is_immovable_holder<immovable_holder_t<T>> = true;

template <typename T>
concept immovable_holder = is_immovable_holder<std::remove_cvref_t<T>>;

decltype(auto) indirect(immovable_holder auto &&holder) {
  return *FWD(holder).value;
}

decltype(auto) indirect(auto &&holder) { return FWD(holder); }

template <typename T> struct atom_state : observable_t {
  using holder_t = std::conditional_t<immovable<T>, immovable_holder_t<T>, T>;
  holder_t holder;

  atom_state() = default;

  atom_state(convertible_to<T> auto &&value)
      : atom_state{std::in_place, FWD(value)} {}

  atom_state(std::in_place_t, auto &&...args)
    requires(immovable<T>)
      : holder{std::make_unique<T>(FWD(args)...)} {}
  atom_state(std::in_place_t, auto &&...args)
    requires(not immovable<T>)
      : holder{FWD(args)...} {}

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
    // that the observer was calculated, and
    // must therefore be considered up-to-date.
    log(6, get_id(*this), ".is_up_to_date() [true]");
    return true;
  }
};

/// Returns a reference to the cached value, automatically linking together the
/// observer and observable.
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

struct magic_mixin;

template <typename Derived, typename Base>
concept derived_from = std::derived_from<std::remove_cvref_t<Derived>, Base>;

decltype(auto)
get_value_from_param(prego::derived_from<magic_mixin> auto &&param) {
  return FWD(param)();
}

decltype(auto) get_value_from_param(auto &&param) { return FWD(param); }

template <typename F> struct magic_wrapper;

template <typename F, typename... Args>
auto make_magic_wrapper(F f, Args &&...args) {
  return magic_wrapper{
      [f, args = std::tuple<Args...>{FWD(args)...}] -> decltype(auto) {
        return std::apply(
            [=](auto &&...args) -> decltype(auto) {
              return f(get_value_from_param(FWD(args))...);
            },
            std::move(args));
      }};
}

#define PREGO_DEFINE_MAGIC_OPERATOR(op)                                        \
  friend auto operator op(auto &&lhs, auto &&rhs) {                            \
    return make_magic_wrapper(                                                 \
        [](auto &&lhs, auto &&rhs) -> decltype(auto) {                         \
          return FWD(lhs) op FWD(rhs);                                         \
        },                                                                     \
        FWD(lhs), FWD(rhs));                                                   \
  }

#define PREGO_DEFINE_MAGIC_MEMBER(member)                                      \
  auto member(this auto &&self, auto &&...args) {                              \
    return make_magic_wrapper(                                                 \
        [](auto &&self, auto &&...args) -> decltype(auto) {                    \
          return FWD(self).member(FWD(args)...);                               \
        },                                                                     \
        FWD(self), FWD(args)...);                                              \
  }

struct magic_mixin {
  PREGO_DEFINE_MAGIC_OPERATOR(+);
  PREGO_DEFINE_MAGIC_OPERATOR(<=>);
  PREGO_DEFINE_MAGIC_OPERATOR(==);
  PREGO_DEFINE_MAGIC_OPERATOR(>);
  PREGO_DEFINE_MAGIC_OPERATOR(<);
  PREGO_DEFINE_MAGIC_OPERATOR(<=);
  PREGO_DEFINE_MAGIC_OPERATOR(>=);

  PREGO_DEFINE_MAGIC_MEMBER(size);
  PREGO_DEFINE_MAGIC_MEMBER(value_or);
};

#undef PREGO_DEFINE_MAGIC_MEMBER
#undef PREGO_DEFINE_MAGIC_OPERATOR

template <typename F> struct magic_wrapper : F, magic_mixin {
  using F::operator();

  using T = std::invoke_result_t<F>;
  operator T(this auto self) { return self(); }
};

} // namespace prego

template <std::derived_from<prego::magic_mixin> F> struct std::formatter<F> {
  constexpr auto parse(auto &ctx) { return ctx.begin(); }

  auto format(const F &f, auto &ctx) const {
    return std::format_to(ctx.out(), "{}", f());
  }
};

namespace prego {

template <typename T> struct atom : magic_mixin {
  using state_t = atom_state<T>;
  std::shared_ptr<state_t> state;

public:
  atom() : state{std::make_shared<state_t>()} {}

  atom(const atom &) = default;
  atom(atom &&) = default;

  atom(std::in_place_t, auto &&...args)
      : state{std::make_shared<state_t>(std::in_place, FWD(args)...)} {}

  // This constructor is necessary to
  // allow for the following syntaxes:
  // auto a = atom{42};
  // atom b = 42;
  //
  // The std::convertible_to is necessary to support:
  // auto c = atom<foo>{42};
  // auto d = atom<immovable>{42};
  atom(convertible_to<T> auto &&value)
    requires(not std::same_as<std::remove_cvref_t<decltype(value)>, atom>)
      : atom{std::in_place, FWD(value)} {}

  atom(std::in_place_type_t<T>, auto &&...args)
      : atom{std::in_place, FWD(args)...} {}

  template <convertible_to<T> U>
  atom(atom<U> &&src) : atom{std::move(src.state->holder)} {}

  auto set(auto &&holder) {
    log(1, "");
    log(1, get_id(*state), ".set(", indirect(holder), ") [",
        indirect(state->holder), "]");
    if (indirect(state->holder) == indirect(holder))
      return;

    state->holder = FWD(holder);
    log(1, get_id(*state), ": changed");

    // First sweep: mark all as stale
    notify_observers(*state, notification_t::stale);

    // Second sweep: update observers
    notify_observers(*state, notification_t::changed);
  }

  auto emplace(auto &&...args) {
    if constexpr (immovable<T>) {
      set(typename state_t::holder_t{std::make_unique<T>(FWD(args)...)});
    } else {
      set(T{FWD(args)...});
    }
  }

  auto &operator=(auto &&value) {
    set(value);
    return *this;
  }

  // Disallow assignment from atoms.
  // That would cause unexpected unwiring
  // of dependent calc nodes.
  atom &operator=(const atom &) = delete;
  atom &operator=(atom &&) = delete;

  /// Returns a reference to the cached value
  /// atom - stored value
  /// calc - (re)calculates, if necessary
  const auto &internal_get(bool reactive = false) const {
    return indirect(state->holder);
  }

  auto &operator()() const { return get(*this); }
  operator const T &() const { return (*this)(); }

  auto observers() const { return state->observers; }
};

template <typename T> atom(const T &) -> atom<T>;
template <typename T> atom(T &) -> atom<T>;
template <typename T> atom(T &&) -> atom<T>;
template <typename T> atom(std::in_place_type_t<T>, auto &&...) -> atom<T>;

template <typename T> auto make_atom(auto &&...args) {
  return atom<T>{std::in_place, FWD(args)...};
}

inline constexpr auto noop_get = [](const auto &o) -> decltype(auto) {
  return o.internal_get();
};
using noop_get_t = decltype(noop_get);

auto get_result_t(std::invocable<noop_get_t> auto f)
    -> std::remove_cvref_t<std::invoke_result_t<decltype(f), noop_get_t>>;
auto get_result_t(std::invocable auto f)
    -> std::remove_cvref_t<std::invoke_result_t<decltype(f)>>;
auto get_result_t(std::invocable<noop_get_t> auto f)
    -> std::remove_cvref_t<std::invoke_result_t<
        std::remove_cvref_t<std::invoke_result_t<decltype(f), noop_get_t>>>>
  requires std::derived_from<
      std::remove_cvref_t<std::invoke_result_t<decltype(f), noop_get_t>>,
      magic_mixin>;
auto get_result_t(std::invocable auto f)
    -> std::remove_cvref_t<std::invoke_result_t<
        std::remove_cvref_t<std::invoke_result_t<decltype(f)>>>>
  requires std::derived_from<
      std::remove_cvref_t<std::invoke_result_t<decltype(f)>>, magic_mixin>;

decltype(auto) get_value(std::invocable<noop_get_t> auto &f, auto observer,
                         auto &observables, bool reactive) {
  return f([&](auto observable) -> auto & {
    // Before linking together the observer
    // and observable, get the value,
    // because the calc::internal_get() function
    // uses the link to determine whether
    // it should recalculate (via is_reactive).
    // So linking them before retrieving the value,
    // might incorrectly flag the observable as
    // reactive and therefore up-to-date,
    // which would skip a recalculation and
    // return an outdated value instead.
    auto &value = observable.internal_get(reactive);

    // Register this observable
    observables.insert(observable.state);
    observable.state->observe(observer, reactive);

    return value;
  });
}

template <typename F> struct scope_guard {
  F f;
  ~scope_guard() { f(); };
};

decltype(auto) get_value(std::invocable auto &f, auto observer,
                         auto &observables, bool reactive) {
  active_observers.emplace_back(observer, &observables, reactive);
  auto _ = scope_guard{[] { active_observers.pop_back(); }};
  return f();
}

template <typename F>
struct calc_state : observable_t,
                    observer_t,
                    std::enable_shared_from_this<calc_state<F>> {
private:
  using T = decltype(get_result_t(std::declval<F>()));
  using holder_t =
      std::conditional_t<immovable<T>, immovable_holder_t<T>, std::optional<T>>;

public:
  F f;
  holder_t holder;
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
        assert(false);
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
      // we need to recalculate as well
      maybe_changed |= notification == notification_t::changed;
      log(1, get_id(*this), ": maybe_changed: ", maybe_changed);

      // Only continue when all observables have been updated
      if (--stale_count != 0)
        break;

      // If we are not reactive,
      // don't propagate nor recalculate
      if (!is_reactive())
        break;

      // If all observables are up to date
      // and unchanged, propagate and early-exit
      if (!maybe_changed) {
        notify_observers(*this, notification_t::unchanged);
        break;
      }

      // Some observables have changed,
      // so we must recalculate
      recalculate(true);
      break;
    }
    }
  }

  virtual void on_nonreactive() override final {
    const auto observer = this->weak_from_this();
    for (auto &observable : observables)
      if (auto p = observable.lock())
        p->observe(observer, false);
      else
        assert(false);
  }

  virtual bool is_up_to_date(bool reactive) override final {
    before_is_up_to_date(reactive);

    if (!holder)
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
          // TODO: this hierarchy is still traversed twice in case an
          // unobserved
          //       atom was changed. Create a test for this
          return false;
        else if (reactive) {
          // if this subtree was up to date and we are currently reactive,
          // mark it as reactive, such that the next time the subtree is
          // checked, it immediately returns true
          p->observe(observer, true);
        }
      } else
        assert(false);

    return true;
  }

  decltype(auto) calculate(bool reactive) {
    log(1, get_id(*this), ": calculate");

    // Continue reactively if we are either called
    // reactively (via observable::set()) or if we
    // are lazily get'ting the value out of an
    // already-reactive observable.
    reactive = reactive or is_reactive();

    // Clear the old observables by moving them
    // into a temporary variable, such that we can
    // later calculate the diff between them.
    auto previous_observables = std::move(observables);

    // Create a reference to the current observer,
    // which we will pass later while linking
    // together the observer and observable.
    const auto observer = this->weak_from_this();

    // This code should be executed after the function
    // has been invoked, to mark any non-reactive
    // dependencies as such.
    // However, in order te support functions that return
    // immovable types, we need to return the result immediately.
    // To achieve that, we use a scope_guard.
    auto _ = scope_guard{[&] {
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
          assert(false);
      }
    }};

    return get_value(f, observer, observables, reactive);
  }

  auto recalculate(bool reactive) {
    // Recalculate and determine whether we actually changed
    const auto old_holder = std::move(holder);

    if constexpr (immovable<T>) {
      holder =
          immovable_holder_t<T>{std::unique_ptr<T>{new T{calculate(reactive)}}};
    } else {
      holder = calculate(reactive);
    }

    const auto changed =
        !old_holder or (indirect(old_holder) != indirect(holder));

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
template <typename F> struct calc : magic_mixin {
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
  const auto &internal_get(bool reactive = false) const {
    if (!state->is_up_to_date(reactive))
      state->recalculate(reactive);

    return *state->holder;
  }

  auto &operator()() const { return get(*this); }

  using T = decltype(get_result_t(std::declval<F>()));
  operator const T &() const { return (*this)(); }

  auto observers() const { return state->observers; }
};

template <typename F> calc(const F &) -> calc<F>;
template <typename F> calc(F &) -> calc<F>;
template <typename F> calc(F &&) -> calc<F>;

template <typename T> auto fwd_capture(T &&x) { return std::tuple<T>(FWD(x)); }

template <typename T> decltype(auto) access(T &&x) {
  return std::get<0>(FWD(x));
}

auto with_return(auto &&f) {
  return [g = fwd_capture(FWD(f))](auto &&...args)
    requires requires { f(FWD(args)...); }
  {
    access(g)(FWD(args)...);
    return 0;
  };
}

auto autorun(auto &&f, scope_manager_t *scope_manager = &global_scope_manager) {
  // Insert a new calc node that simply calls f
  auto c = calc{with_return(FWD(f))};

  // Make sure that c becomes reactive by
  // adding a new calc node that depends on c.
  // Without this additional indirection,
  // c would not have any (reactive) observers
  // and would therefore determine that itself
  // is not reactive.
  // Note that this node itself is not reactive,
  // because it has no observers.
  auto reaction = calc{[=](auto get) {
    get(c);
    return 0;
  }};

  // Now trigger _reactive_ observation of c,
  // through this additional calc node.
  // We should _not_ trigger this in the usual way
  // (i.e. reaction()), because that would eventually
  // link this node as nonreactive from the perspective
  // of c, after the calculation finished.
  reaction.internal_get(true);

  if (scope_manager)
    scope_manager->push_back(reaction.state);

  return reaction;
}

[[nodiscard]] auto autorun(auto &&f, std::nullptr_t) {
  return autorun(FWD(f), static_cast<scope_manager_t *>(nullptr));
}

template <typename Class, auto thunk> struct Thunk {
  const Class *obj;
  Thunk(Class *obj) : obj{obj} {}

  auto operator()(auto get) const
    requires std::invocable<decltype(thunk), noop_get_t>
  {
    return thunk(*obj, get);
  }

  auto operator()() const
    requires std::invocable<decltype(thunk)>
  {
    return thunk(*obj);
  }
};

template <typename Base> struct Calculatable : Base {
  using Class = Base;
};

#define PREGO_CALCULATED(name)                                                 \
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
