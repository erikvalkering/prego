//TODO: are all the todos tested for breaking the app
//TODO: split calc() into a void func and was_changed() func
#include <algorithm>
#include <cassert>
#include <compare>
#include <concepts>
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
