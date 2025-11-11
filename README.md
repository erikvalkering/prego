# Prego

[![CMake on multiple platforms](https://github.com/erikvalkering/prego/actions/workflows/cmake-multi-platform.yml/badge.svg)](https://github.com/erikvalkering/prego/actions/workflows/cmake-multi-platform.yml)

## Example

```cpp
atom first_name = "John"s;
atom last_name  = "Doe"s;
atom nick_name  = std::optional{"Mr Unknown"s};

calc full_name    = first_name + " " + last_name;
calc display_name = nick_name.value_or(full_name);

atom enabled = true;
autorun([=] {
  if (not enabled) return;
  std::println("{}", display_name); // "Mr Unknown"
});

nick_name.reset();       // "John Doe"
first_name = "Jane";     // "Jane Doe"
nick_name  = "Jane Doe"; // -
first_name = "John";     // -
enabled    = false;      // -
nick_name  = "John Doe"; // -
enabled    = true;       // "John Doe"
nick_name.reset();       // -
```

## Introduction

### Atomic values

```cpp
#include <prego/prego.h>

using prego::atom;

// The following are equivalent
auto name = atom<std::string>{"John Doe"s}; // full declaration
auto name = atom{"John Doe"s};              // type is deduced (via ctad)
atom name = "John Doe"s;                    // terse form
```

<details>
<summary>Advanced</summary>

#### Immovable types

```cpp
struct immovable {
  immovable(int x) {}
  immovable(int x, int y) {}

  immovable(const immovable &) = delete;
  immovable(immovable &&) = delete;
  immovable &operator=(const immovable &) = delete;
  immovable &operator=(immovable &&) = delete;
};

// The following are equivalent
atom a = atom<immovable>{std::in_place, 42, 1729};
atom<immovable> a = {std::in_place, 42, 1729};
atom a = {std::in_place_type<immovable>, 42, 1729};

// Alternatively, using a factory function
atom a = make_atom<immovable>(42, 1729);

// Conversion is supported out of the box
atom a = atom<immovable>{42};
```

</details>

### Using values

```cpp
atom name = "John Doe"s;

std::string value = name(); // access value
std::string value = name;   // also fine: implicit conversion

std::println("Name is {}", name());
std::println("Name is {}", name); // builtin formatting support
```

### Combining values

```cpp
atom first_name = "John"s;
atom last_name  = "Doe"s;

// The following are equivalent
std::string full_name = first_name() + " " + last_name();
std::string full_name = first_name + " " + last_name;

// As well as the following
std::size_t length = (first_name() + " " + last_name()).size();
std::size_t length = (first_name + " " + last_name).size();
```

<details>
<summary>Advanced</summary>

Note: we rely on implicit conversions here. For example, `auto length = ...` would _not_ deduce to `size_t`, but would _act_ like it.
In fact, the following:

```cpp
auto length = (first_name + " " + last_name).size();

```

creates a _lazily evaluated expression_. It will only be evaluated if called (i.e. `length()`) or when implicitly (or explicitly) converted to its underlying type (i.e. `std::size_t l = length;`).

</details>

### Shared reference semantics

```cpp
atom name = "John Doe"s;

auto copy = name;  // copy and name re-
name = "Jane Doe"; // ference the same
                   // underlying value
assert(copy == "Jane Doe");

copy = "John Doe";
assert(name == "John Doe");
```

## Calculated values

```cpp
using prego::calc;

atom first_name = "John"s;
atom last_name  = "Doe"s;

// The following are equivalent
calc full_name = [=] { return first_name() + " " + last_name(); };
calc full_name = [=] { return first_name + " " + last_name; };
calc full_name = first_name + " " + last_name;

// Only atomic values can be assigned to.
// Calculated values cannot.
full_name  = "Jane Doe"; // ❌
first_name = "Jane";     // ok

// The following are equivalent
std::string value = full_name(); // calculate and return value
std::string value = full_name;   // or, with implicit conversion

// Calculated values cache their value once calculated
std::println("Name is {}", full_name); // uses cached value

// Calculated values automatically track their dependencies
// and recalculate only if necessary and when requested.
last_name = "Austen";
std::println("{} is a writer", full_name); // (re)calculates a new value

// But if nothing changed,
// nothing needs to be recalculated.
first_name = "Jane";
std::println("{} is a writer", full_name); // uses cached value
```

## Reactions

```cpp
using prego::calc;

atom first_name = "John"s;
atom last_name  = "Doe"s;

calc full_name  = first_name + " " + last_name;
calc is_writer  = full_name == "Jane Austen";

atom enabled = true;
autorun([=] {
  // This will be invoked immediately,
  // as well as each time a dependency changed.
  if (not enabled) return;

  // prints "Name: John Doe"
  std::println("Name: {}{}",
               full_name,
               is_writer ? ", writer" : "");
});

// prints "Name: Jane Doe"
first_name = "Jane";

// prints "Name: Jane Austen, writer"
last_name  = "Austen";

// still invokes autorun,
// but early-exits
enabled = false;

// does not invoke autorun,
// and does not recalculate full_name
// nor is_writer
first_name = "John";

// prints "Name: John Austen"
enabled = true;
```
