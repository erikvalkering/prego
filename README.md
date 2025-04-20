# Prego

[![CMake on multiple platforms](https://github.com/erikvalkering/prego/actions/workflows/cmake-multi-platform.yml/badge.svg)](https://github.com/erikvalkering/prego/actions/workflows/cmake-multi-platform.yml)

## Example

```cpp
atom first_name = "John"s;
atom last_name = "Doe"s;
atom nick_name = std::optional{"Mr Unknown"s};

calc full_name = first_name + " " + last_name;
calc display_name = nick_name.value_or(full_name);

atom enabled = true;
autorun([=] {
  if (enabled)
    std::println("{}", display_name); // prints "Mr Unknown"
});

nick_name.reset();      // prints "John Doe"
first_name = "Jane";    // prints "Jane Doe"
nick_name = "Jane Doe"; // no change, nothing printed
first_name = "John";    // no change, nothing printed
enabled = false;        // autorun re-evaluated, nothing printed
nick_name = "John Doe"; // autorun not re-evaluated, nothing printed
enabled = true;         // autorun re-evaluated, prints "John Doe"
nick_name.reset();      // no change, nothing printed
```

## Introduction

### Atomic values

```cpp
#include <prego/prego.h>

using prego::atom;

// The following are equivalent
auto first_name = atom<std::string>{"John"s}; // full declaration
auto first_name = atom{"John"s};              // type is deduced (via ctad)
atom first_name = "John"s;                    // terse form
```

<details>
<summary>Advanced</summary>
  #### Immovable types
  ```cpp
  struct immovable {
    immovable(int x) {}
    immovable(int x, int y) {}

    immovable(const immovable&) = delete;
    immovable(immovable&&) = delete;
    immovable& operator=(const immovable&) = delete;
    immovable& operator=(immovable&&) = delete;
  };

  atom a = atom<immovable>{42};

  // The following are equivalent
  atom a = atom<immovable>{std::in_place, 42, 1729};
  atom<immovable> a = {std::in_place, 42, 1729};
  atom a = {std::in_place_type<immovable>, 42, 1729};

  // Alternatively, using a factory function
  atom a = make_atom<immovable>(42, 1729);

  ```
</details>

### Using values

```cpp
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
auto full_name = first_name() + " " + last_name();
auto full_name = first_name + " " + last_name;

auto length = (first_name + " " + last_name).size();
std::println("Length: {}", length);
```

> Note: `auto length = ...` does not deduce to size_t, but _acts_ like it.

### Shared reference semantics

```cpp
auto copy = name;
name = "Jane";
assert(copy == "Jane");

copy = "John";
assert(name == "John");
```
