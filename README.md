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

auto copy = name; // copy and name reference the same underlying value
name = "Jane Doe";
assert(copy == "Jane Doe");

copy = "John Doe";
assert(name == "John Doe");
```

## Calculated values

```cpp
using prego::calc;

atom first_name = "John"s;
atom last_name  = "Doe"s;

calc full_name = [=] { return first_name() + " " + last_name(); };
calc full_name = [=] { return first_name + " " + last_name; };
calc full_name = first_name + " " + last_name;

full_name = "Jane Doe"; // ❌ calculated values are not assignable
first_name = "Jane"; // ok

std::string value = full_name(); // calculate and return value

std::println(f"Name is {full_name}"); // uses previously-cached value

last_name = "Austen";
std::println(f"{full_name} is a writer"); // calculates a new value
```

## Complex calculated values

```cpp
using prego::calc;

atom first_name = "John"s;
atom last_name  = "Doe"s;
calc full_name  = first_name + " " + last_name;

calc is_writer  = [=] { return full_name().contains("Jane Austen"); };
calc is_writer  = [=] { return full_name.contains("Jane Austen"); };
calc is_writer  = full_name.contains("Jane Austen");
calc is_writer  = full_name == "Jane Austen";

std::println(f"{full_name} is not a writer");

first_name = "Jane";
std::println(f"{full_name} is not a writer");

last_name = "Austen";
std::println(f"{full_name} is a writer");

auto print_business_card = [=] {
  std::println(f"{full_name}");
  const auto line = std::string{full_name.size(), '-'};
  std::println(f"{line}");
  if (full_name == "Jane Austen")
    std::println(f"Profession: writer");
};

first_name = "John";
print_business_card();

last_name = "Doe";
print_business_card();

calc business_card = [=] {
  const auto line = std::string{full_name.size(), '-'};
  auto result = std::format(f"{full_name}\n{line}");
  if (full_name == "Jane Austen")
    result = std::format(f"{result}\nProfession: writer");

  return result;
};

first_name = "Jane";
std::println(f"{business_card}");

last_name = "Austen";
std::println(f"{business_card}");
```

## Even more complex calculated values

```cpp
using prego::calc;

atom first_name = "John"s;
atom last_name  = "Doe"s;
atom nick_name  = ""s;
calc full_name  = first_name + " " + last_name;
calc display_name = [=] {
  return !nick_name.empty() ? nick_name : full_name;
};

calc business_card = [=] {
  const auto line = std::string{display_name.size(), '-'};
  auto result = std::format(f"{display_name}\n{line}");
  if (full_name == "Jane Austen")
    result = std::format(f"{result}\nProfession: writer");

  return result;
};

first_name = "Jane";
std::println(f"{business_card}");

last_name = "Austen";
std::println(f"{business_card}");

nick_name = "Jane Austen";
std::println(f"{business_card}"); // unnecessary
```

```cpp
using prego::autorun;

atom first_name = "John"s;
atom last_name  = "Doe"s;
atom nick_name  = ""s;
calc full_name  = first_name + " " + last_name;
calc display_name = [=] {
  return !nick_name.empty() ? nick_name : full_name;
};

calc profession = [=] {
  return is_writer
       ? "writer"
       : get_profession_from_db(full_name);
};

calc business_card = [=] {
  const auto line = std::string{display_name.size(), '-'};
  auto result = std::format(f"{display_name}\n{line}");
  if (full_name == "Jane Austen")
    result = std::format(f"{result}\nProfession: writer");

  return result;
};

atom should_print_business_card = true;
autorun([=] {
  if (should_print_business_card)
    std::println(f"{business_card}");
});

// simple unsubscribe based on flag no longer works
atom should_mail_business_card = true;
autorun([=] {
  if (should_mail_business_card)
    mail(business_card);
});


nick_name = "John Doe";
first_name = "John";
last_name = "Doe";
```

## Requirements

- Send business card via mail each time it changes
- Allow user to opt out of mail delivery
- Send business card via email each time it changes (user can print at home)
- Allow user to opt out of email delivery
- user can change:
  - first name
  - last name
  - opt out settings
- technical:
  - reacts to modifications
  - reacts only to _changes_
  - does not perform repeated calculates
  - avoids expensive calculations if unnecessary
  - easy to extend
  - no implicit dependencies
  - local reasoning
  - at no point in the changes of the requirements did we have to come up with clever implementations, work arounds, or have to fix new issues later on: every change to the reactive data model is correct and efficient _by construction_.
  - Instead, with the incremental number of requirements, conventional designs (including the use of the observer pattern) require complete knowledge of the full data model, which makes bug-free extension very hard or impractical. The fundamental cause of these issues,
    is the implicit dependencies between the different pieces of state in the data model.

```

```

```cpp
atom first_name = "John"s;
atom last_name  = "Doe"s;
calc full_name  = first_name + " " + last_name;
atom pseudonym  = std::optional<std::string>{};

calc display_name = pseudonym.value_or(full_name);

calc is_writer = display_name == "Jane Austen"
              or display_name == "J.K. Rowling";
              or db_lookup(display_name);

calc business_card = [=] {
  return display_name
       + (is_writer ? ", writer" : "");
};

atom mail_opt_out = false;
autorun([=] {
  if (mail_opt_out) return;
  std::println(f"physical mailing: {business_card}");
});

atom email_opt_out = false;
autorun([=] {
  if (email_opt_out) return;
  std::println(f"electronic mailing: {business_card}");
});

first_name = "Jane";
last_name  = "Austen";

pseudonym  = "J.K. Rowling";
first_name = "Joane";
last_name  = "Rowling";
`
```
