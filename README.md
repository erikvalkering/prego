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
    std::println(display_name); // prints "Mr Unknown"
});

nickname.reset();               // prints "John Doe"
first_name = "Jane";            // prints "Jane Doe"
nick_name = "Jane Doe";         // no change, nothing printed
first_name = "John";            // no change, nothing printed
enabled = false;                // autorun re-evaluated, nothing printed
nick_name = "John Doe";         // autorun not re-evaluated, nothing printed
enabled = true;                 // autorun re-evaluated, prints "John Doe"
nick_name.reset();              // no change, nothing printed
```
