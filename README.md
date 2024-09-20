[![CMake on multiple platforms](https://github.com/erikvalkering/prego/actions/workflows/cmake-multi-platform.yml/badge.svg)](https://github.com/erikvalkering/prego/actions/workflows/cmake-multi-platform.yml)

# Example:
```cpp
atom first_name = "John"s;
atom last_name = "Doe"s;
atom nick_name = ""s;

calc full_name = [=] {
    return first_name() + " " + last_name();
};

calc display_name = [=] {
    if (nick_name() != "") {
        return nick_name();
    } else {
        return full_name();
    }
};

atom enabled = true;
autorun([=] {
    if (enabled()) {
      std::println(display_name());
    } 
}); // prints "John Doe"

first_name = "Jane"; // prints "Jane Doe"
nick_name = "Jane Doe"; // no change, nothing printed
first_name = "John"; // no change, nothing printed
enabled = false; // autorun re-evaluated, printing skipped
nick_name = "John Doe"; // autorun re-evaluated, printing
enabled = true; // autorun re-evaluated, prints "John Doe"
nick_name = ""; // no change, nothing printed
```

