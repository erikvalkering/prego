#include <prego/prego.h>

#include <string>

using namespace prego;
using namespace std;

int main() {
  auto first_name = atom{"Anita"s};
  auto last_name = atom{"Laera"s};
  auto nick_name = atom{""s};

  auto full_name = calc{[=](auto get) {
    std::cout << "calc full_name\n";
    if (get(nick_name) != "")
      return get(nick_name);
    else
      return get(first_name) + " " + get(last_name);
  }};

  auto display_full = atom{true};
  autorun([=](auto get) {
    std::cout << "calc autorun\n";
    if (get(display_full)) {
      const auto n = get(full_name);
      std::cout << ">> " << n << std::endl;
    } else
      std::cout << "disable autorun\n";
  });

  // Anita Laera
  first_name.set("Missi");
  // full_name >> autorun >> Missi Laera
  last_name.set("Valkering");
  // full_name >> autorun >> Missi Valkering
  first_name.set("Erik");
  // full_name >> autorun >> Erik Valkering
  nick_name.set("Erik Valkering");
  // full_name
  nick_name.set("Erik Engelbertus Johannes Valkering");
  // full_name >> autorun >> Erik Engelbertus Johannes Valkering
  display_full.set(false);
  // autorun >> disable
  nick_name.set("Ciri");
}
