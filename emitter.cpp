#include <boost/signals2.hpp>
#include <functional>
#include <iostream>

template <typename Tag, typename... EventArgs>
struct Emitter {
  constexpr void on(std::function<void(EventArgs...)> handler) {
    signal_.connect(handler);
  }

  constexpr void emit(EventArgs &&... args) {
    signal_(args...);
  }

 private:
  boost::signals2::signal<void(EventArgs...)> signal_;
};

struct t1 {};
struct t2 {};

struct E {
  Emitter<t1> t1;
  Emitter<t2, int> t2;
};

int main() {
  E e;

  e.t1.on([]() { std::cout << "callback t1" << std::endl; });
  e.t1.emit();

  e.t2.on([](int a) { std::cout << "callback t2 " << a << std::endl; });
  e.t2.emit(5);

  return 0;
}
