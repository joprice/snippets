
#include <chrono>
#include <iostream>
#include <thread>
#include <vector>
#include <mutex>
#include <future>

// compile with clang++ -lboost_system -lboost_thread-mt -std=c++11 threads.cpp 

// boost provides promise style 'then' continuations
#define BOOST_THREAD_PROVIDES_FUTURE
#define BOOST_THREAD_PROVIDES_FUTURE_CONTINUATION
#include <boost/thread/future.hpp>

using namespace boost;

/**
 * lifted from http://channel9.msdn.com/Shows/Going+Deep/C-and-Beyond-2012-Herb-Sutter-Concurrency-and-Parallelism
 **/
template <class T> class monitor {
  private:
    // mutable: can be modified from const function
    mutable T t;
    mutable std::mutex m;

  public:
    monitor(T t_ = T()): t(t_) {}

    template<typename F>
    auto operator()(F f) const -> decltype(f(t)) {
      std::lock_guard<mutex>_(m);
      return f(t);
    }
};

void singleThread() {
  std::thread th([]() { 
    std::cout << "in thread\n";
  });
  std::cout << "in main\n";
  th.join();
}

void stdFuture() {
  auto f = std::async([]{
    return "a";
  });

  std::cout << f.get() << '\n';
}

future<int> boostFuture() {
  future<int> f = async([] {
    return 1;
  });

  return f.then([](future<int> i) {
    int val = i.get();
    std::cout << val << '\n';
    return val + 3;
  });
}

void multiple() {
  std::mutex cout_mutex;
  std::vector<std::thread> threads;

  for (int i = 0; i < 10; i++) {
    threads.push_back(std::thread([=, &cout_mutex]() {
      cout_mutex.lock();
      std::cout << "in thread " << i << '\n';
      cout_mutex.unlock();
    }));
  }

  for (auto& t : threads) {
    t.join();
  }
}

int main() {
  singleThread();
  multiple();
  stdFuture();
  future<int> j = boostFuture();

  std::cout << j.get() << '\n';

  return 0;
}

