//
// async_tcp_echo_server.cpp
// ~~~~~~~~~~~~~~~~~~~~~~~~~
//
// Copyright (c) 2003-2018 Christopher M. Kohlhoff (chris at kohlhoff dot com)
//
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//

#define BOOST_ASIO_NO_DEPRECATED

#include <boost/asio.hpp>
#include <boost/fiber/all.hpp>
#include <cstdlib>
#include <exception>
#include <functional>
#include <iostream>
#include <memory>
#include <utility>

#include "fiber/round_robin.hpp"

using boost::asio::ip::tcp;
using error_code = boost::system::error_code;
using byteresult = std::tuple<std::string, error_code>;
template <typename T>
using future = boost::fibers::future<T>;
template <typename T>
using promise = boost::fibers::promise<T>;

template <typename... Args>
struct state_captor {
  enum state_t { init, waiting, complete };

  typedef boost::fibers::detail::spinlock mutex_t;
  typedef std::unique_lock<mutex_t> lock_t;
  typedef boost::intrusive_ptr<state_captor> ptr_t;

  std::tuple<Args...> args;

  mutex_t mtx_{};
  state_t state_{init};
  boost::fibers::context *ctx_{boost::fibers::context::active()};

  void operator()(Args... a) {
    // If originating fiber is busy testing state_ flag, wait until it
    // has observed (completed != state_).
    lock_t lk{mtx_};
    state_t state = state_;
    // Notify a subsequent yield_completion::wait() call that it need not
    // suspend.
    state_ = state_t::complete;
    // set the error_code bound by yield_t
    args = std::make_tuple(a...);
    // unlock the lock that protects state_
    lk.unlock();
    // If ctx_ is still active, e.g. because the async operation
    // immediately called its callback (this method!) before the asio
    // async function called async_result_base::get(), we must not set it
    // ready.
    if (state_t::waiting == state) {
      // wake the fiber
      boost::fibers::context::active()->schedule(ctx_);
    }
  }

  auto get() {
    wait();
    return args;
  }

  void wait() {
    // yield_handler_base::operator()() will set state_ `complete` and
    // attempt to wake a suspended fiber. It would be Bad if that call
    // happened between our detecting (complete != state_) and suspending.
    lock_t lk{mtx_};
    // If state_ is already set, we're done here: don't suspend.
    if (complete != state_) {
      state_ = waiting;
      // suspend(unique_lock<spinlock>) unlocks the lock in the act of
      // resuming another fiber
      boost::fibers::context::active()->suspend(lk);
    }
  }
};

class session : public std::enable_shared_from_this<session> {
 public:
  session(tcp::socket socket) : socket_(std::move(socket)) {}

  std::string read_some(size_t size) {
    std::string data(size, 0);
    state_captor<error_code, size_t> c;

    socket_.async_read_some(boost::asio::buffer(data, size),
                            [&c](error_code e, size_t r) { c(e, r); });

    auto [ec, read] = c.get();

    if (ec) {
      throw boost::system::system_error(ec);
    }

    data.resize(read);  // we could have received less than 'size'
    return data;
  }

  void write(std::string data) {
    state_captor<error_code, size_t> c;

    boost::asio::async_write(
        socket_, boost::asio::buffer(data, data.size()),
        [&c](error_code ec, size_t written) mutable { c(ec, written); });

    auto [ec, written] = c.get();

    if (ec) {
      throw boost::system::system_error(ec);
    }

    assert(data.size() == written);
  }

  tcp::socket socket_;
};

class server {
 public:
  using callback_t = void(std::shared_ptr<session>);
  server(boost::asio::io_context &io_context, short port,
         std::function<callback_t> handler)
      : acceptor_(io_context, tcp::endpoint(tcp::v4(), port)),
        callback_(std::move(handler)) {}

  void listen() {
    do_accept();
  }

 private:
  void do_accept() {
    acceptor_.async_accept(
        [this](boost::system::error_code ec, tcp::socket socket) {
          if (!ec) {
            auto s = std::make_shared<session>(std::move(socket));
            boost::fibers::fiber([c = callback_, s = std::move(s)]() {
              c(s);
            }).detach();
          }

          do_accept();
        });
  }

 private:
  tcp::acceptor acceptor_;
  std::function<callback_t> callback_;
};

int main(int argc, char *argv[]) {
  using boost::fibers::asio::round_robin;
  try {
    boost::asio::io_context io_context{};
    boost::fibers::use_scheduling_algorithm<round_robin>(io_context);

    server s(io_context, 1111, [](std::shared_ptr<session> s) {
      std::cout << "new client "
                << "[fiber: " << boost::this_fiber::get_id()
                << "][thread: " << std::this_thread::get_id() << "]\n";
      try {
        while (true) {
          auto data = s->read_some(1000);
          std::cout << "recv: " << data << "\n";
          s->write(data);
        }
      } catch (const std::exception &e) {
        std::cerr << "caught exception: " << e.what() << "\n";
      }
    });

    s.listen();

    using std::chrono_literals::operator""ms;
    using std::chrono_literals::operator""s;

    io_context.run();
  } catch (std::exception &e) {
    std::cerr << "Exception: " << e.what() << "\n";
  }

  return 0;
}
