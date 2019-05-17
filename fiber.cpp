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
#include "fiber/yield.hpp"

using boost::asio::ip::tcp;
using error_code = boost::system::error_code;
using byteresult = std::tuple<std::string, error_code>;
template <typename T>
using future = boost::fibers::future<T>;
template <typename T>
using promise = boost::fibers::promise<T>;

using boost::fibers::asio::yield;

class session : public std::enable_shared_from_this<session> {
 public:
  session(tcp::socket socket) : socket_(std::move(socket)) {}

  std::string read_some(size_t size) {
    //    error_code ec;
    //    std::string data(size, 0);
    //    socket_.async_read_some(boost::asio::buffer(data, size), yield[ec]);

    promise<size_t> p;
    future<size_t> f(p.get_future());
    std::string data(size, 0);

    socket_.async_read_some(
        boost::asio::buffer(data, size),
        [p = std::move(p)](error_code ec, size_t read) mutable {
          if (!ec) {
            p.set_value(read);
          } else {
            p.set_exception(std::exception_ptr(std::make_exception_ptr(ec)));
          }
        });

    size_t read = f.get();
    data.resize(read);  // we could have received less than 'size'
    return data;

    //    if (ec) {
    //      throw boost::system::system_error(ec);
    //    }
    //
    //    return data;
  }

  void write(std::string data) {
    promise<size_t> p;
    future<size_t> f(p.get_future());

    boost::asio::async_write(
        socket_, boost::asio::buffer(data, data.size()),
        [p = std::move(p)](error_code ec, size_t written) mutable {
          if (!ec) {
            p.set_value(written);
          } else {
            p.set_exception(std::exception_ptr(std::make_exception_ptr(ec)));
          }
        });

    size_t w = f.get();
    assert(w == data.size());
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
  try {
    boost::asio::io_context io_context{};
    boost::fibers::use_scheduling_algorithm<boost::fibers::asio::round_robin>(
        io_context);

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

    io_context.run();
  } catch (std::exception &e) {
    std::cerr << "Exception: " << e.what() << "\n";
  }

  return 0;
}
