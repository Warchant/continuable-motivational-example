//
// async_tcp_echo_server.cpp
// ~~~~~~~~~~~~~~~~~~~~~~~~~
//
// Copyright (c) 2003-2018 Christopher M. Kohlhoff (chris at kohlhoff dot com)
//
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//

#include <boost/asio.hpp>
#include <cstdlib>
#include <iostream>
#include <memory>
#include <utility>

#define CONTINUABLE_WITH_NO_EXCEPTIONS
#include <continuable/continuable.hpp>

using boost::asio::ip::tcp;

class session : public std::enable_shared_from_this<session> {
 public:
  session(tcp::socket socket) : socket_(std::move(socket)) {}

  void start() {
    do_read();
  }

 private:
  auto async_read_some() {
    return cti::promisify<size_t>::from(
        [&](auto &&... args) {
          socket_.async_read_some(std::forward<decltype(args)>(args)...);
        },
        boost::asio::buffer(data_, max_length));
  }

  void do_read() {
    async_read_some()
        .then([t = shared_from_this()](size_t len) {
          std::cout << "read " << len << " bytes\n";
          t->do_write(len);
        })
        .fail([](std::error_condition ec) {
          std::cerr << "async_read_some fail: " << ec.message() << "\n";
        });
  }

  auto async_write(size_t len) {
    return cti::promisify<size_t>::from(
        [&](auto &&... args) {
          boost::asio::async_write(socket_,
                                   std::forward<decltype(args)>(args)...);
        },
        boost::asio::buffer(data_, len));
  }

  void do_write(std::size_t length) {
    async_write(length)
        .then([t = shared_from_this()](size_t written) {
          std::cout << "written " << written << " bytes\n";
          t->do_read();
        })
        .fail([](std::error_condition ec) {
          std::cout << "async_write fail: " << ec.message() << "\n";
        });
  }

  tcp::socket socket_;
  enum { max_length = 1024 };
  char data_[max_length];
};

class server {
 public:
  server(boost::asio::io_context &io_context, short port)
      : acceptor_(io_context, tcp::endpoint(tcp::v4(), port)) {
    do_accept();
  }

 private:
  auto async_accept() {
    return cti::promisify<tcp::socket>::from([&](auto &&... args) {
      acceptor_.async_accept(std::forward<decltype(args)>(args)...);
    });
  }

  void do_accept() {
    async_accept()
        .then([&](tcp::socket socket) {
          std::make_shared<session>(std::move(socket))->start();
          do_accept();
        })
        .fail([&](std::error_condition ec) {
          std::cerr << "do_accept failed: " << ec.message() << "\n";
          do_accept();
        });
  }

  tcp::acceptor acceptor_;
};

int main(int argc, char *argv[]) {
  try {
    boost::asio::io_context io_context;

    server s(io_context, 1111);

    io_context.run();
  } catch (std::exception &e) {
    std::cerr << "Exception: " << e.what() << "\n";
  }

  return 0;
}
