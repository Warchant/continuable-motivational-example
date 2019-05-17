//          Copyright Oliver Kowalke 2013.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#ifndef BOOST_FIBERS_ASIO_ROUND_ROBIN_H
#define BOOST_FIBERS_ASIO_ROUND_ROBIN_H

#include <chrono>
#include <cstddef>
#include <memory>
#include <mutex>

#include <boost/asio.hpp>
#include <boost/asio/steady_timer.hpp>
#include <boost/assert.hpp>
#include <boost/config.hpp>

#include <boost/fiber/condition_variable.hpp>
#include <boost/fiber/context.hpp>
#include <boost/fiber/mutex.hpp>
#include <boost/fiber/operations.hpp>
#include <boost/fiber/scheduler.hpp>

namespace boost::fibers::asio {

  class round_robin : public algo::algorithm {
   private:
    boost::asio::io_context &io_svc_;
    boost::asio::steady_timer suspend_timer_;
    boost::fibers::scheduler::ready_queue_type rqueue_{};
    boost::fibers::mutex mtx_{};
    boost::fibers::condition_variable cnd_{};
    std::size_t counter_{0};

   public:
    using executor_type = boost::asio::io_context::executor_type;
    using executor_work_guard = boost::asio::executor_work_guard<executor_type>;
    struct service : public boost::asio::io_context::service {
      static boost::asio::io_context::id id;

      executor_work_guard work_;

      explicit service(boost::asio::io_context &io_svc)
          : boost::asio::io_context::service(io_svc),
            work_{boost::asio::make_work_guard(io_svc)} {}

      ~service() override = default;

      service(service const &) = delete;
      service &operator=(service const &) = delete;

      void shutdown() final {
        work_.reset();
      }
    };

    explicit round_robin(boost::asio::io_context &io_svc)
        : io_svc_(io_svc), suspend_timer_(io_svc_) {
      // We use add_service() very deliberately. This will throw
      // service_already_exists if you pass the same io_service instance to
      // more than one round_robin instance.
      boost::asio::add_service(io_svc_, new service(io_svc_));
      boost::asio::post(io_svc_, [this]() mutable {
        while (!io_svc_.stopped()) {
          if (has_ready_fibers()) {
            // run all pending handlers in round_robin
            while (io_svc_.poll())
              ;
            // block this fiber till all pending (ready) fibers are processed
            // == round_robin::suspend_until() has been called
            std::unique_lock<boost::fibers::mutex> lk(mtx_);
            cnd_.wait(lk);
          } else {
            // run one handler inside io_service
            // if no handler available, block this thread
            if (!io_svc_.run_one()) {
              break;
            }
          }
        }
        //]
      });
    }

    void awakened(context *ctx) noexcept override {
      BOOST_ASSERT(nullptr != ctx);
      BOOST_ASSERT(!ctx->ready_is_linked());
      ctx->ready_link(rqueue_); /*< fiber, enqueue on ready queue >*/
      if (!ctx->is_context(boost::fibers::type::dispatcher_context)) {
        ++counter_;
      }
    }

    context *pick_next() noexcept override {
      context *ctx(nullptr);
      if (!rqueue_.empty()) { /*<
           pop an item from the ready queue
       >*/
        ctx = &rqueue_.front();
        rqueue_.pop_front();
        BOOST_ASSERT(nullptr != ctx);
        BOOST_ASSERT(context::active() != ctx);
        if (!ctx->is_context(boost::fibers::type::dispatcher_context)) {
          --counter_;
        }
      }
      return ctx;
    }

    bool has_ready_fibers() const noexcept override {
      return 0 < counter_;
    }

    void suspend_until(std::chrono::steady_clock::time_point const
                           &abs_time) noexcept override {
      // Set a timer so at least one handler will eventually fire, causing
      // run_one() to eventually return.
      if ((std::chrono::steady_clock::time_point::max)() != abs_time) {
        // Each expires_at(time_point) call cancels any previous pending
        // call. We could inadvertently spin like this:
        // dispatcher calls suspend_until() with earliest wake time
        // suspend_until() sets suspend_timer_
        // lambda loop calls run_one()
        // some other asio handler runs before timer expires
        // run_one() returns to lambda loop
        // lambda loop yields to dispatcher
        // dispatcher finds no ready fibers
        // dispatcher calls suspend_until() with SAME wake time
        // suspend_until() sets suspend_timer_ to same time, canceling
        // previous async_wait()
        // lambda loop calls run_one()
        // asio calls suspend_timer_ handler with operation_aborted
        // run_one() returns to lambda loop... etc. etc.
        // So only actually set the timer when we're passed a DIFFERENT
        // abs_time value.
        suspend_timer_.expires_at(abs_time);
        suspend_timer_.async_wait(
            [](boost::system::error_code const &) { this_fiber::yield(); });
      }
      cnd_.notify_one();
    }

    void notify() noexcept override {
      // Something has happened that should wake one or more fibers BEFORE
      // suspend_timer_ expires. Reset the timer to cause it to fire
      // immediately, causing the run_one() call to return. In theory we
      // could use cancel() because we don't care whether suspend_timer_'s
      // handler is called with operation_aborted or success. However --
      // cancel() doesn't change the expiration time, and we use
      // suspend_timer_'s expiration time to decide whether it's already
      // set. If suspend_until() set some specific wake time, then notify()
      // canceled it, then suspend_until() was called again with the same
      // wake time, it would match suspend_timer_'s expiration time and we'd
      // refrain from setting the timer. So instead of simply calling
      // cancel(), reset the timer, which cancels the pending sleep AND sets
      // a new expiration time. This will cause us to spin the loop twice --
      // once for the operation_aborted handler, once for timer expiration
      // -- but that shouldn't be a big problem.
      suspend_timer_.async_wait(
          [](boost::system::error_code const &) { this_fiber::yield(); });
      suspend_timer_.expires_at(std::chrono::steady_clock::now());
    }
  };

  boost::asio::io_context::id round_robin::service::id;

}  // namespace boost::fibers::asio

#endif  // BOOST_FIBERS_ASIO_ROUND_ROBIN_H
