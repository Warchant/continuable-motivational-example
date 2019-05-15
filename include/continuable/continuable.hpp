/// This is an automatic generated amalgamation of:
/// continuable version 3.0.0 (d30814c2ff001)

/*

                        /~` _  _ _|_. _     _ |_ | _
                        \_,(_)| | | || ||_|(_||_)|(/_

                    https://github.com/Naios/continuable
                                   v3.0.0

  Copyright(c) 2015 - 2018 Denis Blank <denis.blank at outlook dot com>

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files(the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and / or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions :

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
**/

#ifndef CONTINUABLE_HPP_INCLUDED
#define CONTINUABLE_HPP_INCLUDED

/// Declares the continuable library namespace.
///
/// The most important class is cti::continuable_base, that provides the
/// whole functionality for continuation chaining.
///
/// The class cti::continuable_base is created through the
/// cti::make_continuable() function which accepts a callback taking function.
///
/// Also there are following support functions available:
/// - cti::when_all() - connects cti::continuable_base's to an `all` connection.
/// - cti::when_any() - connects cti::continuable_base's to an `any` connection.
/// - cti::when_seq() - connects cti::continuable_base's to a sequence.
namespace cti {}

// #include <continuable/continuable-base.hpp>

/*

                        /~` _  _ _|_. _     _ |_ | _
                        \_,(_)| | | || ||_|(_||_)|(/_

                    https://github.com/Naios/continuable
                                   v3.0.0

  Copyright(c) 2015 - 2018 Denis Blank <denis.blank at outlook dot com>

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files(the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and / or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions :

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
**/

#ifndef CONTINUABLE_BASE_HPP_INCLUDED
#define CONTINUABLE_BASE_HPP_INCLUDED

#include <cassert>
#include <cstdint>
#include <type_traits>
#include <utility>

// #include <continuable/detail/base.hpp>

/*

                        /~` _  _ _|_. _     _ |_ | _
                        \_,(_)| | | || ||_|(_||_)|(/_

                    https://github.com/Naios/continuable
                                   v3.0.0

  Copyright(c) 2015 - 2018 Denis Blank <denis.blank at outlook dot com>

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files(the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and / or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions :

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
**/

#ifndef CONTINUABLE_DETAIL_BASE_HPP_INCLUDED
#define CONTINUABLE_DETAIL_BASE_HPP_INCLUDED

#include <tuple>
#include <type_traits>
#include <utility>

// #include <continuable/detail/features.hpp>

/*

                        /~` _  _ _|_. _     _ |_ | _
                        \_,(_)| | | || ||_|(_||_)|(/_

                    https://github.com/Naios/continuable
                                   v3.0.0

  Copyright(c) 2015 - 2018 Denis Blank <denis.blank at outlook dot com>

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files(the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and / or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions :

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
**/

#ifndef CONTINUABLE_DETAIL_FEATURES_HPP_INCLUDED
#define CONTINUABLE_DETAIL_FEATURES_HPP_INCLUDED

// Defines CONTINUABLE_WITH_NO_EXCEPTIONS when exception support is disabled
#ifndef CONTINUABLE_WITH_NO_EXCEPTIONS
#if defined(_MSC_VER)
#if !defined(_HAS_EXCEPTIONS) || (_HAS_EXCEPTIONS == 0)
#define CONTINUABLE_WITH_NO_EXCEPTIONS
#endif
#elif defined(__clang__)
#if !(__EXCEPTIONS && __has_feature(cxx_exceptions))
#define CONTINUABLE_WITH_NO_EXCEPTIONS
#endif
#elif defined(__GNUC__)
#if !__EXCEPTIONS
#define CONTINUABLE_WITH_NO_EXCEPTIONS
#endif
#endif
#endif // CONTINUABLE_WITH_NO_EXCEPTIONS

// clang-format off
// Detect if the whole standard is available
#if (defined(_MSC_VER) && defined(_HAS_CXX17) && _HAS_CXX17) ||                \
    (__cplusplus >= 201703L)
#define CONTINUABLE_HAS_CXX17_CONSTEXPR_IF
#define CONTINUABLE_HAS_CXX17_DISJUNCTION
#define CONTINUABLE_HAS_CXX17_CONJUNCTION
#else
// Generic feature detection based on __has_feature
  #if defined(__has_feature)
    #if !defined(CONTINUABLE_HAS_CXX17_CONSTEXPR_IF) &&                        \
        __has_feature(cxx_if_constexpr)
      #define CONTINUABLE_HAS_CXX17_CONSTEXPR_IF
    #endif
  #endif

  #if !defined(CONTINUABLE_HAS_CXX17_DISJUNCTION) &&                           \
      defined(__cpp_lib_experimental_logical_traits) &&                        \
      (__cpp_lib_experimental_logical_traits >= 201511)
    #define CONTINUABLE_HAS_CXX17_DISJUNCTION
  #endif

  #if !defined(CONTINUABLE_HAS_CXX17_CONJUNCTION) &&                           \
      defined(__cpp_lib_experimental_logical_traits) &&                        \
      (__cpp_lib_experimental_logical_traits >= 201511)
    #define CONTINUABLE_HAS_CXX17_CONJUNCTION
  #endif
#endif

/// Usually this is enabled by the CMake project
#if !defined(CONTINUABLE_HAS_EXPERIMENTAL_COROUTINE) &&                        \
    defined(__cpp_coroutines) && (__cpp_coroutines >= 201707)
#define CONTINUABLE_HAS_EXPERIMENTAL_COROUTINE
#endif

/// Define CONTINUABLE_HAS_EXPERIMENTAL_COROUTINE when
/// CONTINUABLE_WITH_EXPERIMENTAL_COROUTINE is defined.
#if !defined(CONTINUABLE_HAS_EXPERIMENTAL_COROUTINE) &&                            \
    defined(CONTINUABLE_WITH_EXPERIMENTAL_COROUTINE)
#define CONTINUABLE_HAS_EXPERIMENTAL_COROUTINE
#endif

/// Define CONTINUABLE_HAS_EXCEPTIONS when exceptions are used
#if !defined(CONTINUABLE_WITH_CUSTOM_ERROR_TYPE) &&                            \
    !defined(CONTINUABLE_WITH_NO_EXCEPTIONS)
#define CONTINUABLE_HAS_EXCEPTIONS 1
#else
#undef CONTINUABLE_HAS_EXCEPTIONS
#endif
// clang-format on

#endif // CONTINUABLE_DETAIL_FEATURES_HPP_INCLUDED

// #include <continuable/detail/hints.hpp>

/*

                        /~` _  _ _|_. _     _ |_ | _
                        \_,(_)| | | || ||_|(_||_)|(/_

                    https://github.com/Naios/continuable
                                   v3.0.0

  Copyright(c) 2015 - 2018 Denis Blank <denis.blank at outlook dot com>

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files(the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and / or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions :

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
**/

#ifndef CONTINUABLE_DETAIL_HINTS_HPP_INCLUDED
#define CONTINUABLE_DETAIL_HINTS_HPP_INCLUDED

#include <type_traits>

// #include <continuable/detail/traits.hpp>

/*

                        /~` _  _ _|_. _     _ |_ | _
                        \_,(_)| | | || ||_|(_||_)|(/_

                    https://github.com/Naios/continuable
                                   v3.0.0

  Copyright(c) 2015 - 2018 Denis Blank <denis.blank at outlook dot com>

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files(the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and / or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions :

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
**/

#ifndef CONTINUABLE_DETAIL_TRAITS_HPP_INCLUDED
#define CONTINUABLE_DETAIL_TRAITS_HPP_INCLUDED

#include <cstdint>
#include <initializer_list>
#include <tuple>
#include <type_traits>
#include <utility>

// #include <continuable/detail/features.hpp>


namespace cti {
namespace detail {
namespace traits {
/// Evaluates to the element at position I.
template <std::size_t I, typename... Args>
using at_t = decltype(std::get<I>(std::declval<std::tuple<Args...>>()));

namespace detail {
template <typename T, typename... Args>
struct index_of_impl;
template <typename T, typename... Args>
struct index_of_impl<T, T, Args...> : std::integral_constant<std::size_t, 0U> {
};
template <typename T, typename U, typename... Args>
struct index_of_impl<T, U, Args...>
    : std::integral_constant<std::size_t,
                             1 + index_of_impl<T, Args...>::value> {};
} // namespace detail

/// Evaluates to the index of T in the given pack
template <typename T, typename... Args>
using index_of_t = detail::index_of_impl<T, Args...>;

/// A tagging type for wrapping other types
template <typename... T>
struct identity {};
template <typename T>
struct identity<T> : std::common_type<T> {};

template <typename>
struct is_identity : std::false_type {};
template <typename... Args>
struct is_identity<identity<Args...>> : std::true_type {};

template <typename T>
constexpr identity<std::decay_t<T>> identity_of(T const& /*type*/) noexcept {
  return {};
}
template <typename... Args>
constexpr identity<Args...> identity_of(identity<Args...> /*type*/) noexcept {
  return {};
}
template <typename T>
using identify = std::conditional_t<is_identity<std::decay_t<T>>::value, T,
                                    identity<std::decay_t<T>>>;

template <std::size_t I, typename... T>
constexpr auto get(identity<T...>) noexcept {
  return identify<at_t<I, T...>>{};
}

namespace detail {
// Equivalent to C++17's std::void_t which targets a bug in GCC,
// that prevents correct SFINAE behavior.
// See http://stackoverflow.com/questions/35753920 for details.
template <typename...>
struct deduce_to_void : std::common_type<void> {};
} // namespace detail

/// C++17 like void_t type
template <typename... T>
using void_t = typename detail::deduce_to_void<T...>::type;

namespace detail {
template <typename Type, typename TrueCallback>
constexpr void static_if_impl(std::true_type, Type&& type,
                              TrueCallback&& trueCallback) {
  std::forward<TrueCallback>(trueCallback)(std::forward<Type>(type));
}

template <typename Type, typename TrueCallback>
constexpr void static_if_impl(std::false_type, Type&& /*type*/,
                              TrueCallback&& /*trueCallback*/) {
}

template <typename Type, typename TrueCallback, typename FalseCallback>
constexpr auto static_if_impl(std::true_type, Type&& type,
                              TrueCallback&& trueCallback,
                              FalseCallback&& /*falseCallback*/) {
  return std::forward<TrueCallback>(trueCallback)(std::forward<Type>(type));
}

template <typename Type, typename TrueCallback, typename FalseCallback>
constexpr auto static_if_impl(std::false_type, Type&& type,
                              TrueCallback&& /*trueCallback*/,
                              FalseCallback&& falseCallback) {
  return std::forward<FalseCallback>(falseCallback)(std::forward<Type>(type));
}

/// Evaluates to the size of the given tuple like type,
// / if the type has no static size it will be one.
template <typename T, typename Enable = void>
struct tuple_like_size : std::integral_constant<std::size_t, 1U> {};
template <typename T>
struct tuple_like_size<T, void_t<decltype(std::tuple_size<T>::value)>>
    : std::tuple_size<T> {};
} // namespace detail

/// Returns the pack size of the given empty pack
constexpr std::size_t pack_size_of(identity<>) noexcept {
  return 0U;
}
/// Returns the pack size of the given type
template <typename T>
constexpr std::size_t pack_size_of(identity<T>) noexcept {
  return detail::tuple_like_size<T>::value;
}
/// Returns the pack size of the given type
template <typename First, typename Second, typename... Args>
constexpr std::size_t pack_size_of(identity<First, Second, Args...>) noexcept {
  return 2U + sizeof...(Args);
}

/// Returns an index sequence of the given type
template <typename T>
constexpr auto sequence_of(identity<T>) noexcept {
  constexpr auto const size = pack_size_of(identity<T>{});
  return std::make_index_sequence<size>();
}

/// Invokes the callback only if the given type matches the check
template <typename Type, typename Check, typename TrueCallback>
constexpr void static_if(Type&& type, Check&& check,
                         TrueCallback&& trueCallback) {
  detail::static_if_impl(std::forward<Check>(check)(type),
                         std::forward<Type>(type),
                         std::forward<TrueCallback>(trueCallback));
}

/// Invokes the callback only if the given type matches the check
template <typename Type, typename Check, typename TrueCallback,
    typename FalseCallback>
constexpr auto static_if(Type&& type, Check&& check,
                         TrueCallback&& trueCallback,
                         FalseCallback&& falseCallback) {
  return detail::static_if_impl(std::forward<Check>(check)(type),
                                std::forward<Type>(type),
                                std::forward<TrueCallback>(trueCallback),
                                std::forward<FalseCallback>(falseCallback));
}

/// Calls the given unpacker with the content of the given sequence
template <typename U, std::size_t... I>
constexpr decltype(auto) unpack(std::integer_sequence<std::size_t, I...>,
                                U&& unpacker) {
  return std::forward<U>(unpacker)(std::integral_constant<std::size_t, I>{}...);
}

/// Calls the given unpacker with the content of the given sequenceable
template <typename F, typename U, std::size_t... I>
constexpr auto unpack(F&& first_sequenceable, U&& unpacker,
                      std::integer_sequence<std::size_t, I...>)
-> decltype(std::forward<U>(unpacker)(
    get<I>(std::forward<F>(first_sequenceable))...)) {
  (void)first_sequenceable;
  return std::forward<U>(unpacker)(
      get<I>(std::forward<F>(first_sequenceable))...);
}
/// Calls the given unpacker with the content of the given sequenceable
template <typename F, typename S, typename U, std::size_t... If,
    std::size_t... Is>
constexpr auto unpack(F&& first_sequenceable, S&& second_sequenceable,
                      U&& unpacker, std::integer_sequence<std::size_t, If...>,
                      std::integer_sequence<std::size_t, Is...>)
-> decltype(std::forward<U>(unpacker)(
    get<If>(std::forward<F>(first_sequenceable))...,
    get<Is>(std::forward<S>(second_sequenceable))...)) {
  (void)first_sequenceable;
  (void)second_sequenceable;
  return std::forward<U>(unpacker)(
      get<If>(std::forward<F>(first_sequenceable))...,
      get<Is>(std::forward<S>(second_sequenceable))...);
}
/// Calls the given unpacker with the content of the given sequenceable
template <typename F, typename U>
constexpr auto unpack(F&& first_sequenceable, U&& unpacker)
-> decltype(unpack(std::forward<F>(first_sequenceable),
                   std::forward<U>(unpacker),
                   sequence_of(identify<decltype(first_sequenceable)>{}))) {
  return unpack(std::forward<F>(first_sequenceable), std::forward<U>(unpacker),
                sequence_of(identify<decltype(first_sequenceable)>{}));
}
/// Calls the given unpacker with the content of the given sequenceables
template <typename F, typename S, typename U>
constexpr auto unpack(F&& first_sequenceable, S&& second_sequenceable,
                      U&& unpacker)
-> decltype(unpack(std::forward<F>(first_sequenceable),
                   std::forward<S>(second_sequenceable),
                   std::forward<U>(unpacker),
                   sequence_of(identity_of(first_sequenceable)),
                   sequence_of(identity_of(second_sequenceable)))) {
  return unpack(std::forward<F>(first_sequenceable),
                std::forward<S>(second_sequenceable), std::forward<U>(unpacker),
                sequence_of(identity_of(first_sequenceable)),
                sequence_of(identity_of(second_sequenceable)));
}

/// Adds the given type at the back of the left sequenceable
template <typename Left, typename Element>
constexpr auto push(Left&& left, Element&& element) {
  return unpack(std::forward<Left>(left), [&](auto&&... args) {
    return std::make_tuple(std::forward<decltype(args)>(args)...,
                           std::forward<Element>(element));
  });
}

/// Adds the element to the back of the identity
template <typename... Args, typename Element>
constexpr auto push(identity<Args...>, identity<Element>) noexcept {
  return identity<Args..., Element>{};
}

/// Removes the first element from the identity
template <typename First, typename... Rest>
constexpr auto pop_first(identity<First, Rest...>) noexcept {
  return identity<Rest...>{};
}

/// Returns the merged sequence
template <typename Left>
constexpr auto merge(Left&& left) {
  return std::forward<Left>(left);
}
/// Merges the left sequenceable with the right ones
template <typename Left, typename Right, typename... Rest>
constexpr auto merge(Left&& left, Right&& right, Rest&&... rest) {
  // Merge the left with the right sequenceable and
  // merge the result with the rest.
  return merge(unpack(std::forward<Left>(left), std::forward<Right>(right),
                      [&](auto&&... args) {
                        // Maybe use: template <template<typename...> class T,
                        // typename... Args>
                        return std::make_tuple(
                            std::forward<decltype(args)>(args)...);
                      }),
               std::forward<Rest>(rest)...);
}
/// Merges the left identity with the right ones
template <typename... LeftArgs, typename... RightArgs, typename... Rest>
constexpr auto merge(identity<LeftArgs...> /*left*/,
                     identity<RightArgs...> /*right*/, Rest&&... rest) {
  return merge(identity<LeftArgs..., RightArgs...>{},
               std::forward<Rest>(rest)...);
}

namespace detail {
template <typename T, typename Args, typename = traits::void_t<>>
struct is_invokable_impl : std::common_type<std::false_type> {};

template <typename T, typename... Args>
struct is_invokable_impl<
    T, std::tuple<Args...>,
    void_t<decltype(std::declval<T>()(std::declval<Args>()...))>>
    : std::common_type<std::true_type> {};
} // namespace detail

/// Deduces to a std::true_type if the given type is callable with the arguments
/// inside the given tuple.
/// The main reason for implementing it with the detection idiom instead of
/// hana like detection is that MSVC has issues with capturing raw template
/// arguments inside lambda closures.
///
/// ```cpp
/// traits::is_invokable<object, std::tuple<Args...>>
/// ```
template <typename T, typename Args>
using is_invokable_from_tuple =
typename detail::is_invokable_impl<T, Args>::type;

// Checks whether the given callable object is invocable with the given
// arguments. This doesn't take member functions into account!
template <typename T, typename... Args>
using is_invocable = is_invokable_from_tuple<T, std::tuple<Args...>>;

/// Deduces to a std::false_type
template <typename T>
using fail = std::integral_constant<bool, !std::is_same<T, T>::value>;

#ifdef CONTINUABLE_HAS_CXX17_DISJUNCTION
using std::disjunction;
#else
namespace detail {
/// Declares a C++14 polyfill for C++17 std::disjunction.
template <typename Args, typename = void_t<>>
struct disjunction_impl : std::common_type<std::true_type> {};
template <typename... Args>
struct disjunction_impl<identity<Args...>,
                        void_t<std::enable_if_t<!bool(Args::value)>...>>
    : std::common_type<std::false_type> {};
} // namespace detail

template <typename... Args>
using disjunction = typename detail::disjunction_impl<identity<Args...>>::type;
#endif // CONTINUABLE_HAS_CXX17_DISJUNCTION

#ifdef CONTINUABLE_HAS_CXX17_CONJUNCTION
using std::conjunction;
#else
namespace detail {
/// Declares a C++14 polyfill for C++17 std::conjunction.
template <typename Args, typename = void_t<>>
struct conjunction_impl : std::common_type<std::false_type> {};
template <typename... Args>
struct conjunction_impl<identity<Args...>,
                        void_t<std::enable_if_t<bool(Args::value)>...>>
    : std::common_type<std::true_type> {};
} // namespace detail

template <typename... Args>
using conjunction = typename detail::conjunction_impl<identity<Args...>>::type;
#endif // CONTINUABLE_HAS_CXX17_CONJUNCTION

} // namespace traits
} // namespace detail
} // namespace cti

#endif // CONTINUABLE_DETAIL_TRAITS_HPP_INCLUDED

// #include <continuable/detail/types.hpp>

/*

                        /~` _  _ _|_. _     _ |_ | _
                        \_,(_)| | | || ||_|(_||_)|(/_

                    https://github.com/Naios/continuable
                                   v3.0.0

  Copyright(c) 2015 - 2018 Denis Blank <denis.blank at outlook dot com>

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files(the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and / or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions :

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
**/

#ifndef CONTINUABLE_DETAIL_TYPES_HPP_INCLUDED
#define CONTINUABLE_DETAIL_TYPES_HPP_INCLUDED

#include <utility>

// #include <continuable/detail/features.hpp>


#ifndef CONTINUABLE_WITH_CUSTOM_ERROR_TYPE
#ifndef CONTINUABLE_WITH_NO_EXCEPTIONS
#include <exception>
#else // CONTINUABLE_WITH_NO_EXCEPTIONS
#include <system_error>
#endif // CONTINUABLE_WITH_NO_EXCEPTIONS
#endif // CONTINUABLE_WITH_CUSTOM_ERROR_TYPE

namespace cti {
template <typename Data, typename Annotation>
class continuable_base;

namespace detail {
/// Contains types used globally across the library
namespace types {
#ifdef CONTINUABLE_WITH_CUSTOM_ERROR_TYPE
using error_type = CONTINUABLE_WITH_CUSTOM_ERROR_TYPE;
#else // CONTINUABLE_WITH_CUSTOM_ERROR_TYPE
#ifndef CONTINUABLE_WITH_NO_EXCEPTIONS
/// Represents the error type when exceptions are enabled
using error_type = std::exception_ptr;
#else  // CONTINUABLE_WITH_NO_EXCEPTIONS
/// Represents the error type when exceptions are disabled
using error_type = std::error_condition;
#endif // CONTINUABLE_WITH_NO_EXCEPTIONS
#endif // CONTINUABLE_WITH_CUSTOM_ERROR_TYPE

/// A tag which is used to execute the continuation inside the current thread
struct this_thread_executor_tag {};
/// A tag which is used to continue with an error
struct dispatch_error_tag {};

/// Marks a given callable object as transformation
template <typename T>
class transform : T {
 public:
  explicit transform(T callable) : T(std::move(callable)) {
  }

  using T::operator();
};

} // namespace types
} // namespace detail
} // namespace cti

#endif // CONTINUABLE_DETAIL_TYPES_HPP_INCLUDED


namespace cti {
namespace detail {
namespace hints {
/// Represents a present signature hint
template <typename... Args>
using signature_hint_tag = traits::identity<Args...>;

/// Returns the signature hint of the given continuable
template <typename Data, typename... Args>
constexpr auto
hint_of(traits::identity<continuable_base<Data, signature_hint_tag<Args...>>>) {
  return hints::signature_hint_tag<Args...>{};
}

/// Extracts the signature we pass to the internal continuable
/// from an argument pack as specified by make_continuable.
///
/// This is the overload taking an arbitrary amount of args
template <typename... HintArgs>
constexpr auto extract(traits::identity<HintArgs...> hint) {
  return hint;
}
/// \copybrief extract
///
/// This is the overload taking a void arg.
constexpr auto extract(traits::identity<void> /*hint*/) {
  return traits::identity<>{};
}
} // namespace hints
} // namespace detail
} // namespace cti

#endif // CONTINUABLE_DETAIL_HINTS_HPP_INCLUDED

// #include <continuable/detail/traits.hpp>

// #include <continuable/detail/types.hpp>

// #include <continuable/detail/util.hpp>

/*

                        /~` _  _ _|_. _     _ |_ | _
                        \_,(_)| | | || ||_|(_||_)|(/_

                    https://github.com/Naios/continuable
                                   v3.0.0

  Copyright(c) 2015 - 2018 Denis Blank <denis.blank at outlook dot com>

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files(the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and / or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions :

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
**/

#ifndef CONTINUABLE_DETAIL_UTIL_HPP_INCLUDED
#define CONTINUABLE_DETAIL_UTIL_HPP_INCLUDED

#include <cassert>
#include <tuple>
#include <type_traits>
#include <utility>

// #include <continuable/detail/features.hpp>

// #include <continuable/detail/traits.hpp>


namespace cti {
namespace detail {
/// Utility namespace which provides useful meta-programming support
namespace util {
/// Helper to trick compilers about that a parameter pack is used
template <typename... T>
constexpr void unused(T&&...) noexcept {
}

namespace detail {
/// Forwards every element in the tuple except the last one
template <typename T>
auto forward_except_last(T&& sequenceable) {
  constexpr auto const size = pack_size_of(traits::identify<T>()) - 1U;
  constexpr auto const sequence = std::make_index_sequence<size>();

  return traits::unpack(std::forward<T>(sequenceable),
                        [](auto&&... args) {
                          return std::forward_as_tuple(
                              std::forward<decltype(args)>(args)...);
                        },
                        sequence);
}

/// We are able to call the callable with the arguments given in the tuple
template <typename T, typename... Args>
auto partial_invoke_impl(std::true_type, T&& callable,
                         std::tuple<Args...> args) {
  return traits::unpack(std::move(args), [&](auto&&... arg) {
    return std::forward<T>(callable)(std::forward<decltype(arg)>(arg)...);
  });
}

/// We were unable to call the callable with the arguments in the tuple.
/// Remove the last argument from the tuple and try it again.
template <typename T, typename... Args>
auto partial_invoke_impl(std::false_type, T&& callable,
                         std::tuple<Args...> args) {

  // If you are encountering this assertion you tried to attach a callback
  // which can't accept the arguments of the continuation.
  //
  // ```cpp
  // continuable<int, int> c;
  // std::move(c).then([](std::vector<int> v) { /*...*/ })
  // ```
  static_assert(
      sizeof...(Args) > 0,
      "There is no way to call the given object with these arguments!");

  // Remove the last argument from the tuple
  auto next = forward_except_last(std::move(args));

  // Test whether we are able to call the function with the given tuple
  traits::is_invokable_from_tuple<decltype(callable), decltype(next)>
      is_invokable;

  return partial_invoke_impl(is_invokable, std::forward<T>(callable),
                             std::move(next));
}

/// Shortcut - we can call the callable directly
template <typename T, typename... Args>
auto partial_invoke_impl_shortcut(std::true_type, T&& callable,
                                  Args&&... args) {
  return std::forward<T>(callable)(std::forward<Args>(args)...);
}

/// Failed shortcut - we were unable to invoke the callable with the
/// original arguments.
template <typename T, typename... Args>
auto partial_invoke_impl_shortcut(std::false_type failed, T&& callable,
                                  Args&&... args) {

  // Our shortcut failed, convert the arguments into a forwarding tuple
  return partial_invoke_impl(
      failed, std::forward<T>(callable),
      std::forward_as_tuple(std::forward<Args>(args)...));
}
} // namespace detail

/// Partially invokes the given callable with the given arguments.
///
/// \note This function will assert statically if there is no way to call the
///       given object with less arguments.
template <typename T, typename... Args>
/*keep this inline*/ inline auto partial_invoke(T&& callable, Args&&... args) {
  // Test whether we are able to call the function with the given arguments.
  traits::is_invokable_from_tuple<decltype(callable), std::tuple<Args...>>
      is_invokable;

  // The implementation is done in a shortcut way so there are less
  // type instantiations needed to call the callable with its full signature.
  return detail::partial_invoke_impl_shortcut(
      is_invokable, std::forward<T>(callable), std::forward<Args>(args)...);
}

/// Invokes the given callable object with the given arguments
template <typename Callable, typename... Args>
constexpr auto invoke(Callable&& callable, Args&&... args) noexcept(
noexcept(std::forward<Callable>(callable)(std::forward<Args>(args)...)))
-> decltype(std::forward<Callable>(callable)(std::forward<Args>(args)...)) {

  return std::forward<Callable>(callable)(std::forward<Args>(args)...);
}
/// Invokes the given member function pointer by reference
template <typename T, typename Type, typename Self, typename... Args>
constexpr auto invoke(Type T::*member, Self&& self, Args&&... args) noexcept(
noexcept((std::forward<Self>(self).*member)(std::forward<Args>(args)...)))
-> decltype((std::forward<Self>(self).*
    member)(std::forward<Args>(args)...)) {
  return (std::forward<Self>(self).*member)(std::forward<Args>(args)...);
}
/// Invokes the given member function pointer by pointer
template <typename T, typename Type, typename Self, typename... Args>
constexpr auto invoke(Type T::*member, Self&& self, Args&&... args) noexcept(
noexcept((std::forward<Self>(self)->*member)(std::forward<Args>(args)...)))
-> decltype(
(std::forward<Self>(self)->*member)(std::forward<Args>(args)...)) {
  return (std::forward<Self>(self)->*member)(std::forward<Args>(args)...);
}

// Class for making child classes non copyable
struct non_copyable {
  constexpr non_copyable() = default;
  non_copyable(non_copyable const&) = delete;
  constexpr non_copyable(non_copyable&&) = default;
  non_copyable& operator=(non_copyable const&) = delete;
  non_copyable& operator=(non_copyable&&) = default;
};

// Class for making child classes non copyable and movable
struct non_movable {
  constexpr non_movable() = default;
  non_movable(non_movable const&) = delete;
  constexpr non_movable(non_movable&&) = delete;
  non_movable& operator=(non_movable const&) = delete;
  non_movable& operator=(non_movable&&) = delete;
};

/// This class is responsible for holding an abstract copy- and
/// move-able ownership that is invalidated when the object
/// is moved to another instance.
class ownership {
  explicit constexpr ownership(bool acquired, bool frozen)
      : acquired_(acquired), frozen_(frozen) {
  }

 public:
  constexpr ownership() : acquired_(true), frozen_(false) {
  }
  constexpr ownership(ownership const&) = default;
  ownership(ownership&& right) noexcept
      : acquired_(right.consume()), frozen_(right.is_frozen()) {
  }
  ownership& operator=(ownership const&) = default;
  ownership& operator=(ownership&& right) noexcept {
    acquired_ = right.consume();
    frozen_ = right.is_frozen();
    return *this;
  }

  // Merges both ownerships together
  ownership operator|(ownership const& right) const noexcept {
    return ownership(is_acquired() && right.is_acquired(),
                     is_frozen() || right.is_frozen());
  }

  constexpr bool is_acquired() const noexcept {
    return acquired_;
  }
  constexpr bool is_frozen() const noexcept {
    return frozen_;
  }

  void release() noexcept {
    assert(is_acquired() && "Tried to release the ownership twice!");
    acquired_ = false;
  }
  void freeze(bool enabled = true) noexcept {
    assert(is_acquired() && "Tried to freeze a released object!");
    frozen_ = enabled;
  }

 private:
  bool consume() noexcept {
    if (is_acquired()) {
      release();
      return true;
    }
    return false;
  }

  /// Is true when the object is in a valid state
  bool acquired_ : 1;
  /// Is true when the automatic invocation on destruction is disabled
  bool frozen_ : 1;
};

/// Hint for the compiler that this point should be unreachable
[[noreturn]] inline void unreachable() {
#if defined(_MSC_VER)
  __assume(false);
#elif defined(__GNUC__)
  __builtin_unreachable();
#elif defined(__has_builtin) && __has_builtin(__builtin_unreachable)
  __builtin_unreachable();
#endif
}

/// Causes the application to exit abnormally because we are
/// in an invalid state.
[[noreturn]] inline void trap() {
#if defined(_MSC_VER)
  __debugbreak();
#elif defined(__GNUC__)
  __builtin_trap();
#elif defined(__has_builtin) && __has_builtin(__builtin_trap)
  __builtin_trap();
#else
  *(volatile int*)0 = 0;
#endif
}
} // namespace util
} // namespace detail
} // namespace cti

#ifdef CONTINUABLE_CONSTEXPR_IF
#define CONTINUABLE_CONSTEXPR_IF(EXPR, TRUE_BRANCH, FALSE_BRANCH)
#else
#define CONTINUABLE_CONSTEXPR_IF(EXPR, TRUE_BRANCH, FALSE_BRANCH)
#endif // CONTINUABLE_CONSTEXPR_IF

#endif // CONTINUABLE_DETAIL_UTIL_HPP_INCLUDED


#if defined(CONTINUABLE_HAS_EXCEPTIONS)
#include <exception>
#endif // CONTINUABLE_HAS_EXCEPTIONS

namespace cti {
namespace detail {
/// The namespace `base` provides the low level API for working
/// with continuable types.
///
/// Important methods are:
/// - Creating a continuation from a callback taking functional
///   base::attorney::create(auto&& callback)
///     -> base::continuation<auto>
/// - Chaining a continuation together with a callback
///   base::chain_continuation(base::continuation<auto> continuation,
///                            auto&& callback)
///     -> base::continuation<auto>
/// - Finally invoking the continuation chain
///    base::finalize_continuation(base::continuation<auto> continuation)
///     -> void
namespace base {
template <typename T>
struct is_continuable : std::false_type {};
template <typename Data, typename Annotation>
struct is_continuable<continuable_base<Data, Annotation>> : std::true_type {};

/// Helper class to access private methods and members of
/// the continuable_base class.
struct attorney {
  /// Makes a continuation wrapper from the given argument
  template <typename T, typename A>
  static auto create(T&& continuation, A /*hint*/, util::ownership ownership_) {
    return continuable_base<std::decay_t<T>, std::decay_t<A>>(
        std::forward<T>(continuation), ownership_);
  }

  /// Invokes a continuation object in a reference correct way
  template <typename Data, typename Annotation, typename Callback>
  static auto
  invoke_continuation(continuable_base<Data, Annotation>&& continuation,
                      Callback&& callback) noexcept {
    auto materialized = std::move(continuation).materialize();
    materialized.release();
    return materialized.data_(std::forward<Callback>(callback));
  }

  template <typename Data, typename Annotation>
  static auto materialize(continuable_base<Data, Annotation>&& continuation) {
    return std::move(continuation).materialize();
  }

  template <typename Data, typename Annotation>
  static Data&&
  consume_data(continuable_base<Data, Annotation>&& continuation) {
    return std::move(continuation).consume_data();
  }

  template <typename Continuable>
  static util::ownership ownership_of(Continuable&& continuation) noexcept {
    return continuation.ownership_;
  }
};

// Returns the invoker of a callback, the next callback
// and the arguments of the previous continuation.
//
// The return type of the invokerOf function matches a callable of:
//   void(auto&& callback, auto&& next_callback, auto&&... args)
//
// The invoker decorates the result type in the following way
// - void              -> next_callback()
// - ?                 -> next_callback(?)
// - std::pair<?, ?>   -> next_callback(?, ?)
// - std::tuple<?...>  -> next_callback(?...)
//
// When the result is a continuation itself pass the callback to it
// - continuation<?...> -> result(next_callback);
namespace decoration {
/// Helper class wrapping the underlaying unwrapping lambda
/// in order to extend it with a hint method.
template <typename T, typename Hint>
class invoker : public T {
 public:
  constexpr explicit invoker(T invoke) : T(std::move(invoke)) {
  }

  using T::operator();

  /// Returns the underlaying signature hint
  static constexpr Hint hint() noexcept {
    return {};
  }
};

#if defined(CONTINUABLE_HAS_EXCEPTIONS)
#define CONTINUABLE_BLOCK_TRY_BEGIN try {
#define CONTINUABLE_BLOCK_TRY_END                                              \
  }                                                                            \
  catch (...) {                                                                \
    std::forward<decltype(next_callback)>(next_callback)(                      \
        types::dispatch_error_tag{}, std::current_exception());                \
  }

#else // CONTINUABLE_HAS_EXCEPTIONS
#define CONTINUABLE_BLOCK_TRY_BEGIN {
#define CONTINUABLE_BLOCK_TRY_END }
#endif // CONTINUABLE_HAS_EXCEPTIONS

/// Invokes the given callable object with the given arguments while
/// marking the operation as non exceptional.
template <typename T, typename... Args>
constexpr auto invoke_no_except(T&& callable, Args&&... args) noexcept {
  return std::forward<T>(callable)(std::forward<Args>(args)...);
}

template <typename T, typename... Args>
constexpr auto make_invoker(T&& invoke, hints::signature_hint_tag<Args...>) {
  return invoker<std::decay_t<T>, hints::signature_hint_tag<Args...>>(
      std::forward<T>(invoke));
}

/// - continuable<?...> -> result(next_callback);
template <typename Data, typename Annotation>
constexpr auto
invoker_of(traits::identity<continuable_base<Data, Annotation>>) {
  /// Get the hint of the unwrapped returned continuable
  using Type = decltype(attorney::materialize(
      std::declval<continuable_base<Data, Annotation>>()));

  auto constexpr const hint = hints::hint_of(traits::identify<Type>{});

  return make_invoker(
      [](auto&& callback, auto&& next_callback, auto&&... args) {
        CONTINUABLE_BLOCK_TRY_BEGIN
          auto continuation_ =
              util::partial_invoke(std::forward<decltype(callback)>(callback),
                                   std::forward<decltype(args)>(args)...);

          attorney::invoke_continuation(
              std::move(continuation_),
              std::forward<decltype(next_callback)>(next_callback));
        CONTINUABLE_BLOCK_TRY_END
      },
      hint);
}

/// - ? -> next_callback(?)
template <typename T>
constexpr auto invoker_of(traits::identity<T>) {
  return make_invoker(
      [](auto&& callback, auto&& next_callback, auto&&... args) {
        CONTINUABLE_BLOCK_TRY_BEGIN
          auto result =
              util::partial_invoke(std::forward<decltype(callback)>(callback),
                                   std::forward<decltype(args)>(args)...);

          invoke_no_except(std::forward<decltype(next_callback)>(next_callback),
                           std::move(result));
        CONTINUABLE_BLOCK_TRY_END
      },
      traits::identify<T>{});
}

/// - void -> next_callback()
inline auto invoker_of(traits::identity<void>) {
  return make_invoker(
      [](auto&& callback, auto&& next_callback, auto&&... args) {
        CONTINUABLE_BLOCK_TRY_BEGIN
          util::partial_invoke(std::forward<decltype(callback)>(callback),
                               std::forward<decltype(args)>(args)...);
          invoke_no_except(
              std::forward<decltype(next_callback)>(next_callback));
        CONTINUABLE_BLOCK_TRY_END
      },
      traits::identity<>{});
}

/// Returns a sequenced invoker which is able to invoke
/// objects where std::get is applicable.
inline auto sequenced_unpack_invoker() {
  return [](auto&& callback, auto&& next_callback, auto&&... args) {
    CONTINUABLE_BLOCK_TRY_BEGIN
      auto result =
          util::partial_invoke(std::forward<decltype(callback)>(callback),
                               std::forward<decltype(args)>(args)...);

      // Workaround for MSVC not capturing the reference correctly inside
      // the lambda.
      using Next = decltype(next_callback);

      traits::unpack(std::move(result), [&](auto&&... types) {
        /// TODO Add inplace resolution here

        invoke_no_except(std::forward<Next>(next_callback),
                         std::forward<decltype(types)>(types)...);
      });
    CONTINUABLE_BLOCK_TRY_END
  };
} // namespace decoration

// - std::pair<?, ?> -> next_callback(?, ?)
template <typename First, typename Second>
constexpr auto invoker_of(traits::identity<std::pair<First, Second>>) {
  return make_invoker(sequenced_unpack_invoker(),
                      traits::identity<First, Second>{});
}

// - std::tuple<?...>  -> next_callback(?...)
template <typename... Args>
constexpr auto invoker_of(traits::identity<std::tuple<Args...>>) {
  return make_invoker(sequenced_unpack_invoker(), traits::identity<Args...>{});
}

#undef CONTINUABLE_BLOCK_TRY_BEGIN
#undef CONTINUABLE_BLOCK_TRY_END
} // namespace decoration

/// Invoke the callback immediately
template <typename Invoker, typename... Args>
void packed_dispatch(types::this_thread_executor_tag, Invoker&& invoker,
                     Args&&... args) {

  // Invoke the callback with the decorated invoker immediately
  std::forward<Invoker>(invoker)(std::forward<Args>(args)...);
}

/// Invoke the callback through the given executor
template <typename Executor, typename Invoker, typename... Args>
void packed_dispatch(Executor&& executor, Invoker&& invoker, Args&&... args) {

  // Create a worker object which when invoked calls the callback with the
  // the returned arguments.
  auto work = [
      invoker = std::forward<Invoker>(invoker),
      args = std::make_tuple(std::forward<Args>(args)...)
  ]() mutable {
    traits::unpack(std::move(args), [&](auto&&... captured_args) {
      // Just use the packed dispatch method which dispatches the work on
      // the current thread.
      packed_dispatch(types::this_thread_executor_tag{}, std::move(invoker),
                      std::forward<decltype(captured_args)>(captured_args)...);
    });
  };

  // Pass the work callable object to the executor
  std::forward<Executor>(executor)(std::move(work));
}

/// Tells whether we potentially move the chain upwards and handle the result
enum class handle_results {
  no, //< The result is forwarded to the next callable
  yes //< The result is handled by the current callable
};

// Silences a doxygen bug, it tries to map forward to std::forward
/// \cond false
/// Tells whether we handle the error through the callback
enum class handle_errors {
  no,     //< The error is forwarded to the next callable
  plain,  //< The error is the only argument accepted by the callable
  forward //< The error is forwarded to the callable while keeping its tag
};
/// \endcond

namespace callbacks {
namespace proto {
template <handle_results HandleResults, typename Base, typename Hint>
struct result_handler_base;
template <typename Base, typename... Args>
struct result_handler_base<handle_results::no, Base,
                           hints::signature_hint_tag<Args...>> {
  void operator()(Args... args) && {
    // Forward the arguments to the next callback
    std::move(static_cast<Base*>(this)->next_callback_)(std::move(args)...);
  }
};
template <typename Base, typename... Args>
struct result_handler_base<handle_results::yes, Base,
                           hints::signature_hint_tag<Args...>> {
  /// The operator which is called when the result was provided
  void operator()(Args... args) && {
    // In order to retrieve the correct decorator we must know what the
    // result type is.
    auto result = traits::identify<decltype(util::partial_invoke(
        std::move(static_cast<Base*>(this)->callback_), std::move(args)...))>{};

    // Pick the correct invoker that handles decorating of the result
    auto invoker = decoration::invoker_of(result);

    // Invoke the callback
    packed_dispatch(std::move(static_cast<Base*>(this)->executor_),
                    std::move(invoker),
                    std::move(static_cast<Base*>(this)->callback_),
                    std::move(static_cast<Base*>(this)->next_callback_),
                    std::move(args)...);
  }
};

inline auto make_error_invoker(
    std::integral_constant<handle_errors, handle_errors::plain>) noexcept {
  return [](auto&& callback, types::error_type&& error) {
    // Errors are not partial invoked
    // NOLINTNEXTLINE(hicpp-move-const-arg)
    std::forward<decltype(callback)>(callback)(std::move(error));
  };
}
inline auto make_error_invoker(
    std::integral_constant<handle_errors, handle_errors::forward>) noexcept {
  return [](auto&& callback, types::error_type&& error) {
    // Errors are not partial invoked
    std::forward<decltype(callback)>(callback)(
        types::dispatch_error_tag{},
        std::move(error)); // NOLINT(hicpp-move-const-arg)
  };
}

template <handle_errors HandleErrors /* = plain or forward*/, typename Base>
struct error_handler_base {
  void operator()(types::dispatch_error_tag, types::error_type error) && {
    // Just invoke the error handler, cancel the calling hierarchy after
    auto invoker = make_error_invoker(
        std::integral_constant<handle_errors, HandleErrors>{});

    // Invoke the error handler
    packed_dispatch(
        std::move(static_cast<Base*>(this)->executor_), std::move(invoker),
        std::move(static_cast<Base*>(this)->callback_), std::move(error));
  }
};
template <typename Base>
struct error_handler_base<handle_errors::no, Base> {
  /// The operator which is called when an error occurred
  void operator()(types::dispatch_error_tag tag, types::error_type error) && {
    // Forward the error to the next callback
    std::move(static_cast<Base*>(this)->next_callback_)(tag, std::move(error));
  }
};
} // namespace proto

template <typename Hint, handle_results HandleResults,
    handle_errors HandleErrors, typename Callback, typename Executor,
    typename NextCallback>
struct callback_base;

template <typename... Args, handle_results HandleResults,
    handle_errors HandleErrors, typename Callback, typename Executor,
    typename NextCallback>
struct callback_base<hints::signature_hint_tag<Args...>, HandleResults,
                     HandleErrors, Callback, Executor, NextCallback>
    : proto::result_handler_base<
        HandleResults,
        callback_base<hints::signature_hint_tag<Args...>, HandleResults,
                      HandleErrors, Callback, Executor, NextCallback>,
        hints::signature_hint_tag<Args...>>,
      proto::error_handler_base<
          HandleErrors,
          callback_base<hints::signature_hint_tag<Args...>, HandleResults,
                        HandleErrors, Callback, Executor, NextCallback>>,
      util::non_copyable {

  Callback callback_;
  Executor executor_;
  NextCallback next_callback_;

  explicit callback_base(Callback callback, Executor executor,
                         NextCallback next_callback)
      : callback_(std::move(callback)), executor_(std::move(executor)),
        next_callback_(std::move(next_callback)) {
  }

  /// Pull the result handling operator() in
  using proto::result_handler_base<
      HandleResults,
      callback_base<hints::signature_hint_tag<Args...>, HandleResults,
                    HandleErrors, Callback, Executor, NextCallback>,
      hints::signature_hint_tag<Args...>>::operator();

  /// Pull the error handling operator() in
  using proto::error_handler_base<
      HandleErrors,
      callback_base<hints::signature_hint_tag<Args...>, HandleResults,
                    HandleErrors, Callback, Executor, NextCallback>>::
  operator();

  /// Resolves the continuation with the given values
  void set_value(Args... args) {
    std::move (*this)(std::move(args)...);
  }

  /// Resolves the continuation with the given error variable.
  void set_exception(types::error_type error) {
    std::move (*this)(types::dispatch_error_tag{}, std::move(error));
  }
};

template <typename Hint, handle_results HandleResults,
    handle_errors HandleErrors, typename Callback, typename Executor,
    typename NextCallback>
auto make_callback(Callback&& callback, Executor&& executor,
                   NextCallback&& next_callback) {
  return callback_base<Hint, HandleResults, HandleErrors,
                       std::decay_t<Callback>, std::decay_t<Executor>,
                       std::decay_t<NextCallback>>{
      std::forward<Callback>(callback), std::forward<Executor>(executor),
      std::forward<NextCallback>(next_callback)};
}

/// Once this was a workaround for GCC bug:
/// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=64095
struct final_callback : util::non_copyable {
  template <typename... Args>
  void operator()(Args... /*args*/) && {
  }

  void operator()(types::dispatch_error_tag, types::error_type error) && {
    (void)error;
#ifndef CONTINUABLE_WITH_UNHANDLED_EXCEPTIONS
    // There were unhandled errors inside the asynchronous call chain!
    // Define `CONTINUABLE_WITH_UNHANDLED_EXCEPTIONS` in order
    // to ignore unhandled errors!"
    util::trap();
#endif // CONTINUABLE_WITH_UNHANDLED_EXCEPTIONS
  }

  template <typename... Args>
  void set_value(Args... args) {
    std::move (*this)(std::forward<Args>(args)...);
  }

  void set_exception(types::error_type error) {
    // NOLINTNEXTLINE(hicpp-move-const-arg)
    std::move (*this)(types::dispatch_error_tag{}, std::move(error));
  }
};
} // namespace callbacks

/// Returns the next hint when the callback is invoked with the given hint
template <typename T, typename... Args>
constexpr auto
next_hint_of(std::integral_constant<handle_results, handle_results::yes>,
             traits::identity<T> /*callback*/,
             hints::signature_hint_tag<Args...> /*current*/) {
  // Partial Invoke the given callback
  using Result = decltype(
  util::partial_invoke(std::declval<T>(), std::declval<Args>()...));

  // Return the hint of thr given invoker
  return decltype(decoration::invoker_of(traits::identify<Result>{}).hint()){};
}
/// Don't progress the hint when we don't continue
template <typename T, typename... Args>
constexpr auto
next_hint_of(std::integral_constant<handle_results, handle_results::no>,
             traits::identity<T> /*callback*/,
             hints::signature_hint_tag<Args...> current) {
  return current;
}

/// Chains a callback together with a continuation and returns a continuation:
///
/// For example given:
/// - Continuation: continuation<[](auto&& callback) { callback("hi"); }>
/// - Callback: [](std::string) { }
///
/// This function returns a function accepting the next callback in the chain:
/// - Result: continuation<[](auto&& callback) { /*...*/ }>
///
template <handle_results HandleResults, handle_errors HandleErrors,
    typename Continuation, typename Callback, typename Executor>
auto chain_continuation(Continuation&& continuation, Callback&& callback,
                        Executor&& executor) {
  static_assert(is_continuable<std::decay_t<Continuation>>{},
                "Expected a continuation!");

  using Hint = decltype(hints::hint_of(traits::identify<Continuation>()));
  constexpr auto next_hint =
      next_hint_of(std::integral_constant<handle_results, HandleResults>{},
                   traits::identify<decltype(callback)>{}, Hint{});

  // TODO consume only the data here so the freeze isn't needed
  auto ownership_ = attorney::ownership_of(continuation);
  continuation.freeze();

  return attorney::create(
      [
          continuation = std::forward<Continuation>(continuation),
          callback = std::forward<Callback>(callback),
          executor = std::forward<Executor>(executor)
      ](auto&& next_callback) mutable {

        // Invokes a continuation with a given callback.
        // Passes the next callback to the resulting continuable or
        // invokes the next callback directly if possible.
        //
        // For example given:
        // - Continuation: continuation<[](auto&& callback) { callback("hi"); }>
        // - Callback: [](std::string) { }
        // - NextCallback: []() { }
        auto proxy =
            callbacks::make_callback<Hint, HandleResults, HandleErrors>(
                std::move(callback), std::move(executor),
                std::forward<decltype(next_callback)>(next_callback));

        // Invoke the continuation with a proxy callback.
        // The proxy callback is responsible for passing
        // the result to the callback as well as decorating it.
        attorney::invoke_continuation(std::move(continuation),
                                      std::move(proxy));
      },
      next_hint, ownership_);
}

/// Final invokes the given continuation chain:
///
/// For example given:
/// - Continuation: continuation<[](auto&& callback) { callback("hi"); }>
template <typename Continuation>
void finalize_continuation(Continuation&& continuation) {
  attorney::invoke_continuation(std::forward<Continuation>(continuation),
                                callbacks::final_callback{});
}

/// Workaround for GCC bug:
/// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=64095
template <typename T>
class supplier_callback {
  T data_;

 public:
  explicit supplier_callback(T data) : data_(std::move(data)) {
  }

  template <typename... Args>
  auto operator()(Args...) {
    return std::move(data_);
  }
};

/// Returns a continuable into a callable object returning the continuable
template <typename Continuation>
auto wrap_continuation(Continuation&& continuation) {
  continuation.freeze();
  return supplier_callback<std::decay_t<Continuation>>(
      std::forward<Continuation>(continuation));
}
} // namespace base
} // namespace detail
} // namespace cti

#endif // CONTINUABLE_DETAIL_BASE_HPP_INCLUDED

// #include <continuable/detail/connection-all.hpp>

/*

                        /~` _  _ _|_. _     _ |_ | _
                        \_,(_)| | | || ||_|(_||_)|(/_

                    https://github.com/Naios/continuable
                                   v3.0.0

  Copyright(c) 2015 - 2018 Denis Blank <denis.blank at outlook dot com>

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files(the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and / or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions :

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
**/

#ifndef CONTINUABLE_DETAIL_CONNECTION_ALL_HPP_INCLUDED
#define CONTINUABLE_DETAIL_CONNECTION_ALL_HPP_INCLUDED

#include <atomic>
#include <memory>
#include <mutex>
#include <tuple>
#include <type_traits>
#include <utility>

// #include <continuable/detail/base.hpp>

// #include <continuable/detail/connection-aggregated.hpp>

/*

                        /~` _  _ _|_. _     _ |_ | _
                        \_,(_)| | | || ||_|(_||_)|(/_

                    https://github.com/Naios/continuable
                                   v3.0.0

  Copyright(c) 2015 - 2018 Denis Blank <denis.blank at outlook dot com>

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files(the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and / or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions :

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
**/

#ifndef CONTINUABLE_DETAIL_CONNECTION_REMAPPING_HPP_INCLUDED
#define CONTINUABLE_DETAIL_CONNECTION_REMAPPING_HPP_INCLUDED

#include <cassert>
#include <tuple>
#include <type_traits>
#include <utility>

// #include <continuable/continuable-traverse.hpp>

/*

                        /~` _  _ _|_. _     _ |_ | _
                        \_,(_)| | | || ||_|(_||_)|(/_

                    https://github.com/Naios/continuable
                                   v3.0.0

  Copyright(c) 2015 - 2018 Denis Blank <denis.blank at outlook dot com>

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files(the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and / or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions :

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
**/

#ifndef CONTINUABLE_TRAVERSE_HPP_INCLUDED
#define CONTINUABLE_TRAVERSE_HPP_INCLUDED

#include <tuple>
#include <type_traits>
#include <utility>

// #include <continuable/detail/traverse.hpp>

/*

                        /~` _  _ _|_. _     _ |_ | _
                        \_,(_)| | | || ||_|(_||_)|(/_

                    https://github.com/Naios/continuable
                                   v3.0.0

  Copyright(c) 2015 - 2018 Denis Blank <denis.blank at outlook dot com>

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files(the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and / or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions :

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
**/

#ifndef CONTINUABLE_DETAIL_TRAVERSE_HPP_INCLUDED
#define CONTINUABLE_DETAIL_TRAVERSE_HPP_INCLUDED

#include <cstddef>
#include <iterator>
#include <memory>
#include <tuple>
#include <type_traits>
#include <utility>

// #include <continuable/detail/container-category.hpp>

/*

                        /~` _  _ _|_. _     _ |_ | _
                        \_,(_)| | | || ||_|(_||_)|(/_

                    https://github.com/Naios/continuable
                                   v3.0.0

  Copyright(c) 2015 - 2018 Denis Blank <denis.blank at outlook dot com>

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files(the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and / or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions :

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
**/

#ifndef CONTINUABLE_DETAIL_CONTAINER_CATEGORY_HPP_INCLUDED
#define CONTINUABLE_DETAIL_CONTAINER_CATEGORY_HPP_INCLUDED

#include <tuple>
#include <type_traits>

// #include <continuable/detail/traits.hpp>


namespace cti {
namespace detail {
namespace traversal {
/// Deduces to a true type if the given parameter T
/// has a begin() and end() method.
// TODO Find out whether we should use std::begin and std::end instead, which
// could cause issues with plain arrays.
template <typename T, typename = void>
struct is_range : std::false_type {};
template <typename T>
struct is_range<T, traits::void_t<decltype(std::declval<T>().begin() ==
    std::declval<T>().end())>>
    : std::true_type {};

/// Deduces to a true type if the given parameter T
/// is accessible through std::tuple_size.
template <typename T, typename = void>
struct is_tuple_like : std::false_type {};
template <typename T>
struct is_tuple_like<T, traits::void_t<decltype(std::tuple_size<T>::value)>>
    : std::true_type {};

/// A tag for dispatching based on the tuple like
/// or container properties of a type.
///
/// This type deduces to a true_type if it has any category.
template <bool IsContainer, bool IsTupleLike>
struct container_category_tag
    : std::integral_constant<bool, IsContainer || IsTupleLike> {};

/// Deduces to the container_category_tag of the given type T.
template <typename T>
using container_category_of_t =
container_category_tag<is_range<T>::value, is_tuple_like<T>::value>;
} // namespace traversal
} // namespace detail
} // namespace cti

#endif // CONTINUABLE_DETAIL_CONTAINER_CATEGORY_HPP_INCLUDED

// #include <continuable/detail/traits.hpp>


namespace cti {
namespace detail {
namespace traversal {
/// Exposes useful facilities for dealing with 1:n mappings
namespace spreading {
/// \cond false
/// A struct to mark a tuple to be unpacked into the parent context
template <typename... T>
class spread_box {
  std::tuple<T...> boxed_;

 public:
  explicit constexpr spread_box(std::tuple<T...> boxed)
      : boxed_(std::move(boxed)) {
  }

  std::tuple<T...> unbox() {
    return std::move(boxed_);
  }
};
template <>
class spread_box<> {
 public:
  explicit constexpr spread_box() noexcept {
  }
  explicit constexpr spread_box(std::tuple<>) noexcept {
  }

  constexpr std::tuple<> unbox() const noexcept {
    return std::tuple<>{};
  }
};

/// Returns an empty spread box which represents an empty
/// mapped object.
constexpr spread_box<> empty_spread() noexcept {
  return spread_box<>{};
}

/// Deduces to a true_type if the given type is a spread marker
template <typename T>
struct is_spread : std::false_type {};
template <typename... T>
struct is_spread<spread_box<T...>> : std::true_type {};

/// Deduces to a true_type if the given type is an empty
/// spread marker
template <typename T>
struct is_empty_spread : std::false_type {};
template <>
struct is_empty_spread<spread_box<>> : std::true_type {};

/// Converts types to the type and spread_box objects to its
/// underlying tuple.
template <typename T>
constexpr T unpack(T&& type) {
  return std::forward<T>(type);
}
template <typename... T>
constexpr auto unpack(spread_box<T...> type) -> decltype(type.unbox()) {
  return type.unbox();
}

/// Deduces to the type unpack is returning when called with the
/// the given type T.
template <typename T>
using unpacked_of_t = decltype(unpack(std::declval<T>()));

/// Converts types to the type and spread_box objects to its
/// underlying tuple. If the type is mapped to zero elements,
/// the return type will be void.
template <typename T>
constexpr auto unpack_or_void(T&& type)
-> decltype(unpack(std::forward<T>(type))) {
  return unpack(std::forward<T>(type));
}
inline void unpack_or_void(spread_box<>) noexcept {
}

/// Converts types to the a tuple carrying the single type and
/// spread_box objects to its underlying tuple.
template <typename T>
constexpr std::tuple<T> undecorate(T&& type) {
  return std::tuple<T>{std::forward<T>(type)};
}
template <typename... T>
constexpr auto undecorate(spread_box<T...> type) -> decltype(type.unbox()) {
  return type.unbox();
}

/// A callable object which maps its content back to a
/// tuple like type.
template <typename EmptyType, template <typename...> class Type>
struct tupelizer_base {
  // We overload with one argument here so Clang and GCC don't
  // have any issues with overloading against zero arguments.
  template <typename First, typename... T>
  constexpr Type<First, T...> operator()(First&& first, T&&... args) const {
    return Type<First, T...>{std::forward<First>(first),
                             std::forward<T>(args)...};
  }

  // Specifically return the empty object which can be different
  // from a tuple.
  constexpr EmptyType operator()() const noexcept(noexcept(EmptyType{})) {
    return EmptyType{};
  }
};

/// A callable object which maps its content back to a tuple.
template <template <typename...> class Type = std::tuple>
using tupelizer_of_t = tupelizer_base<std::tuple<>, Type>;

/// A callable object which maps its content back to a tuple like
/// type if it wasn't empty. For empty types arguments an empty
/// spread box is returned instead. This is useful to propagate
/// empty mappings back to the caller.
template <template <typename...> class Type = std::tuple>
using flat_tupelizer_of_t = tupelizer_base<spread_box<>, Type>;

/// A callable object which maps its content back to an
/// array like type.
/// This transform can only be used for (flat) mappings which
/// return an empty mapping back to the caller.
template <template <typename, std::size_t> class Type>
struct flat_arraylizer {
  /// Deduces to the array type when the array is instantiated
  /// with the given arguments.
  template <typename First, typename... Rest>
  using array_type_of_t =
  Type<typename std::decay<First>::type, 1 + sizeof...(Rest)>;

  // We overload with one argument here so Clang and GCC don't
  // have any issues with overloading against zero arguments.
  template <typename First, typename... T>
  constexpr auto operator()(First&& first, T&&... args) const
  -> array_type_of_t<First, T...> {
    return array_type_of_t<First, T...>{
        {std::forward<First>(first), std::forward<T>(args)...}};
  }

  constexpr auto operator()() const noexcept -> decltype(empty_spread()) {
    return empty_spread();
  }
};

/// Use the recursive instantiation for a variadic pack which
/// may contain spread types
template <typename C, typename... T>
constexpr auto apply_spread_impl(std::true_type, C&& callable, T&&... args)
-> decltype(
traits::unpack(std::tuple_cat(undecorate(std::forward<T>(args))...),
               std::forward<C>(callable))) {
  return traits::unpack(std::tuple_cat(undecorate(std::forward<T>(args))...),
                        std::forward<C>(callable));
}

/// Use the linear instantiation for variadic packs which don't
/// contain spread types.
template <typename C, typename... T>
constexpr auto apply_spread_impl(std::false_type, C&& callable, T&&... args)
-> decltype(std::forward<C>(callable)(std::forward<T>(args)...)) {
  return std::forward<C>(callable)(std::forward<T>(args)...);
}

/// Deduces to a true_type if any of the given types marks
/// the underlying type to be spread into the current context.
template <typename... T>
using is_any_spread_t = traits::disjunction<is_spread<T>...>;

template <typename C, typename... T>
constexpr auto map_spread(C&& callable, T&&... args)
-> decltype(apply_spread_impl(is_any_spread_t<T...>{},
                              std::forward<C>(callable),
                              std::forward<T>(args)...)) {
  // Check whether any of the args is a detail::flatted_tuple_t,
  // if not, use the linear called version for better
  // compilation speed.
  return apply_spread_impl(is_any_spread_t<T...>{}, std::forward<C>(callable),
                           std::forward<T>(args)...);
}

/// Converts the given variadic arguments into a tuple in a way
/// that spread return values are inserted into the current pack.
template <typename... T>
constexpr auto tupelize(T&&... args)
-> decltype(map_spread(tupelizer_of_t<>{}, std::forward<T>(args)...)) {
  return map_spread(tupelizer_of_t<>{}, std::forward<T>(args)...);
}

/// Converts the given variadic arguments into a tuple in a way
/// that spread return values are inserted into the current pack.
/// If the arguments were mapped to zero arguments, the empty
/// mapping is propagated backwards to the caller.
template <template <typename...> class Type, typename... T>
constexpr auto flat_tupelize_to(T&&... args)
-> decltype(map_spread(flat_tupelizer_of_t<Type>{},
                       std::forward<T>(args)...)) {
  return map_spread(flat_tupelizer_of_t<Type>{}, std::forward<T>(args)...);
}

/// Converts the given variadic arguments into an array in a way
/// that spread return values are inserted into the current pack.
/// Through this the size of the array like type might change.
/// If the arguments were mapped to zero arguments, the empty
/// mapping is propagated backwards to the caller.
template <template <typename, std::size_t> class Type, typename... T>
constexpr auto flat_arraylize_to(T&&... args)
-> decltype(map_spread(flat_arraylizer<Type>{}, std::forward<T>(args)...)) {
  return map_spread(flat_arraylizer<Type>{}, std::forward<T>(args)...);
}

/// Converts an empty tuple to void
template <typename First, typename... Rest>
constexpr std::tuple<First, Rest...>
voidify_empty_tuple(std::tuple<First, Rest...> val) {
  return std::move(val);
}
inline void voidify_empty_tuple(std::tuple<>) noexcept {
}

/// Converts the given variadic arguments into a tuple in a way
/// that spread return values are inserted into the current pack.
///
/// If the returned tuple is empty, voidis returned instead.
template <typename... T>
constexpr decltype(auto) tupelize_or_void(T&&... args) {
  return voidify_empty_tuple(tupelize(std::forward<T>(args)...));
}
/// \endcond
} // namespace spreading

/// Just traverses the pack with the given callable object,
/// no result is returned or preserved.
struct strategy_traverse_tag {};
/// Remaps the variadic pack with the return values from the mapper.
struct strategy_remap_tag {};

/// Deduces to a true type if the type leads to at least one effective
/// call to the mapper.
template <typename Mapper, typename T>
using is_effective_t = traits::is_invocable<typename Mapper::traversor_type, T>;

// TODO find out whether the linear compile-time instantiation is faster:
// template <typename Mapper, typename... T>
// struct is_effective_any_of_t
//     : traits::disjunction<is_effective_t<Mapper, T>...> {};
// template <typename Mapper>
// struct is_effective_any_of_t<Mapper> : std::false_type {};

/// Deduces to a true type if any type leads to at least one effective
/// call to the mapper.
template <typename Mapper, typename... T>
struct is_effective_any_of_t;
template <typename Mapper, typename First, typename... Rest>
struct is_effective_any_of_t<Mapper, First, Rest...>
    : std::conditional<is_effective_t<Mapper, First>::value, std::true_type,
                       is_effective_any_of_t<Mapper, Rest...>>::type {};
template <typename Mapper>
struct is_effective_any_of_t<Mapper> : std::false_type {};

/// Provides utilities for remapping the whole content of a
/// container like type to the same container holding different types.
namespace container_remapping {
/// Deduces to a true type if the given parameter T
/// has a push_back method that accepts a type of E.
template <typename T, typename E, typename = void>
struct has_push_back : std::false_type {};
template <typename T, typename E>
struct has_push_back<
    T, E,
    traits::void_t<decltype(std::declval<T>().push_back(std::declval<E>()))>>
    : std::true_type {};

/// Specialization for a container with a single type T
template <typename NewType, template <class> class Base, typename OldType>
auto rebind_container(Base<OldType> const & /*container*/) -> Base<NewType> {
  return Base<NewType>();
}

/// Specialization for a container with a single type T and
/// a particular allocator,
/// which is preserved across the remap.
/// -> We remap the allocator through std::allocator_traits.
template <
    typename NewType, template <class, class> class Base, typename OldType,
    typename OldAllocator,
    // Check whether the second argument of the container was
    // the used allocator.
    typename std::enable_if<std::uses_allocator<
        Base<OldType, OldAllocator>, OldAllocator>::value>::type* = nullptr,
    typename NewAllocator = typename std::allocator_traits<
        OldAllocator>::template rebind_alloc<NewType>>
auto rebind_container(Base<OldType, OldAllocator> const& container)
-> Base<NewType, NewAllocator> {
  // Create a new version of the allocator, that is capable of
  // allocating the mapped type.
  return Base<NewType, NewAllocator>(NewAllocator(container.get_allocator()));
}

/// Returns the default iterators of the container in case
/// the container was passed as an l-value reference.
/// Otherwise move iterators of the container are returned.
template <typename C, typename = void>
class container_accessor {
  static_assert(std::is_lvalue_reference<C>::value,
                "This should be a lvalue reference here!");

  C container_;

 public:
  container_accessor(C container) : container_(container) {
  }

  auto begin() -> decltype(container_.begin()) {
    return container_.begin();
  }

  auto end() -> decltype(container_.end()) {
    return container_.end();
  }
};
template <typename C>
class container_accessor<
    C, typename std::enable_if<std::is_rvalue_reference<C&&>::value>::type> {
  C&& container_;

 public:
  container_accessor(C&& container) : container_(std::move(container)) {
  }

  auto begin() -> decltype(std::make_move_iterator(container_.begin())) {
    return std::make_move_iterator(container_.begin());
  }

  auto end() -> decltype(std::make_move_iterator(container_.end())) {
    return std::make_move_iterator(container_.end());
  }
};

template <typename T>
container_accessor<T> container_accessor_of(T&& container) {
  // Don't use any decay here
  return container_accessor<T>(std::forward<T>(container));
}

/// Deduces to the type the homogeneous container is containing
///
/// This alias deduces to the same type on which
/// container_accessor<T> is iterating.
///
/// The basic idea is that we deduce to the type the homogeneous
/// container T is carrying as reference while preserving the
/// original reference type of the container:
/// - If the container was passed as l-value its containing
///   values are referenced through l-values.
/// - If the container was passed as r-value its containing
///   values are referenced through r-values.
template <typename Container>
using element_of_t = typename std::conditional<
    std::is_rvalue_reference<Container&&>::value,
    decltype(std::move(*(std::declval<Container>().begin()))),
    decltype(*(std::declval<Container>().begin()))>::type;

/// Removes all qualifier and references from the given type
/// if the type is a l-value or r-value reference.
template <typename T>
using dereferenced_of_t =
typename std::conditional<std::is_reference<T>::value,
                          typename std::decay<T>::type, T>::type;

/// Returns the type which is resulting if the mapping is applied to
/// an element in the container.
///
/// Since standard containers don't allow to be instantiated with
/// references we try to construct the container from a copied
/// version.
template <typename Container, typename Mapping>
using mapped_type_from_t = dereferenced_of_t<spreading::unpacked_of_t<decltype(
                                                                      std::declval<Mapping>()(std::declval<element_of_t<Container>>()))>>;

/// Deduces to a true_type if the mapping maps to zero elements.
template <typename T, typename M>
using is_empty_mapped = spreading::is_empty_spread<typename std::decay<decltype(
                                                                       std::declval<M>()(std::declval<element_of_t<T>>()))>::type>;

/// We are allowed to reuse the container if we map to the same
/// type we are accepting and when we have
/// the full ownership of the container.
template <typename T, typename M>
using can_reuse = std::integral_constant<
    bool, std::is_same<element_of_t<T>, mapped_type_from_t<T, M>>::value &&
        std::is_rvalue_reference<T&&>::value>;

/// Categorizes a mapping of a homogeneous container
///
/// \tparam IsEmptyMapped Identifies whether the mapping maps to
///         to zero arguments.
/// \tparam CanReuse Identifies whether the container can be
///         re-used through the mapping.
template <bool IsEmptyMapped, bool CanReuse>
struct container_mapping_tag {};

/// Categorizes the given container through a container_mapping_tag
template <typename T, typename M>
using container_mapping_tag_of_t =
container_mapping_tag<is_empty_mapped<T, M>::value, can_reuse<T, M>::value>;

/// Deduces to a true type if the given parameter T supports a `reserve` method
template <typename From, typename To, typename = void>
struct is_reservable_from : std::false_type {};
template <typename From, typename To>
struct is_reservable_from<From, To,
                          traits::void_t<decltype(std::declval<To>().reserve(
                              std::declval<From>().size()))>> : std::true_type {
};

template <typename Dest, typename Source>
void reserve_if(std::true_type, Dest&& dest, Source&& source) {
  // Reserve the mapped size
  dest.reserve(source.size());
}
template <typename Dest, typename Source>
void reserve_if(std::false_type, Dest&&, Source&&) noexcept {
  // We do nothing here, since the container doesn't support reserving
}

/// We create a new container, which may hold the resulting type
template <typename M, typename T>
auto remap_container(container_mapping_tag<false, false>, M&& mapper,
                     T&& container)
-> decltype(rebind_container<mapped_type_from_t<T, M>>(container)) {
  static_assert(
      has_push_back<typename std::decay<T>::type, element_of_t<T>>::value,
      "Can only remap containers that provide a push_back "
      "method!");

  // Create the new container, which is capable of holding
  // the remappped types.
  auto remapped = rebind_container<mapped_type_from_t<T, M>>(container);

  // We try to reserve the original size from the source
  // container inside the destination container.
  reserve_if(
      is_reservable_from<std::decay_t<T>, std::decay_t<decltype(remapped)>>{},
      remapped, container);

  // Perform the actual value remapping from the source to
  // the destination.
  // We could have used std::transform for this, however,
  // I didn't want to pull a whole header for it in.
  for (auto&& val : container_accessor_of(std::forward<T>(container))) {
    remapped.push_back(spreading::unpack(
        std::forward<M>(mapper)(std::forward<decltype(val)>(val))));
  }

  return remapped; // RVO
}

/// The remapper optimized for the case that we map to the same
/// type we accepted such as int -> int.
template <typename M, typename T>
auto remap_container(container_mapping_tag<false, true>, M&& mapper,
                     T&& container) -> typename std::decay<T>::type {
  for (auto&& val : container_accessor_of(std::forward<T>(container))) {
    val = spreading::unpack(
        std::forward<M>(mapper)(std::forward<decltype(val)>(val)));
  }
  return std::forward<T>(container);
}

/// Remap the container to zero arguments
template <typename M, typename T>
auto remap_container(container_mapping_tag<true, false>, M&& mapper,
                     T&& container) -> decltype(spreading::empty_spread()) {
  for (auto&& val : container_accessor_of(std::forward<T>(container))) {
    // Don't save the empty mapping for each invocation
    // of the mapper.
    std::forward<M>(mapper)(std::forward<decltype(val)>(val));
  }
  // Return one instance of an empty mapping for the container
  return spreading::empty_spread();
}

/// \cond false
/// Remaps the content of the given container with type T,
/// to a container of the same type which may contain
/// different types.
template <typename T, typename M>
auto remap(
    strategy_remap_tag, T&& container, M&& mapper,
    typename std::enable_if<is_effective_t<M, element_of_t<T>>::value>::type* =
    nullptr) -> decltype(remap_container(container_mapping_tag_of_t<T, M>{},
                                         std::forward<M>(mapper),
                                         std::forward<T>(container))) {
  return remap_container(container_mapping_tag_of_t<T, M>{},
                         std::forward<M>(mapper), std::forward<T>(container));
}
/// \endcond

/// Just call the visitor with the content of the container
template <typename T, typename M>
void remap(
    strategy_traverse_tag, T&& container, M&& mapper,
    typename std::enable_if<is_effective_t<M, element_of_t<T>>::value>::type* =
    nullptr) {
  for (auto&& element : container_accessor_of(std::forward<T>(container))) {
    std::forward<M>(mapper)(std::forward<decltype(element)>(element));
  }
}
} // end namespace container_remapping

/// Provides utilities for remapping the whole content of a
/// tuple like type to the same type holding different types.
namespace tuple_like_remapping {
template <typename Strategy, typename Mapper, typename T,
    typename Enable = void>
struct tuple_like_remapper;

/// Specialization for std::tuple like types which contain
/// an arbitrary amount of heterogenous arguments.
template <typename M, template <typename...> class Base, typename... OldArgs>
struct tuple_like_remapper<strategy_remap_tag, M, Base<OldArgs...>,
    // Support for skipping completely untouched types
                           typename std::enable_if<is_effective_any_of_t<
                               M, OldArgs...>::value>::type> {
  M mapper_;

  template <typename... Args>
  auto operator()(Args&&... args) -> decltype(spreading::flat_tupelize_to<Base>(
      std::declval<M>()(std::forward<Args>(args))...)) {
    return spreading::flat_tupelize_to<Base>(
        mapper_(std::forward<Args>(args))...);
  }
};
template <typename M, template <typename...> class Base, typename... OldArgs>
struct tuple_like_remapper<strategy_traverse_tag, M, Base<OldArgs...>,
    // Support for skipping completely untouched types
                           typename std::enable_if<is_effective_any_of_t<
                               M, OldArgs...>::value>::type> {
  M mapper_;

  template <typename... Args>
  auto operator()(Args&&... args) -> traits::void_t<
      decltype(std::declval<M>()(std::declval<OldArgs>()))...> {
    int dummy[] = {0, ((void)mapper_(std::forward<Args>(args)), 0)...};
    (void)dummy;
  }
};

/// Specialization for std::array like types, which contains a
/// compile-time known amount of homogeneous types.
template <typename M, template <typename, std::size_t> class Base,
    typename OldArg, std::size_t Size>
struct tuple_like_remapper<
    strategy_remap_tag, M, Base<OldArg, Size>,
    // Support for skipping completely untouched types
    typename std::enable_if<is_effective_t<M, OldArg>::value>::type> {
  M mapper_;

  template <typename... Args>
  auto operator()(Args&&... args)
  -> decltype(spreading::flat_arraylize_to<Base>(
      mapper_(std::forward<Args>(args))...)) {
    return spreading::flat_arraylize_to<Base>(
        mapper_(std::forward<Args>(args))...);
  }
};
template <typename M, template <typename, std::size_t> class Base,
    typename OldArg, std::size_t Size>
struct tuple_like_remapper<
    strategy_traverse_tag, M, Base<OldArg, Size>,
    // Support for skipping completely untouched types
    typename std::enable_if<is_effective_t<M, OldArg>::value>::type> {
  M mapper_;

  template <typename... Args>
  auto operator()(Args&&... args)
  -> decltype((std::declval<M>()(std::declval<OldArg>()))()) {
    int dummy[] = {0, ((void)mapper_(std::forward<Args>(args)), 0)...};
    (void)dummy;
  }
};

/// Remaps the content of the given tuple like type T,
/// to a container of the same type which may contain
/// different types.
template <typename Strategy, typename T, typename M>
auto remap(Strategy, T&& container, M&& mapper) -> decltype(traits::unpack(
    std::forward<T>(container),
    std::declval<tuple_like_remapper<Strategy, typename std::decay<M>::type,
                                     typename std::decay<T>::type>>())) {
  return traits::unpack(
      std::forward<T>(container),
      tuple_like_remapper<Strategy, typename std::decay<M>::type,
                          typename std::decay<T>::type>{
          std::forward<M>(mapper)});
}
} // end namespace tuple_like_remapping

/// Base class for making strategy dependent behaviour available
/// to the mapping_helper class.
template <typename Strategy>
struct mapping_strategy_base {
  template <typename T>
  auto may_void(T&& element) const -> typename std::decay<T>::type {
    return std::forward<T>(element);
  }
};
template <>
struct mapping_strategy_base<strategy_traverse_tag> {
  template <typename T>
  void may_void(T&& /*element*/) const noexcept {
  }
};

/// A helper class which applies the mapping or
/// routes the element through
template <typename Strategy, typename M>
class mapping_helper : protected mapping_strategy_base<Strategy> {
  M mapper_;

  class traversal_callable_base {
    mapping_helper* helper_;

   public:
    explicit traversal_callable_base(mapping_helper* helper) : helper_(helper) {
    }

   protected:
    mapping_helper* get_helper() noexcept {
      return helper_;
    }
  };

  /// A callable object which forwards its invocations
  /// to mapping_helper::traverse.
  class traversor : public traversal_callable_base {
   public:
    using traversal_callable_base::traversal_callable_base;

    /// SFINAE helper
    template <typename T>
    auto operator()(T&& element)
    -> decltype(std::declval<traversor>().get_helper()->traverse(
        Strategy{}, std::forward<T>(element)));

    /// An alias to this type
    using traversor_type = traversor;
  };

  /// A callable object which forwards its invocations
  /// to mapping_helper::try_traverse.
  ///
  /// This callable object will accept any input,
  /// since elements passed to it are passed through,
  /// if the provided mapper doesn't accept it.
  class try_traversor : public traversal_callable_base {
   public:
    using traversal_callable_base::traversal_callable_base;

    template <typename T>
    auto operator()(T&& element)
    -> decltype(std::declval<try_traversor>().get_helper()->try_traverse(
        Strategy{}, std::forward<T>(element))) {
      return this->get_helper()->try_traverse(Strategy{},
                                              std::forward<T>(element));
    }

    /// An alias to the traversor type
    using traversor_type = traversor;
  };

  /// Invokes the real mapper with the given element
  template <typename T>
  auto invoke_mapper(T&& element) -> decltype(
  std::declval<mapping_helper>().mapper_(std::forward<T>(element))) {
    return mapper_(std::forward<T>(element));
  }

  /// SFINAE helper for plain elements not satisfying the tuple like
  /// or container requirements.
  ///
  /// We use the proxy function invoke_mapper here,
  /// because some compilers (MSVC) tend to instantiate the invocation
  /// before matching the tag, which leads to build failures.
  template <typename T>
  auto match(container_category_tag<false, false>, T&& element) -> decltype(
  std::declval<mapping_helper>().invoke_mapper(std::forward<T>(element)));

  /// SFINAE helper for elements satisfying the container
  /// requirements, which are not tuple like.
  template <typename T>
  auto match(container_category_tag<true, false>, T&& container)
  -> decltype(container_remapping::remap(Strategy{},
                                         std::forward<T>(container),
                                         std::declval<traversor>()));

  /// SFINAE helper for elements which are tuple like and
  /// that also may satisfy the container requirements
  template <bool IsContainer, typename T>
  auto match(container_category_tag<IsContainer, true>, T&& tuple_like)
  -> decltype(tuple_like_remapping::remap(Strategy{},
                                          std::forward<T>(tuple_like),
                                          std::declval<traversor>()));

  /// This method implements the functionality for routing
  /// elements through, that aren't accepted by the mapper.
  /// Since the real matcher methods below are failing through SFINAE,
  /// the compiler will try to specialize this function last,
  /// since it's the least concrete one.
  /// This works recursively, so we only call the mapper
  /// with the minimal needed set of accepted arguments.
  template <typename MatcherTag, typename T>
  auto try_match(MatcherTag, T&& element) -> decltype(
  std::declval<mapping_helper>().may_void(std::forward<T>(element))) {
    return this->may_void(std::forward<T>(element));
  }

  /// Match plain elements not satisfying the tuple like or
  /// container requirements.
  ///
  /// We use the proxy function invoke_mapper here,
  /// because some compilers (MSVC) tend to instantiate the invocation
  /// before matching the tag, which leads to build failures.
  template <typename T>
  auto try_match(container_category_tag<false, false>, T&& element) -> decltype(
  std::declval<mapping_helper>().invoke_mapper(std::forward<T>(element))) {
    // T could be any non container or non tuple like type here,
    // take int or hpx::future<int> as an example.
    return invoke_mapper(std::forward<T>(element));
  }

  /// Match elements satisfying the container requirements,
  /// which are not tuple like.
  template <typename T>
  auto try_match(container_category_tag<true, false>, T&& container)
  -> decltype(container_remapping::remap(Strategy{},
                                         std::forward<T>(container),
                                         std::declval<try_traversor>())) {
    return container_remapping::remap(Strategy{}, std::forward<T>(container),
                                      try_traversor{this});
  }

  /// Match elements which are tuple like and that also may
  /// satisfy the container requirements
  /// -> We match tuple like types over container like ones
  template <bool IsContainer, typename T>
  auto try_match(container_category_tag<IsContainer, true>, T&& tuple_like)
  -> decltype(tuple_like_remapping::remap(Strategy{},
                                          std::forward<T>(tuple_like),
                                          std::declval<try_traversor>())) {
    return tuple_like_remapping::remap(Strategy{}, std::forward<T>(tuple_like),
                                       try_traversor{this});
  }

  /// Traverses a single element.
  ///
  /// SFINAE helper: Doesn't allow routing through elements,
  /// that aren't accepted by the mapper
  template <typename T>
  auto traverse(Strategy, T&& element)
  -> decltype(std::declval<mapping_helper>().match(
      std::declval<container_category_of_t<typename std::decay<T>::type>>(),
      std::declval<T>()));

  /// \copybrief traverse
  template <typename T>
  auto try_traverse(Strategy, T&& element)
  -> decltype(std::declval<mapping_helper>().try_match(
      std::declval<container_category_of_t<typename std::decay<T>::type>>(),
      std::declval<T>())) {
    // We use tag dispatching here, to categorize the type T whether
    // it satisfies the container or tuple like requirements.
    // Then we can choose the underlying implementation accordingly.
    return try_match(container_category_of_t<typename std::decay<T>::type>{},
                     std::forward<T>(element));
  }

 public:
  explicit mapping_helper(M mapper) : mapper_(std::move(mapper)) {
  }

  /// \copybrief try_traverse
  template <typename T>
  decltype(auto) init_traverse(strategy_remap_tag, T&& element) {
    return spreading::unpack_or_void(
        try_traverse(strategy_remap_tag{}, std::forward<T>(element)));
  }
  template <typename T>
  void init_traverse(strategy_traverse_tag, T&& element) {
    try_traverse(strategy_traverse_tag{}, std::forward<T>(element));
  }

  /// Calls the traversal method for every element in the pack,
  /// and returns a tuple containing the remapped content.
  template <typename First, typename Second, typename... T>
  decltype(auto) init_traverse(strategy_remap_tag strategy, First&& first,
                               Second&& second, T&&... rest) {
    return spreading::tupelize_or_void(
        try_traverse(strategy, std::forward<First>(first)),
        try_traverse(strategy, std::forward<Second>(second)),
        try_traverse(strategy, std::forward<T>(rest))...);
  }

  /// Calls the traversal method for every element in the pack,
  /// without preserving the return values of the mapper.
  template <typename First, typename Second, typename... T>
  void init_traverse(strategy_traverse_tag strategy, First&& first,
                     Second&& second, T&&... rest) {
    try_traverse(strategy, std::forward<First>(first));
    try_traverse(strategy, std::forward<Second>(second));
    int dummy[] = {0,
                   ((void)try_traverse(strategy, std::forward<T>(rest)), 0)...};
    (void)dummy;
  }
};

/// Traverses the given pack with the given mapper and strategy
template <typename Strategy, typename Mapper, typename... T>
decltype(auto) transform(Strategy strategy, Mapper&& mapper, T&&... pack) {
  mapping_helper<Strategy, typename std::decay<Mapper>::type> helper(
      std::forward<Mapper>(mapper));
  return helper.init_traverse(strategy, std::forward<T>(pack)...);
}
} // namespace traversal
} // namespace detail
} // namespace cti

#endif // CONTINUABLE_DETAIL_TRAVERSE_HPP_INCLUDED


namespace cti {
/// \defgroup Traversal Traversal
/// provides functions to traverse and remap nested packs.
/// \{

/// Maps the pack with the given mapper.
///
/// This function tries to visit all plain elements which may be wrapped in:
/// - homogeneous containers (`std::vector`, `std::list`)
/// - heterogenous containers `(std::tuple`, `std::pair`, `std::array`)
/// and re-assembles the pack with the result of the mapper.
/// Mapping from one type to a different one is supported.
///
/// Elements that aren't accepted by the mapper are routed through
/// and preserved through the hierarchy.
///
///    ```cpp
///    // Maps all integers to floats
///    map_pack([](int value) {
///      return float(value);
///    },
///    1, std::make_tuple(2, std::vector<int>{3, 4}), 5);
///    ```
///
/// \throws       std::exception like objects which are thrown by an
///               invocation to the mapper.
///
/// \param mapper A callable object, which accept an arbitrary type
///               and maps it to another type or the same one.
///
/// \param pack   An arbitrary variadic pack which may contain any type.
///
/// \returns      The mapped element or in case the pack contains
///               multiple elements, the pack is wrapped into
///               a `std::tuple`.
///
/// \since        3.0.0
///
template <typename Mapper, typename... T>
/*keep this inline*/ inline decltype(auto) map_pack(Mapper&& mapper,
                                                    T&&... pack) {
  return detail::traversal::transform(detail::traversal::strategy_remap_tag{},
                                      std::forward<Mapper>(mapper),
                                      std::forward<T>(pack)...);
}

/// Indicate that the result shall be spread across the parent container
/// if possible. This can be used to create a mapper function used
/// in map_pack that maps one element to an arbitrary count (1:n).
///
/// \since 3.0.0
template <typename... T>
constexpr detail::traversal::spreading::spread_box<std::decay_t<T>...>
spread_this(T&&... args) noexcept(
noexcept(std::make_tuple(std::forward<T>(args)...))) {
  using type = detail::traversal::spreading::spread_box<std::decay_t<T>...>;
  return type(std::make_tuple(std::forward<T>(args)...));
}

/// Traverses the pack with the given visitor.
///
/// This function works in the same way as `map_pack`,
/// however, the result of the mapper isn't preserved.
///
/// See `map_pack` for a detailed description.
///
/// \since 3.0.0
template <typename Mapper, typename... T>
void traverse_pack(Mapper&& mapper, T&&... pack) {
  detail::traversal::transform(detail::traversal::strategy_traverse_tag{},
                               std::forward<Mapper>(mapper),
                               std::forward<T>(pack)...);
}
/// \}
} // namespace cti

#endif // CONTINUABLE_TRAVERSE_HPP_INCLUDED

// #include <continuable/detail/base.hpp>

// #include <continuable/detail/flat-variant.hpp>

/*

                        /~` _  _ _|_. _     _ |_ | _
                        \_,(_)| | | || ||_|(_||_)|(/_

                    https://github.com/Naios/continuable
                                   v3.0.0

  Copyright(c) 2015 - 2018 Denis Blank <denis.blank at outlook dot com>

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files(the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and / or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions :

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
**/

#ifndef CONTINUABLE_DETAIL_FLAT_VARIANT_HPP_INCLUDED
#define CONTINUABLE_DETAIL_FLAT_VARIANT_HPP_INCLUDED

#include <cassert>
#include <cstdint>
#include <limits>
#include <memory>
#include <type_traits>
#include <utility>

// #include <continuable/detail/traits.hpp>


namespace cti {
namespace detail {
namespace container {
namespace detail {
// We don't want to pull the algorithm header in
template <typename... T>
constexpr std::size_t max_element_of(std::initializer_list<std::size_t> list) {
  std::size_t m = 0;
  for (auto current : list) {
    if (current > m) {
      m = current;
    }
  }
  return m;
}
template <typename... T>
constexpr auto storage_of_impl() {
  constexpr auto size = max_element_of({sizeof(T)...});
  constexpr auto align = max_element_of({alignof(T)...});
  return std::aligned_storage_t<size, align>{};
}

/// Declares the aligned storage union for the given types
template <typename... T>
using storage_of_t = decltype(storage_of_impl<T...>());

/// The value fpr the empty slot
using slot_t = std::uint8_t;

/// The value which is used to mark the empty slot
using empty_slot =
std::integral_constant<slot_t, std::numeric_limits<slot_t>::max()>;

template <typename... T>
struct flat_variant_base {
  storage_of_t<T...> storage_;
  slot_t slot_;

  constexpr flat_variant_base() : slot_(empty_slot::value) {
  }

  flat_variant_base(flat_variant_base const&) noexcept {
  }
  flat_variant_base(flat_variant_base&&) noexcept {
  }
  flat_variant_base& operator=(flat_variant_base const&) {
    return *this;
  }
  flat_variant_base& operator=(flat_variant_base&&) {
    return *this;
  }
};

template <typename Base>
struct flat_variant_move_base {
  constexpr flat_variant_move_base() = default;

  flat_variant_move_base(flat_variant_move_base const&) = default;
  explicit flat_variant_move_base(flat_variant_move_base&& right) {
    Base& me = *static_cast<Base*>(this);
    Base& other = *static_cast<Base*>(&right);

    if (other.is_empty()) {
      me.set_slot(empty_slot::value);
    } else {

      other.visit([&](auto&& value) {
#ifndef NDEBUG
        me.set_slot(empty_slot::value);
#endif
        // NOLINTNEXTLINE(misc-move-forwarding-reference)
        me.init(std::move(value), other.get_slot());
      });
    }

    other.destroy();
  }
  flat_variant_move_base& operator=(flat_variant_move_base const&) = default;
  flat_variant_move_base& operator=(flat_variant_move_base&& right) {
    Base& me = *static_cast<Base*>(this);
    Base& other = *static_cast<Base*>(&right);

    me.weak_destroy();

    if (other.is_empty()) {
      me.set_slot(empty_slot::value);
    } else {
      other.visit([&](auto&& value) {
        // ...
        me.init(std::move(value), other.get_slot());
      });
    }
    other.destroy();
    return *this;
  }
};
template <typename Base, bool IsCopyable /*= true*/>
struct flat_variant_copy_base : flat_variant_move_base<Base> {
  constexpr flat_variant_copy_base() = default;

  flat_variant_copy_base(flat_variant_copy_base&&) = default;
  explicit flat_variant_copy_base(flat_variant_copy_base const& right)
      : flat_variant_move_base<Base>()
  // TODO noexcept(Base::is_nothrow_move_constructible)
  {
    Base& me = *static_cast<Base*>(this);
    Base const& other = *static_cast<Base const*>(&right);

    if (other.is_empty()) {
      me.set_slot(empty_slot::value);
    } else {
      other.visit([&](auto&& value) {
#ifndef NDEBUG
        me.set_slot(empty_slot::value);
#endif
        me.init(std::move(value), other.get_slot());
      });
    }
  }
  flat_variant_copy_base& operator=(flat_variant_copy_base&&) = default;
  flat_variant_copy_base& operator=(flat_variant_copy_base const& right)
  // TODO  noexcept(Base::is_nothrow_move_constructible)
  {
    Base& me = *static_cast<Base*>(this);
    Base const& other = *static_cast<Base const*>(&right);

    me.weak_destroy();

    if (other.is_empty()) {
      me.set_slot(empty_slot::value);
    } else {
      other.visit([&](auto&& value) {
        // ...
        me.init(std::move(value), other.get_slot());
      });
    }
    return *this;
  }
};
template <typename Base /*, bool IsCopyable = false*/>
struct flat_variant_copy_base<Base, false> : flat_variant_move_base<Base> {
  constexpr flat_variant_copy_base() = default;

  flat_variant_copy_base(flat_variant_copy_base const&) = delete;
  explicit flat_variant_copy_base(flat_variant_copy_base&& right) = default;
  flat_variant_copy_base& operator=(flat_variant_copy_base const&) = delete;
  flat_variant_copy_base& operator=(flat_variant_copy_base&&) = default;
};

/// Deduces to a true_type if all parameters T satisfy the predicate.
template <template <typename> class Predicate, typename... T>
using every = traits::conjunction<Predicate<T>...>;
} // namespace detail

/// A class similar to the one in the variant proposal,
/// however it is capable of carrying an empty state by default.
template <typename... T>
class flat_variant;

template <typename T>
struct is_flat_variant : std::false_type {};
template <typename... T>
struct is_flat_variant<flat_variant<T...>> : std::true_type {};

template <typename... T>
class flat_variant
    : detail::flat_variant_copy_base<
        flat_variant<T...>,
        detail::every<std::is_copy_constructible, T...>::value>,
      detail::flat_variant_base<T...> {

  static_assert(sizeof...(T) > 0, "At least one paremeter T is required!");

  template <typename...>
  friend class flat_variant;
  template <typename>
  friend struct detail::flat_variant_move_base;
  template <typename, bool>
  friend struct detail::flat_variant_copy_base;

  template <typename V>
  flat_variant(V&& value, detail::slot_t const slot) {
#ifndef NDEBUG
    set_slot(detail::empty_slot::value);
#endif
    init(std::forward<V>(value), slot);
  }

 public:
  constexpr flat_variant() = default;
  flat_variant(flat_variant const&) = default;
  flat_variant(flat_variant&&) = default;
  flat_variant& operator=(flat_variant const&) = default;
  flat_variant& operator=(flat_variant&&) = default;

  ~flat_variant() noexcept(
  detail::every<std::is_nothrow_destructible, T...>::value) {
    weak_destroy();
  }

  template <
      typename V,
      std::enable_if_t<!is_flat_variant<std::decay_t<V>>::value>* = nullptr>
  // Since the flat_variant isn't allowed through SFINAE
  // this overload is safed against the linted issue.
  // NOLINTNEXTLINE(misc-forwarding-reference-overload)
  explicit flat_variant(V&& value)
      : flat_variant(std::forward<V>(value),
                     traits::index_of_t<std::decay_t<V>, T...>::value) {
  }

  template <
      typename V,
      std::enable_if_t<!is_flat_variant<std::decay_t<V>>::value>* = nullptr>
  flat_variant& operator=(V&& value) {
    weak_destroy();
    init(std::forward<V>(value),
         traits::index_of_t<std::decay_t<V>, T...>::value);
    return *this;
  }

  template <typename V, std::size_t Index =
  traits::index_of_t<std::decay_t<V>, T...>::value>
  bool is() const noexcept {
    return is_slot(Index);
  }

  bool is_empty() const noexcept {
    return is_slot(detail::empty_slot::value);
  }

  explicit constexpr operator bool() const noexcept {
    return !is_empty();
  }

  template <typename V>
  V& cast() noexcept {
    assert(is_slot(traits::index_of_t<std::decay_t<V>, T...>::value));
    return *reinterpret_cast<std::decay_t<V>*>(&this->storage_);
  }

  template <typename V>
  V const& cast() const noexcept {
    assert(is_slot(traits::index_of_t<std::decay_t<V>, T...>::value));
    return *reinterpret_cast<std::decay_t<V> const*>(&this->storage_);
  }

 private:
  template <typename C, typename V>
  static void visit_dispatch(flat_variant* me, V&& visitor) {
    std::forward<V>(visitor)(me->cast<C>());
  }
  template <typename C, typename V>
  static void visit_dispatch_const(flat_variant const* me, V&& visitor) {
    std::forward<V>(visitor)(me->cast<C>());
  }

  template <typename V>
  void visit(V&& visitor) {
    if (!is_empty()) {
      using callback_t = void (*)(flat_variant*, V &&);
      constexpr callback_t const callbacks[] = {&visit_dispatch<T, V>...};
      callbacks[get_slot()](this, std::forward<V>(visitor));
    }
  }
  template <typename V>
  void visit(V&& visitor) const {
    if (!is_empty()) {
      using callback_t = void (*)(flat_variant const*, V&&);
      constexpr callback_t const callbacks[] = {&visit_dispatch_const<T, V>...};
      callbacks[get_slot()](this, std::forward<V>(visitor));
    }
  }

  template <typename V>
  void init(V&& value, detail::slot_t const slot) {
    assert(is_empty());
    assert(sizeof(this->storage_) >= sizeof(std::decay_t<V>));

    using type = std::decay_t<V>;
    new (&this->storage_) type(std::forward<V>(value));
    set_slot(slot);
  }
  void destroy() {
    weak_destroy();

#ifdef NDEBUG
    set_slot(detail::empty_slot::value);
#endif
  }
  void weak_destroy() {
    visit([&](auto&& value) {
      using type = std::decay_t<decltype(value)>;
      value.~type();
    });

#ifndef NDEBUG
    set_slot(detail::empty_slot::value);
#endif
  }
  detail::slot_t get_slot() const noexcept {
    return this->slot_;
  }
  bool is_slot(detail::slot_t const slot) const noexcept {
    return get_slot() == slot;
  }
  void set_slot(detail::slot_t const slot) {
    this->slot_ = slot;
  }
};
} // namespace container
} // namespace detail
} // namespace cti

#endif // CONTINUABLE_DETAIL_FLAT_VARIANT_HPP_INCLUDED

// #include <continuable/detail/traits.hpp>


namespace cti {
namespace detail {
namespace connection {
/// This namespace provides utilities for performing compound
/// connections between deeply nested continuables and values.
///
/// We create the result pack from the provides values and
/// the async values if those are default constructible,
/// otherwise use a lazy initialization wrapper and unwrap
/// the whole pack when the connection is finished.
///   - value -> value
///   - single async value -> single value
///   - multiple async value -> tuple of async values.
namespace aggregated {
/// Guards a type to be default constructible,
/// and wraps it into an optional type if it isn't default constructible.
template <typename T>
using lazy_value_t = std::conditional_t<std::is_default_constructible<T>::value,
                                        T, container::flat_variant<T>>;

template <typename T>
decltype(auto) unpack_lazy(T&& value) {
  return std::forward<T>(value);
}
template <typename T>
T&& unpack_lazy(container::flat_variant<T>&& value) {
  assert(value.template is<T>() &&
      "The connection was finalized before all values were present!");

  return std::move(value.template cast<T>());
}

template <typename Continuable>
class continuable_box;
template <typename Data>
class continuable_box<continuable_base<Data, hints::signature_hint_tag<>>> {

  continuable_base<Data, hints::signature_hint_tag<>> continuable_;

 public:
  explicit continuable_box(
      continuable_base<Data, hints::signature_hint_tag<>>&& continuable)
      : continuable_(std::move(continuable)) {
  }

  continuable_base<Data, hints::signature_hint_tag<>>&& fetch() {
    return std::move(continuable_);
  }

  void assign() {
  }

  auto unbox() && {
    return spread_this();
  }
};
template <typename Data, typename First>
class continuable_box<
    continuable_base<Data, hints::signature_hint_tag<First>>> {

  continuable_base<Data, hints::signature_hint_tag<First>> continuable_;
  lazy_value_t<First> first_;

 public:
  explicit continuable_box(
      continuable_base<Data, hints::signature_hint_tag<First>>&& continuable)
      : continuable_(std::move(continuable)) {
  }

  continuable_base<Data, hints::signature_hint_tag<First>>&& fetch() {
    return std::move(continuable_);
  }

  void assign(First first) {
    first_ = std::move(first);
  }

  auto unbox() && {
    return unpack_lazy(std::move(first_));
  }
};
template <typename Data, typename First, typename Second, typename... Rest>
class continuable_box<
    continuable_base<Data, hints::signature_hint_tag<First, Second, Rest...>>> {

  continuable_base<Data, hints::signature_hint_tag<First, Second, Rest...>>
      continuable_;
  lazy_value_t<std::tuple<First, Second, Rest...>> args_;

 public:
  explicit continuable_box(
      continuable_base<Data,
                       hints::signature_hint_tag<First, Second, Rest...>>&&
      continuable)
      : continuable_(std::move(continuable)) {
  }

  continuable_base<Data, hints::signature_hint_tag<First, Second, Rest...>>&&
  fetch() {
    return std::move(continuable_);
  }

  void assign(First first, Second second, Rest... rest) {
    args_ = std::make_tuple(std::move(first), std::move(second),
                            std::move(rest)...);
  }

  auto unbox() && {
    return traits::unpack(unpack_lazy(std::move(args_)), [](auto&&... args) {
      return spread_this(std::forward<decltype(args)>(args)...);
    });
  }
};

template <typename T>
struct is_continuable_box : std::false_type {};
template <typename Continuable>
struct is_continuable_box<continuable_box<Continuable>> : std::true_type {};

namespace detail {
/// Maps a deeply nested pack of continuables to a continuable_box
struct continuable_box_packer {
  template <
      typename T,
      std::enable_if_t<base::is_continuable<std::decay_t<T>>::value>* = nullptr>
  auto operator()(T&& continuable) {
    return continuable_box<std::decay_t<T>>{std::forward<T>(continuable)};
  }
};
/// Maps a deeply nested pack of continuable_boxes to its result
struct continuable_box_unpacker {
  template <
      typename T,
      std::enable_if_t<is_continuable_box<std::decay_t<T>>::value>* = nullptr>
  auto operator()(T&& box) {
    return std::forward<T>(box).unbox();
  }
};
} // namespace detail

/// Returns the boxed pack of the given deeply nested pack.
/// This transforms all continuables into a continuable_box which is
/// capable of caching the result from the corresponding continuable.
template <typename... Args>
constexpr auto box_continuables(Args&&... args) {
  return cti::map_pack(detail::continuable_box_packer{},
                       std::forward<Args>(args)...);
}

/// Returns the unboxed pack of the given deeply nested boxed pack.
/// This transforms all continuable_boxes into its result.
template <typename... Args>
constexpr auto unbox_continuables(Args&&... args) {
  return cti::map_pack(detail::continuable_box_unpacker{},
                       std::forward<Args>(args)...);
}

namespace detail {
template <typename Callback, typename Data>
constexpr auto finalize_impl(traits::identity<void>, Callback&& callback,
                             Data&&) {
  return std::forward<Callback>(callback)();
}
template <typename... Args, typename Callback, typename Data>
constexpr auto finalize_impl(traits::identity<std::tuple<Args...>>,
                             Callback&& callback, Data&& data) {
  // Call the final callback with the cleaned result
  return traits::unpack(unbox_continuables(std::forward<Data>(data)),
                        std::forward<Callback>(callback));
}

struct hint_mapper {
  template <typename... T>
  constexpr auto operator()(T...) -> hints::signature_hint_tag<T...> {
    return {};
  }
};
} // namespace detail

template <typename Callback, typename Data>
constexpr auto finalize_data(Callback&& callback, Data&& data) {
  using result_t = decltype(unbox_continuables(std::forward<Data>(data)));
  // Guard the final result against void
  return detail::finalize_impl(traits::identity<std::decay_t<result_t>>{},
                               std::forward<Callback>(callback),
                               std::forward<Data>(data));
}

template <typename Data>
constexpr auto hint_of_data() {
  return decltype(finalize_data(detail::hint_mapper{}, std::declval<Data>())){};
}
} // namespace aggregated
} // namespace connection
} // namespace detail
} // namespace cti

#endif // CONTINUABLE_DETAIL_CONNECTION_REMAPPING_HPP_INCLUDED

// #include <continuable/detail/connection.hpp>

/*

                        /~` _  _ _|_. _     _ |_ | _
                        \_,(_)| | | || ||_|(_||_)|(/_

                    https://github.com/Naios/continuable
                                   v3.0.0

  Copyright(c) 2015 - 2018 Denis Blank <denis.blank at outlook dot com>

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files(the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and / or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions :

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
**/

#ifndef CONTINUABLE_DETAIL_CONNECTION_HPP_INCLUDED
#define CONTINUABLE_DETAIL_CONNECTION_HPP_INCLUDED

#include <cassert>
#include <tuple>
#include <type_traits>
#include <utility>

// #include <continuable/continuable-traverse.hpp>

// #include <continuable/detail/base.hpp>

// #include <continuable/detail/traits.hpp>

// #include <continuable/detail/types.hpp>

// #include <continuable/detail/util.hpp>


namespace cti {
namespace detail {
/// The namespace `connection` offers methods to chain continuations together
/// with `all`, `any` or `seq` logic.
namespace connection {
template <typename T>
struct is_connection_strategy // ...
    : std::false_type {};

/// Adds the given continuation tuple to the left connection
template <typename... LeftArgs, typename... RightArgs>
auto chain_connection(std::tuple<LeftArgs...> leftPack,
                      std::tuple<RightArgs...> rightPack) {

  return traits::merge(std::move(leftPack), std::move(rightPack));
}

/// Normalizes a continuation to a tuple holding an arbitrary count of
/// continuations matching the given strategy.
///
/// Basically we can encounter 3 cases:
/// - The continuable isn't in any strategy:
///   -> make a tuple containing the continuable as only element
template <
    typename Strategy, typename Data, typename Annotation,
    std::enable_if_t<!is_connection_strategy<Annotation>::value>* = nullptr>
auto normalize(Strategy /*strategy*/,
               continuable_base<Data, Annotation>&& continuation) {

  // If the continuation isn't a strategy initialize the strategy
  return std::make_tuple(std::move(continuation));
}
/// - The continuable is in a different strategy then the current one:
///   -> materialize it
template <
    typename Strategy, typename Data, typename Annotation,
    std::enable_if_t<is_connection_strategy<Annotation>::value>* = nullptr>
auto normalize(Strategy /*strategy*/,
               continuable_base<Data, Annotation>&& continuation) {

  // If the right continuation is a different strategy materialize it
  // in order to keep the precedence in cases where: `c1 && (c2 || c3)`.
  return std::make_tuple(base::attorney::materialize(std::move(continuation)));
}
/// - The continuable is inside the current strategy state:
///   -> return the data of the tuple
template <typename Strategy, typename Data>
auto normalize(Strategy /*strategy*/,
               continuable_base<Data, Strategy>&& continuation) {

  // If we are in the given strategy we can just use the data of the continuable
  return base::attorney::consume_data(std::move(continuation));
}

/// Entry function for connecting two continuables with a given strategy.
template <typename Strategy, typename LData, typename LAnnotation,
    typename RData, typename RAnnotation>
auto connect(Strategy strategy, continuable_base<LData, LAnnotation>&& left,
             continuable_base<RData, RAnnotation>&& right) {

  auto ownership_ =
      base::attorney::ownership_of(left) | base::attorney::ownership_of(right);

  left.freeze();
  right.freeze();

  // Make the new data which consists of a tuple containing
  // all connected continuables.
  auto data = chain_connection(normalize(strategy, std::move(left)),
                               normalize(strategy, std::move(right)));

  // Return a new continuable containing the tuple and holding
  // the current strategy as annotation.
  return base::attorney::create(std::move(data), strategy, ownership_);
}

/// All strategies should specialize this class in order to provide:
/// - A finalize static method that creates the callable object which
///   is invoked with the callback to call when the connection is finished.
/// - A static method hint that returns the new signature hint.
template <typename Strategy>
struct connection_finalizer;

/// Finalizes the connection logic of a given connection
template <typename Data, typename Strategy>
auto finalize_connection(continuable_base<Data, Strategy>&& continuation) {
  using finalizer = connection_finalizer<Strategy>;

  util::ownership ownership = base::attorney::ownership_of(continuation);
  auto connection = base::attorney::consume_data(std::move(continuation));

  // Return a new continuable which
  return finalizer::finalize(std::move(connection), std::move(ownership));
}

/// A base class from which the continuable may inherit in order to
/// provide a materializer method which will finalize an oustanding strategy.
template <typename Continuable, typename = void>
struct materializer {
  static constexpr auto&& apply(Continuable&& continuable) {
    return std::move(continuable);
  }
};
template <typename Data, typename Strategy>
struct materializer<continuable_base<Data, Strategy>,
                    std::enable_if_t<is_connection_strategy<Strategy>::value>> {

  static constexpr auto apply(continuable_base<Data, Strategy>&& continuable) {
    return finalize_connection(std::move(continuable));
  }
};

class prepare_continuables {
  util::ownership& ownership_;

 public:
  explicit constexpr prepare_continuables(util::ownership& ownership)
      : ownership_(ownership) {
  }

  template <typename Continuable,
      std::enable_if_t<base::is_continuable<
          std::decay_t<Continuable>>::value>* = nullptr>
  auto operator()(Continuable&& continuable) noexcept {
    util::ownership current = base::attorney::ownership_of(continuable);
    assert(current.is_acquired() &&
        "Only valid continuables should be passed!");

    // Propagate a frozen state to the new continuable
    if (!ownership_.is_frozen() && current.is_frozen()) {
      ownership_.freeze();
    }

    // Freeze the continuable since it is stored for later usage
    continuable.freeze();

    // Materialize every continuable
    // TODO Actually we would just need to consume the data here
    return base::attorney::materialize(std::forward<Continuable>(continuable));
  }
};

template <typename Strategy, typename... Args>
auto apply_connection(Strategy, Args&&... args) {
  using finalizer = connection_finalizer<Strategy>;

  // Freeze every continuable inside the given arguments,
  // and freeze the ownership if one of the continuables
  // is frozen already.
  // Additionally test whether every continuable is acquired.
  // Also materialize every continuable.
  util::ownership ownership;
  auto connection = map_pack(prepare_continuables{ownership},
                             std::make_tuple(std::forward<Args>(args)...));

  return finalizer::finalize(std::move(connection), std::move(ownership));
}
} // namespace connection
} // namespace detail
} // namespace cti

#endif // CONTINUABLE_DETAIL_CONNECTION_HPP_INCLUDED

// #include <continuable/detail/hints.hpp>

// #include <continuable/detail/traits.hpp>

// #include <continuable/detail/types.hpp>


namespace cti {
namespace detail {
namespace connection {
namespace all {
/// Caches the partial results and invokes the callback when all results
/// are arrived. This class is thread safe.
template <typename Callback, typename Result>
class result_submitter
    : public std::enable_shared_from_this<result_submitter<Callback, Result>>,
      public util::non_movable {

  Callback callback_;
  Result result_;

  std::atomic<std::size_t> left_;
  std::once_flag flag_;

  // Invokes the callback with the cached result
  void invoke() {
    assert((left_ == 0U) && "Expected that the submitter is finished!");
    std::atomic_thread_fence(std::memory_order_acquire);

    // Call the final callback with the cleaned result
    std::call_once(flag_, [&] {
      aggregated::finalize_data(std::move(callback_), std::move(result_));
    });
  }

  // Completes one result
  void complete_one() {
    assert((left_ > 0U) && "Expected that the submitter isn't finished!");

    auto const current = --left_;
    if (!current) {
      invoke();
    }
  }

  template <typename Box>
  struct partial_all_callback {
    Box* box;
    std::shared_ptr<result_submitter> me;

    template <typename... Args>
    void operator()(Args&&... args) && {

      // Assign the result to the target
      box->assign(std::forward<decltype(args)>(args)...);

      // Complete one result
      me->complete_one();
    }

    template <typename... PartialArgs>
    void operator()(types::dispatch_error_tag tag, types::error_type error) && {
      // We never complete the connection, but we forward the first error
      // which was raised.
      std::call_once(me->flag_, std::move(me->callback_), tag,
                     std::move(error));
    }
  };

 public:
  explicit result_submitter(Callback callback, Result&& result)
      : callback_(std::move(callback)), result_(std::move(result)), left_(1) {
  }

  /// Creates a submitter which submits it's result into the storage
  template <typename Box>
  auto create_callback(Box* box) {
    left_.fetch_add(1, std::memory_order_seq_cst);
    return partial_all_callback<std::decay_t<Box>>{box,
                                                   this->shared_from_this()};
  }

  /// Initially the counter is created with an initial count of 1 in order
  /// to prevent that the connection is finished before all callbacks
  /// were registered.
  void accept() {
    complete_one();
  }

  constexpr auto& head() noexcept {
    return result_;
  }
};

template <typename Submitter>
struct continuable_dispatcher {
  std::shared_ptr<Submitter>& submitter;

  template <typename Box, std::enable_if_t<aggregated::is_continuable_box<
      std::decay_t<Box>>::value>* = nullptr>
  void operator()(Box&& box) const {
    // Retrieve a callback from the submitter and attach it to the continuable
    box.fetch().next(submitter->create_callback(std::addressof(box))).done();
  }
};
} // namespace all

struct connection_strategy_all_tag {};
template <>
struct is_connection_strategy<connection_strategy_all_tag> // ...
    : std::true_type {};

/// Finalizes the all logic of a given connection
template <>
struct connection_finalizer<connection_strategy_all_tag> {
  /// Finalizes the all logic of a given connection
  template <typename Connection>
  static auto finalize(Connection&& connection, util::ownership ownership) {
    // Create the target result from the connection
    auto result =
        aggregated::box_continuables(std::forward<Connection>(connection));

    auto signature = aggregated::hint_of_data<decltype(result)>();

    return base::attorney::create(
        [result = std::move(result)](auto&& callback) mutable {

          using submitter_t =
          all::result_submitter<std::decay_t<decltype(callback)>,
                                std::decay_t<decltype(result)>>;

          // Create the shared state which holds the result and the final
          // callback
          auto state = std::make_shared<submitter_t>(
              std::forward<decltype(callback)>(callback), std::move(result));

          // Dispatch the continuables and store its partial result
          // in the whole result
          traverse_pack(all::continuable_dispatcher<submitter_t>{state},
                        state->head());

          // Finalize the connection if all results arrived in-place
          state->accept();
        },
        signature, std::move(ownership));
  }
};
} // namespace connection
} // namespace detail
} // namespace cti

#endif // CONTINUABLE_DETAIL_CONNECTION_ALL_HPP_INCLUDED

// #include <continuable/detail/connection-any.hpp>

/*

                        /~` _  _ _|_. _     _ |_ | _
                        \_,(_)| | | || ||_|(_||_)|(/_

                    https://github.com/Naios/continuable
                                   v3.0.0

  Copyright(c) 2015 - 2018 Denis Blank <denis.blank at outlook dot com>

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files(the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and / or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions :

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
**/

#ifndef CONTINUABLE_DETAIL_CONNECTION_ANY_HPP_INCLUDED
#define CONTINUABLE_DETAIL_CONNECTION_ANY_HPP_INCLUDED

#include <atomic>
#include <memory>
#include <mutex>
#include <tuple>
#include <type_traits>
#include <utility>

// #include <continuable/continuable-promise-base.hpp>

/*

                        /~` _  _ _|_. _     _ |_ | _
                        \_,(_)| | | || ||_|(_||_)|(/_

                    https://github.com/Naios/continuable
                                   v3.0.0

  Copyright(c) 2015 - 2018 Denis Blank <denis.blank at outlook dot com>

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files(the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and / or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions :

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
**/

#ifndef CONTINUABLE_PROMISE_BASE_HPP_INCLUDED
#define CONTINUABLE_PROMISE_BASE_HPP_INCLUDED

#include <type_traits>
#include <utility>

// #include <continuable/detail/hints.hpp>

// #include <continuable/detail/types.hpp>

// #include <continuable/detail/util.hpp>


namespace cti {
/// \defgroup Base Base
/// provides classes and functions to create continuable_base objects.
/// \{

/// The promise_base makes it possible to resolve an asynchronous
/// continuable through it's result or through an error type.
///
/// Use the promise type defined in `continuable/continuable_types.hpp`,
/// in order to use this class.
///
/// If we want to resolve the  promise_base trough the call operator,
/// and we want to resolve it through an exception, we must call it with a
/// dispatch_error_tag as first and the exception as second argument.
/// Additionally the promise is resolveable only through its call
/// operator when invoked as an r-value.
///
/// \since 2.0.0
// clang-format off
template <typename Data, typename Hint>
class promise_base
/// \cond false
;
template <typename Data, typename... Args>
class promise_base<Data, detail::hints::signature_hint_tag<Args...>>
    : detail::util::non_copyable
  /// \endcond
{ // clang-format on

  /// \cond false
  // The callback type
  Data data_;
  /// \endcond

 public:
  /// Constructor accepting the data object
  explicit promise_base(Data data) : data_(std::move(data)) {
  }

  /// Constructor accepting any object convertible to the data object
  template <typename OData, std::enable_if_t<std::is_convertible<
      std::decay_t<OData>, Data>::value>* = nullptr>
  promise_base(OData&& data) : data_(std::forward<OData>(data)) {
  }

  /// Resolves the continuation with the given values.
  ///
  /// \throws This method never throws an exception.
  ///
  /// \since  2.0.0
  void operator()(Args... args) && noexcept {
    std::move(data_)(std::move(args)...);
  }
  /// Resolves the continuation with the given exception.
  ///
  /// \throws This method never throws an exception.
  ///
  /// \since  2.0.0
  void operator()(detail::types::dispatch_error_tag tag,
                  detail::types::error_type exception) &&
  noexcept {
    std::move(data_)(tag, std::move(exception));
  }

  /// Resolves the continuation with the given values.
  ///
  /// \throws This method never throws an exception.
  ///
  /// \since  2.0.0
  void set_value(Args... args) noexcept {
    std::move(data_)(std::move(args)...);
  }

  /// Resolves the continuation with the given exception.
  ///
  /// \throws This method never throws an exception.
  ///
  /// \since  2.0.0
  void set_exception(detail::types::error_type exception) noexcept {
    std::move(data_)(detail::types::dispatch_error_tag{}, std::move(exception));
  }
};
/// \}
} // namespace cti

#endif // CONTINUABLE_PROMISE_BASE_HPP_INCLUDED

// #include <continuable/continuable-traverse.hpp>

// #include <continuable/detail/base.hpp>

// #include <continuable/detail/container-category.hpp>

// #include <continuable/detail/hints.hpp>

// #include <continuable/detail/traits.hpp>

// #include <continuable/detail/types.hpp>


namespace cti {
namespace detail {
namespace connection {
namespace any {
/// Invokes the callback with the first arriving result
template <typename T>
class any_result_submitter
    : public std::enable_shared_from_this<any_result_submitter<T>>,
      public util::non_movable {

  T callback_;
  std::once_flag flag_;

  struct any_callback {
    std::shared_ptr<any_result_submitter> me_;

    template <typename... PartialArgs>
    void operator()(PartialArgs&&... args) && {
      me_->invoke(std::forward<decltype(args)>(args)...);
    }
  };

 public:
  explicit any_result_submitter(T callback) : callback_(std::move(callback)) {
  }

  /// Creates a submitter which submits it's result to the callback
  auto create_callback() {
    return any_callback{this->shared_from_this()};
  }

 private:
  // Invokes the callback with the given arguments
  template <typename... ActualArgs>
  void invoke(ActualArgs&&... args) {
    std::call_once(flag_, std::move(callback_),
                   std::forward<ActualArgs>(args)...);
  }
};

struct result_deducer {
  template <typename T>
  static auto deduce_one(std::false_type, traits::identity<T>) {
    static_assert(traits::fail<T>::value,
                  "Non continuable types except tuple like and homogeneous "
                  "containers aren't allowed inside an any expression!");
  }
  template <typename T>
  static auto deduce_one(std::true_type, traits::identity<T> id) {
    return hints::hint_of(id);
  }
  template <typename T>
  static auto deduce(traversal::container_category_tag<false, false>,
                     traits::identity<T> id) {
    return deduce_one<T>(base::is_continuable<T>{}, id);
  }

  /// Deduce a homogeneous container
  template <bool IsTupleLike, typename T>
  static auto deduce(traversal::container_category_tag<true, IsTupleLike>,
                     traits::identity<T>) {

    // Deduce the containing type
    using element_t = std::decay_t<decltype(*std::declval<T>().begin())>;
    return deduce(traversal::container_category_of_t<element_t>{},
                  traits::identity<element_t>{});
  }

  template <typename First, typename... T>
  static auto deduce_same_hints(First first, T...) {
    static_assert(traits::conjunction<std::is_same<First, T>...>::value,
                  "The continuables inside the given pack must have the "
                  "same signature hint!");

    return first;
  }

  template <std::size_t... I, typename T>
  static auto deduce_tuple_like(std::integer_sequence<std::size_t, I...>,
                                traits::identity<T>) {

    return deduce_same_hints(deduce(
        traversal::container_category_of_t<
            std::decay_t<decltype(std::get<I>(std::declval<T>()))>>{},
        traits::identity<
            std::decay_t<decltype(std::get<I>(std::declval<T>()))>>{})...);
  }

  /// Traverse tuple like container
  template <typename T>
  static auto deduce(traversal::container_category_tag<false, true>,
                     traits::identity<T> id) {

    constexpr auto const size = std::tuple_size<T>::value;
    return deduce_tuple_like(std::make_index_sequence<size>{}, id);
  }
};

template <typename Submitter>
struct continuable_dispatcher {
  std::shared_ptr<Submitter>& submitter;

  template <typename Continuable,
      std::enable_if_t<base::is_continuable<
          std::decay_t<Continuable>>::value>* = nullptr>
  void operator()(Continuable&& continuable) {
    // Retrieve a callback from the submitter and attach it to the continuable
    std::forward<Continuable>(continuable)
        .next(submitter->create_callback())
        .done();
  }
};
} // namespace any

struct connection_strategy_any_tag {};
template <>
struct is_connection_strategy<connection_strategy_any_tag> // ...
    : std::true_type {};

/// Finalizes the any logic of a given connection
template <>
struct connection_finalizer<connection_strategy_any_tag> {
  template <typename Connection>
  static auto finalize(Connection&& connection, util::ownership ownership) {
    constexpr auto const signature = decltype(any::result_deducer::deduce(
        traversal::container_category_of_t<std::decay_t<Connection>>{},
        traits::identity<std::decay_t<Connection>>{})){};

    return base::attorney::create(
        [connection =
        std::forward<Connection>(connection)](auto&& callback) mutable {

          using submitter_t =
          any::any_result_submitter<std::decay_t<decltype(callback)>>;

          // Create the submitter which calls the given callback once at the
          // first callback invocation.
          auto submitter = std::make_shared<submitter_t>(
              std::forward<decltype(callback)>(callback));

          traverse_pack(any::continuable_dispatcher<submitter_t>{submitter},
                        std::move(connection));
        },
        signature, std::move(ownership));
  }
};
} // namespace connection
} // namespace detail
} // namespace cti

#endif // CONTINUABLE_DETAIL_CONNECTION_ANY_HPP_INCLUDED

// #include <continuable/detail/connection-seq.hpp>

/*

                        /~` _  _ _|_. _     _ |_ | _
                        \_,(_)| | | || ||_|(_||_)|(/_

                    https://github.com/Naios/continuable
                                   v3.0.0

  Copyright(c) 2015 - 2018 Denis Blank <denis.blank at outlook dot com>

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files(the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and / or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions :

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
**/

#ifndef CONTINUABLE_DETAIL_CONNECTION_SEQ_HPP_INCLUDED
#define CONTINUABLE_DETAIL_CONNECTION_SEQ_HPP_INCLUDED

#include <cassert>
#include <memory>
#include <tuple>
#include <type_traits>
#include <utility>

// #include <continuable/continuable-traverse-async.hpp>

/*

                        /~` _  _ _|_. _     _ |_ | _
                        \_,(_)| | | || ||_|(_||_)|(/_

                    https://github.com/Naios/continuable
                                   v3.0.0

  Copyright(c) 2015 - 2018 Denis Blank <denis.blank at outlook dot com>

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files(the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and / or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions :

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
**/

#ifndef CONTINUABLE_TRAVERSE_ASYNC_HPP_INCLUDED
#define CONTINUABLE_TRAVERSE_ASYNC_HPP_INCLUDED

#include <utility>

// #include <continuable/detail/traverse-async.hpp>

/*

                        /~` _  _ _|_. _     _ |_ | _
                        \_,(_)| | | || ||_|(_||_)|(/_

                    https://github.com/Naios/continuable
                                   v3.0.0

  Copyright(c) 2015 - 2018 Denis Blank <denis.blank at outlook dot com>

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files(the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and / or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions :

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
**/

#ifndef CONTINUABLE_DETAIL_TRAVERSE_ASYNC_HPP_INCLUDED
#define CONTINUABLE_DETAIL_TRAVERSE_ASYNC_HPP_INCLUDED

#include <atomic>
#include <cassert>
#include <cstddef>
#include <iterator>
#include <memory>
#include <tuple>
#include <type_traits>
#include <utility>

// #include <continuable/detail/container-category.hpp>

// #include <continuable/detail/traits.hpp>


namespace cti {
namespace detail {
namespace traversal {
/// A tag which is passed to the `operator()` of the visitor
/// if an element is visited synchronously.
struct async_traverse_visit_tag {};

/// A tag which is passed to the `operator()` of the visitor
/// if an element is visited after the traversal was detached.
struct async_traverse_detach_tag {};

/// A tag which is passed to the `operator()` of the visitor
/// if the asynchronous pack traversal was finished.
struct async_traverse_complete_tag {};

/// A tag to identify that a mapper shall be constructed in-place
/// from the first argument passed.
template <typename T>
struct async_traverse_in_place_tag {};

/// Relocates the given pack with the given offset
template <std::size_t Offset, typename Pack>
struct relocate_index_pack;
template <std::size_t Offset, std::size_t... Sequence>
struct relocate_index_pack<Offset,
                           std::integer_sequence<std::size_t, Sequence...>>
    : std::common_type<
        std::integer_sequence<std::size_t, (Sequence + Offset)...>> {};

/// Creates a sequence from begin to end explicitly
template <std::size_t Begin, std::size_t End>
using explicit_range_sequence_of_t =
typename relocate_index_pack<Begin,
                             std::make_index_sequence<End - Begin>>::type;

/// Continues the traversal when the object is called
template <typename Frame, typename State>
class resume_traversal_callable {
  Frame frame_;
  State state_;

 public:
  explicit resume_traversal_callable(Frame frame, State state)
      : frame_(std::move(frame)), state_(std::move(state)) {
  }

  /// The callable operator for resuming
  /// the asynchronous pack traversal
  void operator()();
};

/// Creates a resume_traversal_callable from the given frame and the
/// given iterator tuple.
template <typename Frame, typename State>
auto make_resume_traversal_callable(Frame&& frame, State&& state)
-> resume_traversal_callable<typename std::decay<Frame>::type,
                             typename std::decay<State>::type> {
  return resume_traversal_callable<typename std::decay<Frame>::type,
                                   typename std::decay<State>::type>(
      std::forward<Frame>(frame), std::forward<State>(state));
}

template <typename T, typename = void>
struct has_head : std::false_type {};
template <typename T>
struct has_head<T, traits::void_t<decltype(std::declval<T>().head())>>
    : std::true_type {};

template <typename Visitor, typename... Args>
class async_traversal_frame_data : public Visitor {

  std::tuple<Args...> args_;

 public:
  explicit async_traversal_frame_data(Visitor visitor, Args... args)
      : Visitor(std::move(visitor)),
        args_(std::make_tuple(std::move(args)...)) {
  }
  template <typename MapperArg>
  explicit async_traversal_frame_data(async_traverse_in_place_tag<Visitor>,
                                      MapperArg&& mapper_arg, Args... args)
      : Visitor(std::forward<MapperArg>(mapper_arg)),
        args_(std::make_tuple(std::move(args)...)) {
  }

  /// Returns the arguments of the frame
  std::tuple<Args...>& head() noexcept {
    return args_;
  }
};
template <typename Visitor>
class async_traversal_frame_no_data : public Visitor {
 public:
  explicit async_traversal_frame_no_data(Visitor visitor)
      : Visitor(std::move(visitor)) {
  }
  template <typename MapperArg>
  explicit async_traversal_frame_no_data(async_traverse_in_place_tag<Visitor>,
                                         MapperArg&& mapper_arg)
      : Visitor(std::forward<MapperArg>(mapper_arg)) {
  }
};

template <typename Visitor, typename... Args>
using data_layout_t =
std::conditional_t<has_head<Visitor>::value,
                   async_traversal_frame_no_data<Visitor>,
                   async_traversal_frame_data<Visitor, Args...>>;

/// Stores the visitor and the arguments to traverse
template <typename Visitor, typename... Args>
class async_traversal_frame : public data_layout_t<Visitor, Args...> {
#ifndef NDEBUG
  std::atomic<bool> finished_;
#endif // NDEBUG

  Visitor& visitor() noexcept {
    return *static_cast<Visitor*>(this);
  }

  Visitor const& visitor() const noexcept {
    return *static_cast<Visitor const*>(this);
  }

 public:
  template <typename... T>
  explicit async_traversal_frame(T&&... args)
      : data_layout_t<Visitor, Args...>(std::forward<T>(args)...)
#ifndef NDEBUG
      ,
        finished_(false)
#endif // NDEBUG
  {
  }

  /// We require a virtual base
  virtual ~async_traversal_frame() override = default;

  /// Calls the visitor with the given element
  template <typename T>
  auto traverse(T&& value) -> decltype(visitor()(async_traverse_visit_tag{},
                                                 std::forward<T>(value))) {
    return visitor()(async_traverse_visit_tag{}, std::forward<T>(value));
  }

  /// Calls the visitor with the given element and a continuation
  /// which is capable of continuing the asynchronous traversal
  /// when it's called later.
  template <typename T, typename Hierarchy>
  void async_continue(T&& value, Hierarchy&& hierarchy) {
    // Cast the frame up
    auto frame = std::static_pointer_cast<async_traversal_frame>(
        this->shared_from_this());

    // Create a callable object which resumes the current
    // traversal when it's called.
    auto resumable = make_resume_traversal_callable(
        std::move(frame), std::forward<Hierarchy>(hierarchy));

    // Invoke the visitor with the current value and the
    // callable object to resume the control flow.
    visitor()(async_traverse_detach_tag{}, std::forward<T>(value),
              std::move(resumable));
  }

  /// Calls the visitor with no arguments to signalize that the
  /// asynchronous traversal was finished.
  void async_complete() {
#ifndef NDEBUG
    {
      bool expected = false;
      assert(finished_.compare_exchange_strong(expected, true));
    }
#endif // NDEBUG

    visitor()(async_traverse_complete_tag{}, std::move(this->head()));
  }
};

template <typename Target, std::size_t Begin, std::size_t End>
struct static_async_range {
  Target* target_;

  constexpr decltype(auto) operator*() const noexcept {
    return std::get<Begin>(*target_);
  }

  template <std::size_t Position>
  constexpr auto relocate() const noexcept {
    return static_async_range<Target, Position, End>{target_};
  }

  constexpr auto next() const noexcept {
    return static_async_range<Target, Begin + 1, End>{target_};
  }

  constexpr bool is_finished() const noexcept {
    return false;
  }
};

/// Specialization for the end marker which doesn't provide
/// a particular element dereference
template <typename Target, std::size_t Begin>
struct static_async_range<Target, Begin, Begin> {
  explicit static_async_range(Target*) {
  }

  constexpr bool is_finished() const noexcept {
    return true;
  }
};

/// Returns a static range for the given type
template <typename T>
auto make_static_range(T&& element) {
  using range_t = static_async_range<std::decay_t<T>, 0U,
                                     std::tuple_size<std::decay_t<T>>::value>;

  return range_t{std::addressof(element)};
}

template <typename Begin, typename Sentinel>
struct dynamic_async_range {
  Begin begin_;
  Sentinel sentinel_;

  dynamic_async_range& operator++() noexcept {
    ++begin_;
    return *this;
  }

  auto operator*() const noexcept -> decltype(*std::declval<Begin const&>()) {
    return *begin_;
  }

  dynamic_async_range next() const {
    dynamic_async_range other = *this;
    ++other;
    return other;
  }

  bool is_finished() const {
    return begin_ == sentinel_;
  }
};

template <typename T>
using dynamic_async_range_of_t = dynamic_async_range<
    typename std::decay<decltype(std::begin(std::declval<T>()))>::type,
    typename std::decay<decltype(std::end(std::declval<T>()))>::type>;

/// Returns a dynamic range for the given type
template <typename T>
auto make_dynamic_async_range(T&& element) {
  using range_t = dynamic_async_range_of_t<T>;
  return range_t{std::begin(element), std::end(element)};
}

/// Represents a particular point in a asynchronous traversal hierarchy
template <typename Frame, typename... Hierarchy>
class async_traversal_point {
  Frame frame_;
  std::tuple<Hierarchy...> hierarchy_;
  bool& detached_;

 public:
  explicit async_traversal_point(Frame frame,
                                 std::tuple<Hierarchy...> hierarchy,
                                 bool& detached)
      : frame_(std::move(frame)), hierarchy_(std::move(hierarchy)),
        detached_(detached) {
  }

  // Abort the current control flow
  void detach() noexcept {
    assert(!detached_);
    detached_ = true;
  }

  /// Returns true when we should abort the current control flow
  bool is_detached() const noexcept {
    return detached_;
  }

  /// Creates a new traversal point which
  template <typename Parent>
  auto push(Parent&& parent)
  -> async_traversal_point<Frame, std::decay_t<Parent>, Hierarchy...> {
    // Create a new hierarchy which contains the
    // the parent (the last traversed element).
    auto hierarchy = std::tuple_cat(
        std::make_tuple(std::forward<Parent>(parent)), hierarchy_);

    return async_traversal_point<Frame, typename std::decay<Parent>::type,
                                 Hierarchy...>(frame_, std::move(hierarchy),
                                               detached_);
  }

  /// Forks the current traversal point and continues the child
  /// of the given parent.
  template <typename Child, typename Parent>
  void fork(Child&& child, Parent&& parent) {
    // Push the parent on top of the hierarchy
    auto point = push(std::forward<Parent>(parent));

    // Continue the traversal with the current element
    point.async_traverse(std::forward<Child>(child));
  }

  /// Async traverse a single element, and do nothing.
  /// This function is matched last.
  template <typename Matcher, typename Current>
  void async_traverse_one_impl(Matcher, Current&& /*current*/) {
    // Do nothing if the visitor doesn't accept the type
  }

  /// Async traverse a single element which isn't a container or
  /// tuple like type. This function is SFINAEd out if the element
  /// isn't accepted by the visitor.
  template <typename Current>
  auto async_traverse_one_impl(container_category_tag<false, false>,
                               Current&& current)
  /// SFINAE this out if the visitor doesn't accept
  /// the given element
  -> traits::void_t<decltype(std::declval<Frame>()->traverse(*current))> {
    if (!frame_->traverse(*current)) {
      // Store the current call hierarchy into a tuple for
      // later re-entrance.
      auto hierarchy =
          std::tuple_cat(std::make_tuple(current.next()), hierarchy_);

      // First detach the current execution context
      detach();

      // If the traversal method returns false, we detach the
      // current execution context and call the visitor with the
      // element and a continue callable object again.
      frame_->async_continue(*current, std::move(hierarchy));
    }
  }

  /// Async traverse a single element which is a container or
  /// tuple like type.
  template <bool IsTupleLike, typename Current>
  void async_traverse_one_impl(container_category_tag<true, IsTupleLike>,
                               Current&& current) {
    auto range = make_dynamic_async_range(*current);
    fork(std::move(range), std::forward<Current>(current));
  }

  /// Async traverse a single element which is a tuple like type only.
  template <typename Current>
  void async_traverse_one_impl(container_category_tag<false, true>,
                               Current&& current) {
    auto range = make_static_range(*current);
    fork(std::move(range), std::forward<Current>(current));
  }

  /// Async traverse the current iterator
  template <typename Current>
  void async_traverse_one(Current&& current) {
    using ElementType = typename std::decay<decltype(*current)>::type;
    return async_traverse_one_impl(container_category_of_t<ElementType>{},
                                   std::forward<Current>(current));
  }

  /// Async traverse the current iterator but don't traverse
  /// if the control flow was detached.
  template <typename Current>
  void async_traverse_one_checked(Current&& current) {
    if (!is_detached()) {
      async_traverse_one(std::forward<Current>(current));
    }
  }

  template <std::size_t... Sequence, typename Current>
  void async_traverse_static_async_range(
      std::integer_sequence<std::size_t, Sequence...>, Current&& current) {
    int dummy[] = {0, ((void)async_traverse_one_checked(
        current.template relocate<Sequence>()),
        0)...};
    (void)dummy;
    (void)current;
  }

  /// Traverse a static range
  template <typename Target, std::size_t Begin, std::size_t End>
  void async_traverse(static_async_range<Target, Begin, End> current) {
    async_traverse_static_async_range(
        explicit_range_sequence_of_t<Begin, End>{}, current);
  }

  /// Traverse a dynamic range
  template <typename Begin, typename Sentinel>
  void async_traverse(dynamic_async_range<Begin, Sentinel> range) {
    if (!is_detached()) {
      for (/**/; !range.is_finished(); ++range) {
        async_traverse_one(range);
        if (is_detached()) // test before increment
          break;
      }
    }
  }
};

/// Deduces to the traversal point class of the
/// given frame and hierarchy
template <typename Frame, typename... Hierarchy>
using traversal_point_of_t =
async_traversal_point<typename std::decay<Frame>::type,
                      typename std::decay<Hierarchy>::type...>;

/// A callable object which is capable of resuming an asynchronous
/// pack traversal.
struct resume_state_callable {
  /// Reenter an asynchronous iterator pack and continue
  /// its traversal.
  template <typename Frame, typename Current, typename... Hierarchy>
  void operator()(Frame&& frame, Current&& current,
                  Hierarchy&&... hierarchy) const {
    bool detached = false;
    next(detached, std::forward<Frame>(frame), std::forward<Current>(current),
         std::forward<Hierarchy>(hierarchy)...);
  }

  template <typename Frame, typename Current>
  void next(bool& detached, Frame&& frame, Current&& current) const {
    // Only process the next element if the current iterator
    // hasn't reached its end.
    if (!current.is_finished()) {
      traversal_point_of_t<Frame> point(frame, std::make_tuple(), detached);

      point.async_traverse(std::forward<Current>(current));

      // Don't continue the frame when the execution was detached
      if (detached) {
        return;
      }
    }

    frame->async_complete();
  }

  /// Reenter an asynchronous iterator pack and continue
  /// its traversal.
  template <typename Frame, typename Current, typename Parent,
      typename... Hierarchy>
  void next(bool& detached, Frame&& frame, Current&& current, Parent&& parent,
            Hierarchy&&... hierarchy) const {
    // Only process the element if the current iterator
    // hasn't reached its end.
    if (!current.is_finished()) {
      // Don't forward the arguments here, since we still need
      // the objects in a valid state later.
      traversal_point_of_t<Frame, Parent, Hierarchy...> point(
          frame, std::make_tuple(parent, hierarchy...), detached);

      point.async_traverse(std::forward<Current>(current));

      // Don't continue the frame when the execution was detached
      if (detached) {
        return;
      }
    }

    // Pop the top element from the hierarchy, and shift the
    // parent element one to the right
    next(detached, std::forward<Frame>(frame),
         std::forward<Parent>(parent).next(),
         std::forward<Hierarchy>(hierarchy)...);
  }
};

template <typename Frame, typename State>
void resume_traversal_callable<Frame, State>::operator()() {
  auto hierarchy = std::tuple_cat(std::make_tuple(frame_), state_);
  traits::unpack(std::move(hierarchy), resume_state_callable{});
}

/// Gives access to types related to the traversal frame
template <typename Visitor, typename... Args>
struct async_traversal_types {
  /// Deduces to the async traversal frame type of the given
  /// traversal arguments and mapper
  using frame_t = async_traversal_frame<typename std::decay<Visitor>::type,
                                        typename std::decay<Args>::type...>;

  /// The type of the demoted visitor type
  using visitor_t = Visitor;
};

template <typename Visitor, typename VisitorArg, typename... Args>
struct async_traversal_types<async_traverse_in_place_tag<Visitor>, VisitorArg,
                             Args...>
    : async_traversal_types<Visitor, Args...> {};

/// Traverses the given pack with the given mapper
template <typename Visitor, typename... Args>
auto apply_pack_transform_async(Visitor&& visitor, Args&&... args) {

  // Provide the frame and visitor type
  using types = async_traversal_types<Visitor, Args...>;
  using frame_t = typename types::frame_t;
  using visitor_t = typename types::visitor_t;

  // Check whether the visitor inherits enable_shared_from_this
  static_assert(std::is_base_of<std::enable_shared_from_this<visitor_t>,
                                visitor_t>::value,
                "The visitor must inherit std::enable_shared_from_this!");

  // Check whether the visitor is virtual destructible
  static_assert(std::has_virtual_destructor<visitor_t>::value,
                "The visitor must have a virtual destructor!");

  // Create the frame on the heap which stores the arguments
  // to traverse asynchronous. It persists until the
  // traversal frame isn't referenced anymore.
  auto frame = std::make_shared<frame_t>(std::forward<Visitor>(visitor),
                                         std::forward<Args>(args)...);

  // Create a static range for the top level tuple
  auto range = std::make_tuple(make_static_range(frame->head()));

  // Create a resumer to start the asynchronous traversal
  auto resumer = make_resume_traversal_callable(frame, std::move(range));

  // Start the asynchronous traversal
  resumer();

  // Cast the shared_ptr down to the given visitor type
  // for implementation invisibility
  return std::static_pointer_cast<visitor_t>(std::move(frame));
}
} // namespace traversal
} // namespace detail
} // namespace cti

#endif // CONTINUABLE_DETAIL_TRAVERSE_ASYNC_HPP_INCLUDED


namespace cti {
/// \defgroup Traversal Traversal
/// provides functions to traverse and remap nested packs.
/// \{

/// A tag which is passed to the `operator()` of the visitor
/// if an element is visited synchronously through \ref traverse_pack_async.
///
/// \since 3.0.0
using async_traverse_visit_tag = detail::traversal::async_traverse_visit_tag;
/// A tag which is passed to the `operator()` of the visitor if an element is
/// visited after the traversal was detached through \ref traverse_pack_async.
///
/// \since 3.0.0
using async_traverse_detach_tag = detail::traversal::async_traverse_detach_tag;
/// A tag which is passed to the `operator()` of the visitor if the
/// asynchronous pack traversal was finished through \ref traverse_pack_async.
///
/// \since 3.0.0
using async_traverse_complete_tag =
detail::traversal::async_traverse_complete_tag;

/// A tag to identify that a mapper shall be constructed in-place
/// from the first argument passed to \ref traverse_pack_async.
///
/// \since 3.0.0
template <typename T>
using async_traverse_in_place_tag =
detail::traversal::async_traverse_in_place_tag<T>;

/// Traverses the pack with the given visitor in an asynchronous way.
///
/// This function works in the same way as `traverse_pack`,
/// however, we are able to suspend and continue the traversal at
/// later time.
/// Thus we require a visitor callable object which provides three
/// `operator()` overloads as depicted by the code sample below:
///    ```cpp
///    struct my_async_visitor {
///      /// The synchronous overload is called for each object,
///      /// it may return false to suspend the current control.
///      /// In that case the overload below is called.
///      template <typename T>
///      bool operator()(async_traverse_visit_tag, T&& element) {
///        return true;
///      }
///
///      /// The asynchronous overload this is called when the
///      /// synchronous overload returned false.
///      /// In addition to the current visited element the overload is
///      /// called with a contnuation callable object which resumes the
///      /// traversal when it's called later.
///      /// The continuation next may be stored and called later or
///      /// dropped completely to abort the traversal early.
///      template <typename T, typename N>
///      void operator()(async_traverse_detach_tag, T&& element, N&& next) {
///      }
///
///      /// The overload is called when the traversal was finished.
///      /// As argument the whole pack is passed over which we
///      /// traversed asynchrnously.
///      template <typename T>
///      void operator()(async_traverse_complete_tag, T&& pack) {
///      }
///    };
///    ```
///
/// \param   visitor A visitor object which provides the three `operator()`
///                  overloads that were described above.
///                  Additionally the visitor must be compatible
///                  for referencing it from a `boost::intrusive_ptr`.
///                  The visitor should must have a virtual destructor!
///
/// \param   pack    The arbitrary parameter pack which is traversed
///                  asynchronously. Nested objects inside containers and
///                  tuple like types are traversed recursively.
///
/// \returns         A std::shared_ptr that references an instance of
///                  the given visitor object.
///
/// \since           3.0.0
///
/// See `traverse_pack` for a detailed description about the
/// traversal behaviour and capabilities.
///
template <typename Visitor, typename... T>
auto traverse_pack_async(Visitor&& visitor, T&&... pack) {
  return detail::traversal::apply_pack_transform_async(
      std::forward<Visitor>(visitor), std::forward<T>(pack)...);
}
/// \}
} // namespace cti

#endif // CONTINUABLE_TRAVERSE_ASYNC_HPP_INCLUDED

// #include <continuable/detail/base.hpp>

// #include <continuable/detail/connection-aggregated.hpp>

// #include <continuable/detail/traits.hpp>

// #include <continuable/detail/util.hpp>


namespace cti {
namespace detail {
namespace connection {
namespace seq {
/// Connects the left and the right continuable to a sequence
///
/// \note This is implemented in an eager way because we would not gain
///       any profit from chaining sequences lazily.
template <typename Left, typename Right>
auto sequential_connect(Left&& left, Right&& right) {
  left.freeze(right.is_frozen());
  right.freeze();

  return std::forward<Left>(left).then([right = std::forward<Right>(right)](
      auto&&... args) mutable {
    return std::move(right).then([previous = std::make_tuple(
        std::forward<decltype(args)>(args)...)](
        auto&&... args) mutable {
      return traits::merge(
          std::move(previous),
          std::make_tuple(std::forward<decltype(args)>(args)...));
    });
  });
}

template <typename Callback, typename Box>
struct sequential_dispatch_data {
  Callback callback;
  Box box;
};

template <typename Data>
class sequential_dispatch_visitor
    : public std::enable_shared_from_this<sequential_dispatch_visitor<Data>>,
      public util::non_movable {

  Data data_;

 public:
  explicit sequential_dispatch_visitor(Data&& data) : data_(std::move(data)) {
  }

  virtual ~sequential_dispatch_visitor() = default;

  /// Returns the pack that should be traversed
  auto& head() {
    return data_.box;
  }

  template <typename Box, std::enable_if_t<aggregated::is_continuable_box<
      std::decay_t<Box>>::value>* = nullptr>
  bool operator()(async_traverse_visit_tag, Box&& /*box*/) {
    return false;
  }

  template <typename Box, typename N>
  void operator()(async_traverse_detach_tag, Box&& box, N&& next) {
    box.fetch()
        .then([ box = std::addressof(box),
                  next = std::forward<N>(next) ](auto&&... args) mutable {

          // Assign the result to the target
          box->assign(std::forward<decltype(args)>(args)...);

          // Continue the asynchronous sequential traversal
          next();
        })
        .fail([me = this->shared_from_this()](types::error_type exception) {
          // Abort the traversal when an error occurred
          std::move(me->data_.callback)(types::dispatch_error_tag{},
                                        std::move(exception));
        })
        .done();
  }

  template <typename T>
  void operator()(async_traverse_complete_tag, T&& /*pack*/) {
    return aggregated::finalize_data(std::move(data_.callback),
                                     std::move(data_.box));
  }
};
} // namespace seq

struct connection_strategy_seq_tag {};
template <>
struct is_connection_strategy<connection_strategy_seq_tag> // ...
    : std::true_type {};

/// Finalizes the seq logic of a given connection
template <>
struct connection_finalizer<connection_strategy_seq_tag> {
  /// Finalizes the all logic of a given connection
  template <typename Connection>
  static auto finalize(Connection&& connection, util::ownership ownership) {

    auto result =
        aggregated::box_continuables(std::forward<Connection>(connection));

    auto signature = aggregated::hint_of_data<decltype(result)>();

    return base::attorney::create(
        [result = std::move(result)](auto&& callback) mutable {

          // The data from which the visitor is constructed in-place
          using data_t =
          seq::sequential_dispatch_data<std::decay_t<decltype(callback)>,
                                        std::decay_t<decltype(result)>>;

          // The visitor type
          using visitor_t = seq::sequential_dispatch_visitor<data_t>;

          traverse_pack_async(async_traverse_in_place_tag<visitor_t>{},
                              data_t{std::forward<decltype(callback)>(callback),
                                     std::move(result)});
        },
        signature, std::move(ownership));
  }
};
} // namespace connection
} // namespace detail
} // namespace cti

#endif // CONTINUABLE_DETAIL_CONNECTION_SEQ_HPP_INCLUDED

// #include <continuable/detail/connection.hpp>

// #include <continuable/detail/features.hpp>

// #include <continuable/detail/traits.hpp>

// #include <continuable/detail/types.hpp>

// #include <continuable/detail/util.hpp>


#ifdef CONTINUABLE_HAS_EXPERIMENTAL_COROUTINE
// #include <continuable/detail/awaiting.hpp>

/*

                        /~` _  _ _|_. _     _ |_ | _
                        \_,(_)| | | || ||_|(_||_)|(/_

                    https://github.com/Naios/continuable
                                   v3.0.0

  Copyright(c) 2015 - 2018 Denis Blank <denis.blank at outlook dot com>

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files(the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and / or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions :

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
**/

// Exclude this header when coroutines are not available
#ifndef CONTINUABLE_DETAIL_AWAITING_HPP_INCLUDED
#define CONTINUABLE_DETAIL_AWAITING_HPP_INCLUDED

#include <cassert>
#include <experimental/coroutine>

// #include <continuable/detail/expected.hpp>

/*

                        /~` _  _ _|_. _     _ |_ | _
                        \_,(_)| | | || ||_|(_||_)|(/_

                    https://github.com/Naios/continuable
                                   v3.0.0

  Copyright(c) 2015 - 2018 Denis Blank <denis.blank at outlook dot com>

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files(the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and / or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions :

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
**/

#ifndef CONTINUABLE_DETAIL_EXPECTED_HPP_INCLUDED
#define CONTINUABLE_DETAIL_EXPECTED_HPP_INCLUDED

#include <type_traits>
#include <utility>

// #include <continuable/detail/flat-variant.hpp>

// #include <continuable/detail/hints.hpp>

// #include <continuable/detail/types.hpp>


namespace cti {
namespace detail {
namespace container {
/// A class similar to the one in the expected proposal,
/// however it is capable of carrying an exception_ptr if
/// exceptions are used.
template <typename T>
class expected {
  flat_variant<T, types::error_type> variant_;

public:
  explicit expected() = default;
  explicit expected(expected const&) = default;
  explicit expected(expected&&) = default;
  expected& operator=(expected const&) = default;
  expected& operator=(expected&&) = default;
  ~expected() = default;

  explicit expected(T value) : variant_(std::move(value)) {
  }
  explicit expected(types::error_type exception)
      : variant_(std::move(exception)) {
  }

  expected& operator=(T value) {
    variant_ = std::move(value);
    return *this;
  }
  expected& operator=(types::error_type exception) {
    variant_ = std::move(exception);
    return *this;
  }

  void set_value(T value) {
    variant_ = std::move(value);
  }
  void set_exception(types::error_type exception) {
    variant_ = std::move(exception);
  }

  bool is_value() const noexcept {
    return variant_.template is<T>();
  }
  bool is_exception() const noexcept {
    return variant_.template is<types::error_type>();
  }

  explicit constexpr operator bool() const noexcept {
    return is_value();
  }

  T& get_value() noexcept {
    return variant_.template cast<T>();
  }
  T const& get_value() const noexcept {
    return variant_.template cast<T>();
  }
  types::error_type& get_exception() noexcept {
    return variant_.template cast<types::error_type>();
  }
  types::error_type const& get_exception() const noexcept {
    return variant_.template cast<types::error_type>();
  }

  T& operator*() noexcept {
    return get_value();
  }
  T const& operator*() const noexcept {
    return get_value();
  }
};

namespace detail {
struct void_guard_tag {};

template <typename T>
struct expected_result_trait;
template <>
struct expected_result_trait<traits::identity<>> {
  using expected_type = expected<void_guard_tag>;

  static constexpr void_guard_tag wrap() noexcept {
    return {};
  }
  static void unwrap(expected_type&& e) {
    assert(e.is_value());
    (void)e;
  }
};
template <typename T>
struct expected_result_trait<traits::identity<T>> {
  using expected_type = expected<T>;

  static auto wrap(T arg) {
    return std::move(arg);
  }
  static auto unwrap(expected_type&& e) {
    assert(e.is_value());
    return std::move(e.get_value());
  }
};
template <typename First, typename Second, typename... Rest>
struct expected_result_trait<traits::identity<First, Second, Rest...>> {
  using expected_type = expected<std::tuple<First, Second, Rest...>>;

  static auto wrap(First first, Second second, Rest... rest) {
    return std::make_tuple(std::move(first), std::move(second),
                           std::move(rest)...);
  }
  static auto unwrap(expected_type&& e) {
    assert(e.is_value());
    return std::move(e.get_value());
  }
};
} // namespace detail

template <typename Continuable>
using expected_result_trait_t = detail::expected_result_trait<decltype(
    hints::hint_of(traits::identify<Continuable>{}))>;
} // namespace container
} // namespace detail
} // namespace cti

#endif // CONTINUABLE_DETAIL_EXPECTED_HPP_INCLUDED

// #include <continuable/detail/features.hpp>

// #include <continuable/detail/hints.hpp>

// #include <continuable/detail/types.hpp>

// #include <continuable/detail/util.hpp>


#if defined(CONTINUABLE_HAS_EXCEPTIONS)
#include <exception>
#endif // CONTINUABLE_HAS_EXCEPTIONS

namespace cti {
namespace detail {
namespace awaiting {
/// We import the coroutine handle in our namespace
using std::experimental::coroutine_handle;

/// An object which provides the internal buffer and helper methods
/// for waiting on a continuable in a stackless coroutine.
template <typename Continuable>
class awaitable {
  using trait_t = container::expected_result_trait_t<Continuable>;

  /// The continuable which is invoked upon suspension
  Continuable continuable_;
  /// A cache which is used to pass the result of the continuation
  /// to the coroutine.
  typename trait_t::expected_type result_;

public:
  explicit constexpr awaitable(Continuable&& continuable)
      : continuable_(std::move(continuable)) {
  }

  /// Since continuables are evaluated lazily we are not
  /// capable to say whether the resumption will be instantly.
  bool await_ready() const noexcept {
    return false;
  }

  /// Suspend the current context
  // TODO Convert this to an r-value function once possible
  void await_suspend(coroutine_handle<> h) {
    // Forward every result to the current awaitable
    std::move(continuable_)
        .next([h, this](auto&&... args) mutable {
          resolve(std::forward<decltype(args)>(args)...);
          h.resume();
        })
        .done();
  }

  /// Resume the coroutine represented by the handle
  auto await_resume() noexcept(false) {
    if (result_) {
      // When the result was resolved return it
      return trait_t::unwrap(std::move(result_));
    }

#if defined(CONTINUABLE_HAS_EXCEPTIONS)
    std::rethrow_exception(result_.get_exception());
#else  // CONTINUABLE_HAS_EXCEPTIONS
    // Returning error types in await isn't supported as of now
    util::trap();
#endif // CONTINUABLE_HAS_EXCEPTIONS
  }

private:
  /// Resolve the continuation through the result
  template <typename... Args>
  void resolve(Args&&... args) {
    result_.set_value(trait_t::wrap(std::forward<Args>(args)...));
  }

  /// Resolve the continuation through an error
  void resolve(types::dispatch_error_tag, types::error_type error) {
    result_.set_exception(std::move(error));
  }
};

/// Converts a continuable into an awaitable object as described by
/// the C++ coroutine TS.
template <typename T>
constexpr auto create_awaiter(T&& continuable) {
  return awaitable<std::decay_t<T>>(std::forward<T>(continuable));
}
} // namespace awaiting
} // namespace detail
} // namespace cti

// As far as I know there is no other was to implement this specialization...
// NOLINTNEXTLINE(cert-dcl58-cpp)
namespace std {
namespace experimental {
template <typename Data, typename... Args, typename... FunctionArgs>
struct coroutine_traits<
    cti::continuable_base<Data,
                          cti::detail::hints::signature_hint_tag<Args...>>,
    FunctionArgs...> {

  static_assert(cti::detail::traits::fail<Data>::value,
                "Using a continuable as return type from co_return "
                "expressions isn't supported yet!");
};
} // namespace experimental
} // namespace std

#endif // CONTINUABLE_DETAIL_UTIL_HPP_INCLUDED

#endif // CONTINUABLE_HAS_EXPERIMENTAL_COROUTINE

namespace cti {
/// \defgroup Base Base
/// provides classes and functions to create continuable_base objects.
/// \{

/// Represents a tag which can be placed first in a signature
/// in order to overload callables with the asynchronous result
/// as well as an error.
///
/// See the example below:
/// ```cpp
/// struct my_callable {
///   void operator() (std::string result) {
///     // ...
///   }
///   void operator() (cti::dispatch_error_tag, cti::error_type) {
///     // ...
///   }
/// };
///
/// // Will receive errors and results
/// continuable.next(my_callable{});
/// ```
///
/// \note see continuable::next for details.
///
/// \since 2.0.0
using dispatch_error_tag = detail::types::dispatch_error_tag;

/// Represents the type that is used as error type
///
/// By default this type deduces to `std::exception_ptr`.
/// If `CONTINUABLE_WITH_NO_EXCEPTIONS` is defined the type
/// will be a `std::error_condition`.
/// A custom error type may be set through
/// defining `CONTINUABLE_WITH_CUSTOM_ERROR_TYPE`.
///
/// \since 2.0.0
using error_type = detail::types::error_type;

/// Deduces to a true_type if the given type is a continuable_base.
///
/// \since 3.0.0
template <typename T>
using is_continuable = detail::base::is_continuable<T>;

/// The main class of the continuable library, it provides the functionality
/// for chaining callbacks and continuations together to a unified hierarchy.
///
/// The most important method is the cti::continuable_base::then() method,
/// which allows to attach a callback to the continuable.
///
/// Use the continuable types defined in `continuable/continuable.hpp`,
/// in order to use this class.
///
/// \tparam Data The internal data which is used to store the current
///         continuation and intermediate lazy connection result.
///
/// \tparam Annotation The internal data used to store the current signature
///         hint or strategy used for combining lazy connections.
///
/// \note Nearly all methods of the cti::continuable_base are required to be
///       called as r-value. This is required because the continuable carries
///       variables which are consumed when the object is transformed as part
///       of a method call.
///
/// \attention The continuable_base objects aren't intended to be stored.
///            If you want to store a continuble_base you should always
///            call the continuable_base::freeze method for disabling the
///            invocation on destruction.
///
/// \since 1.0.0
template <typename Data, typename Annotation>
class continuable_base {

  /// \cond false
  template <typename, typename>
  friend class continuable_base;
  friend struct detail::base::attorney;

  // The continuation type or intermediate result
  Data data_;
  // The transferable state which represents the validity of the object
  detail::util::ownership ownership_;
  /// \endcond

  /// Constructor accepting the data object while erasing the annotation
  explicit continuable_base(Data data, detail::util::ownership ownership)
      : data_(std::move(data)), ownership_(std::move(ownership)) {
  }

 public:
  /// Constructor accepting the data object while erasing the annotation
  explicit continuable_base(Data data) : data_(std::move(data)) {
  }

  /// Constructor accepting any object convertible to the data object,
  /// while erasing the annotation
  template <typename OData, std::enable_if_t<std::is_convertible<
      std::decay_t<OData>, Data>::value>* = nullptr>
  continuable_base(OData&& data) : data_(std::forward<OData>(data)) {
  }

  /// Constructor taking the data of other continuable_base objects
  /// while erasing the hint.
  ///
  /// This constructor makes it possible to replace the internal data object of
  /// the continuable by any object which is useful for type-erasure.
  template <typename OData, typename OAnnotation>
  continuable_base(continuable_base<OData, OAnnotation>&& other)
      : continuable_base(std::move(other).materialize().consume_data()) {
  }

  /// \cond false
  continuable_base(continuable_base&&) = default;
  continuable_base(continuable_base const&) = delete;

  continuable_base& operator=(continuable_base&&) = default;
  continuable_base& operator=(continuable_base const&) = delete;
  /// \endcond

  /// The destructor automatically invokes the continuable_base
  /// if it wasn't consumed yet.
  ///
  /// In order to invoke the continuable early you may call the
  /// continuable_base::done() method.
  ///
  /// The continuable_base::freeze method disables the automatic
  /// invocation on destruction without invalidating the object.
  ///
  /// \since 1.0.0
  ~continuable_base() {
    if (ownership_.is_acquired() && !ownership_.is_frozen()) {
      std::move(*this).done();
    }
    assert((!ownership_.is_acquired() || ownership_.is_frozen()) &&
        "Ownership should be released!");
  }

  /// Main method of the continuable_base to chain the current continuation
  /// with a new callback.
  ///
  /// \param callback The callback which is used to process the current
  ///        asynchronous result on arrival. The callback is required to accept
  ///        the current result at least partially (or nothing of the result).
  /// ```cpp
  /// (http_request("github.com") && http_request("atom.io"))
  ///   .then([](std::string github, std::string atom) {
  ///     // We use the whole result
  ///   });
  ///
  /// (http_request("github.com") && http_request("atom.io"))
  ///   .then([](std::string github) {
  ///     // We only use the result partially
  ///   });
  ///
  /// (http_request("github.com") && http_request("atom.io"))
  ///   .then([] {
  ///     // We discard the result
  ///   });
  /// ```
  ///
  /// \param executor The optional executor which is used to dispatch
  ///        the callback. The executor needs to accept callable objects
  ///        callable through an `operator()` through its operator() itself.
  ///        The executor can be move-only, but it's not required to.
  ///        The default executor which is used when omitting the argument
  ///        dispatches the callback on the current executing thread.
  ///        Consider the example shown below:
  /// ```cpp
  /// auto executor = [](auto&& work) {
  ///   // Dispatch the work here or forward it to an executor of
  ///   // your choice.
  ///   std::forward<decltype(work)>(work)();
  /// };
  ///
  /// http_request("github.com")
  ///   .then([](std::string github) {
  ///     // Do something...
  ///    }, executor);
  /// ```
  ///
  /// \returns Returns a continuable_base with an asynchronous return type
  ///          depending on the return value of the callback:
  /// |      Callback returns      |              Resulting type               |
  /// | : ---------------------- : | : --------------------------------------- |
  /// | `void`                     | `continuable_base with <>`                |
  /// | `Arg`                      | `continuable_base with <Arg>`             |
  /// | `std::pair<First, Second>` | `continuable_base with <First, Second>`   |
  /// | `std::tuple<Args...>`      | `continuable_base with <Args...>`         |
  /// | `continuable_base<Arg...>` | `continuable_base with <Args...>`         |
  ///          Which means the result type of the continuable_base is equal to
  ///          the plain types the callback returns (`std::tuple` and
  ///          `std::pair` arguments are unwrapped).
  ///          A single continuable_base as argument is resolved and the result
  ///          type is equal to the resolved continuable_base.
  ///          Consider the following examples:
  /// ```cpp
  /// http_request("github.com")
  ///   .then([](std::string github) { return; })
  ///   .then([] { }); // <void>
  ///
  /// http_request("github.com")
  ///   .then([](std::string github) { return 0; })
  ///   .then([](int a) { }); // <int>
  ///
  /// http_request("github.com")
  ///   .then([](std::string github) { return std::make_pair(1, 2); })
  ///   .then([](int a, int b) { }); // <int, int>
  ///
  /// http_request("github.com")
  ///   .then([](std::string github) { return std::make_tuple(1, 2, 3); })
  ///   .then([](int a, int b, int c) { }); // <int, int, int>
  ///
  /// http_request("github.com")
  ///   .then([](std::string github) { return http_request("atom.io"); })
  ///   .then([](std::string atom) { }); // <std::string>
  /// ```
  ///
  /// \since 1.0.0
  template <typename T, typename E = detail::types::this_thread_executor_tag>
  auto then(T&& callback,
            E&& executor = detail::types::this_thread_executor_tag{}) && {
    return detail::base::chain_continuation<detail::base::handle_results::yes,
                                            detail::base::handle_errors::no>(
        std::move(*this).materialize(), std::forward<T>(callback),
        std::forward<E>(executor));
  }

  /// Additional overload of the continuable_base::then() method
  /// which is accepting a continuable_base itself.
  ///
  /// \param continuation A continuable_base reflecting the continuation
  ///        which is used to continue the call hierarchy.
  ///        The result of the current continuable is discarded and the given
  ///        continuation is invoked as shown below.
  /// ```cpp
  /// http_request("github.com")
  ///   .then(http_request("atom.io"))
  ///   .then([](std::string atom) {
  ///     // ...
  ///   });
  /// ```
  ///
  /// \returns Returns a continuable_base representing the next asynchronous
  ///          result to continue within the asynchronous call hierarchy.
  ///
  /// \since 1.0.0
  template <typename OData, typename OAnnotation>
  auto then(continuable_base<OData, OAnnotation>&& continuation) && {
    return std::move(*this).then(
        detail::base::wrap_continuation(std::move(continuation).materialize()));
  }

  /// Main method of the continuable_base to catch exceptions and error codes
  /// in case the asynchronous control flow failed and was resolved
  /// through an error code or exception.
  ///
  /// \param callback The callback which is used to process the current
  ///        asynchronous error result on arrival.
  ///        In case the continuable_base is using exceptions,
  ///        the usage is as shown below:
  ///
  /// ```cpp
  /// http_request("github.com")
  ///   .then([](std::string github) { })
  ///   .fail([](std::exception_ptr ptr) {
  ///     // Handle the error here
  ///     try {
  ///       std::rethrow_exception(ptr);
  ///     } catch (std::exception& e) {
  ///       e.what(); // Handle the exception
  ///     }
  ///   });
  /// ```
  ///        In case exceptions are disabled, `std::error_condition` is
  ///        used as error result instead of `std::exception_ptr`.
  /// ```cpp
  /// http_request("github.com")
  ///   .then([](std::string github) { })
  ///   .fail([](std::error_condition error) {
  ///     error.message(); // Handle the error here
  ///   });
  /// ```
  ///
  /// \param executor The optional executor which is used to dispatch
  ///        the callback. See the description in `then` above.
  ///
  /// \returns Returns a continuable_base with an asynchronous return type
  ///          depending on the previous result type.
  ///
  ///
  /// \since 2.0.0
  template <typename T, typename E = detail::types::this_thread_executor_tag>
  auto fail(T&& callback,
            E&& executor = detail::types::this_thread_executor_tag{}) && {
    return detail::base::chain_continuation<detail::base::handle_results::no,
                                            detail::base::handle_errors::plain>(
        std::move(*this).materialize(), std::forward<T>(callback),
        std::forward<E>(executor));
  }

  /// Additional overload of the continuable_base::fail() method
  /// which is accepting a continuable_base itself.
  ///
  /// \param continuation A continuable_base reflecting the continuation
  ///        which is used to continue the call hierarchy on errors.
  ///        The result of the current continuable is discarded and the given
  ///        continuation is invoked as shown below.
  /// ```cpp
  /// http_request("github.com")
  ///   .fail(http_request("atom.io"))
  /// ```
  ///
  /// \returns Returns a continuable_base with an asynchronous return type
  ///          depending on the previous result type.
  ///
  /// \since 2.0.0
  template <typename OData, typename OAnnotation>
  auto fail(continuable_base<OData, OAnnotation>&& continuation) && {
    continuation.freeze();
    return std::move(*this).fail([continuation = std::move(continuation)](
        error_type) mutable { std::move(continuation).done(); });
  }

  /// A method which allows to use an overloaded callable for the error
  /// as well as the valid result path.
  ///
  /// \param callback The callback which is used to process the current
  ///        asynchronous result and error on arrival.
  ///
  /// ```cpp
  /// struct my_callable {
  ///   void operator() (std::string result) {
  ///     // ...
  ///   }
  ///   void operator() (cti::dispatch_error_tag, cti::error_type) {
  ///     // ...
  ///   }
  ///
  /// // Will receive errors and results
  /// http_request("github.com")
  ///   .next(my_callable{});
  /// ```
  ///
  /// \param executor The optional executor which is used to dispatch
  ///        the callback. See the description in `then` above.
  ///
  /// \returns Returns a continuable_base with an asynchronous return type
  ///          depending on the current result type.
  ///
  /// \since 2.0.0
  template <typename T, typename E = detail::types::this_thread_executor_tag>
  auto next(T&& callback,
            E&& executor = detail::types::this_thread_executor_tag{}) && {
    return detail::base::chain_continuation<
        detail::base::handle_results::yes,
        detail::base::handle_errors::forward>(std::move(*this).materialize(),
                                              std::forward<T>(callback),
                                              std::forward<E>(executor));
  }

  /// A method which allows to apply this continuable to the given callable.
  ///
  /// \param transform A transform which shall accept this continuable
  ///
  /// \returns Returns the result of the given transform when this
  ///          continuable is passed into it.
  ///
  /// \since 2.0.0
  template <typename T>
  auto apply(T&& transform) && {
    return std::forward<T>(transform)(std::move(*this).materialize());
  }

  /// The pipe operator | is an alias for the continuable::then method.
  ///
  /// \param right The argument on the right-hand side to connect.
  ///
  /// \returns See the corresponding continuable_base::then method for the
  ///          explanation of the return type.
  ///
  /// \since 2.0.0
  template <typename T>
  auto operator|(T&& right) && {
    return std::move(*this).then(std::forward<T>(right));
  }

  /// The pipe operator | is an alias for the continuable::apply method.
  ///
  /// \param transform The transformer which is applied.
  ///
  /// \returns See the corresponding continuable_base::apply method for the
  ///          explanation of the return type.
  ///
  /// \note    You may create your own transformation through
  ///          calling make_transformation.
  ///
  /// \since 3.0.0
  template <typename T>
  auto operator|(detail::types::transform<T> transform) && {
    return std::move(*this).apply(std::move(transform));
  }

  /// Invokes both continuable_base objects parallel and calls the
  /// callback with the result of both continuable_base objects.
  ///
  /// \param right The continuable on the right-hand side to connect.
  ///
  /// \returns Returns a continuable_base with a result type matching
  ///          the result of the left continuable_base combined with the
  ///          right continuable_base.
  ///          The returned continuable_base will be in an intermediate lazy
  ///          state, further calls to its continuable_base::operator &&
  ///          will add other continuable_base objects to the current
  ///          invocation chain.
  /// ```cpp
  /// (http_request("github.com") && http_request("atom.io"))
  ///   .then([](std::string github, std::string atom) {
  ///     // ...
  ///   });
  ///
  /// auto request = http_request("github.com") && http_request("atom.io");
  /// (std::move(request) && http_request("travis-ci.org"))
  ///    // All three requests are invoked in parallel although we added
  ///    // the request to "travis-ci.org" last.
  ///   .then([](std::string github, std::string atom, std::string travis) {
  ///     // ...
  ///   });
  /// ```
  ///
  /// \note The continuable_base objects are invoked all at onve,
  ///       because the `all` strategy tries to resolve
  ///       the continuations as fast as possible.
  ///       Sequential invocation is also supported through the
  ///       continuable_base::operator>> method.
  ///
  /// \since 1.0.0
  template <typename OData, typename OAnnotation>
  auto operator&&(continuable_base<OData, OAnnotation>&& right) && {
    return detail::connection::connect(
        detail::connection::connection_strategy_all_tag{}, std::move(*this),
        std::move(right));
  }

  /// Invokes both continuable_base objects parallel and calls the
  /// callback once with the first result available.
  ///
  /// \param right The continuable on the right-hand side to connect.
  ///              The right continuable is required to have the same
  ///              result as the left connected continuable_base.
  ///
  /// \returns Returns a continuable_base with a result type matching
  ///          the combined result which of all connected
  ///          continuable_base objects.
  ///          The returned continuable_base will be in an intermediate lazy
  ///          state, further calls to its continuable_base::operator ||
  ///          will add other continuable_base objects to the current
  ///          invocation chain.
  /// ```cpp
  /// (http_request("github.com") || http_request("atom.io"))
  ///   .then([](std::string github_or_atom) {
  ///     // ...
  ///   });
  ///
  /// (make_ready_continuable(10, 'A') || make_ready_continuable(29, 'B'))
  ///   .then([](int a, char b) {
  ///     // ...
  ///   });
  /// ```
  ///
  /// \note The continuable_base objects are invoked all at once,
  ///       however, the callback is only called once with
  ///       the first result or exception which becomes available.
  ///
  /// \since 1.0.0
  template <typename OData, typename OAnnotation>
  auto operator||(continuable_base<OData, OAnnotation>&& right) && {
    return detail::connection::connect(
        detail::connection::connection_strategy_any_tag{}, std::move(*this),
        std::move(right));
  }

  /// Invokes both continuable_base objects sequential and calls the
  /// callback with the result of both continuable_base objects.
  ///
  /// \param right The continuable on the right-hand side to connect.
  ///
  /// \returns Returns a continuable_base with a result type matching
  ///          the result of the left continuable_base combined with the
  ///          right continuable_base.
  /// ```cpp
  /// (http_request("github.com") >> http_request("atom.io"))
  ///   .then([](std::string github, std::string atom) {
  ///     // The callback is called with the result of both requests,
  ///     // however, the request to atom was started after the request
  ///     // to github was finished.
  ///   });
  /// ```
  ///
  /// \note The continuable_base objects are invoked sequential one after
  ///       the previous one was finished. Parallel invocation is also
  ///       supported through the continuable_base::operator && method.
  ///
  /// \since 1.0.0
  template <typename OData, typename OAnnotation>
  auto operator>>(continuable_base<OData, OAnnotation>&& right) && {
    return detail::connection::seq::sequential_connect(std::move(*this),
                                                       std::move(right));
  }

  /// Invokes the continuation chain manually even before the
  /// cti::continuable_base is destructed. This will release the object.
  ///
  /// \see continuable_base::~continuable_base() for further details about
  ///      the continuation invocation on destruction.
  ///
  /// \attention This method will trigger an assertion if the
  ///            continuable_base was released already.
  ///
  /// \since 1.0.0
  void done() && {
    detail::base::finalize_continuation(std::move(*this));
  }

  /// Predicate to check whether the cti::continuable_base is frozen or not.
  ///
  /// \returns Returns true when the continuable_base is frozen.
  ///
  /// \see continuable_base::freeze for further details.
  ///
  /// \attention This method will trigger an assertion if the
  ///            continuable_base was released already.
  ///
  /// \since 1.0.0
  bool is_frozen() const noexcept {
    assert_acquired();
    return ownership_.is_frozen();
  }

  /// Prevents the automatic invocation of the continuation chain
  /// which happens on destruction of the continuable_base.
  /// You may still invoke the chain through the continuable_base::done method.
  ///
  /// This is useful for storing a continuable_base inside a continuation
  /// chain while storing it for further usage.
  ///
  /// \param enabled Indicates whether the freeze is enabled or disabled.
  ///
  /// \see continuable_base::~continuable_base() for further details about
  ///      the continuation invocation on destruction.
  ///
  /// \attention This method will trigger an assertion if the
  ///            continuable_base was released already.
  ///
  /// \since 1.0.0
  continuable_base& freeze(bool enabled = true) & noexcept {
    ownership_.freeze(enabled);
    return *this;
  }

  /// \copydoc continuable_base::freeze
  continuable_base&& freeze(bool enabled = true) && noexcept {
    ownership_.freeze(enabled);
    return std::move(*this);
  }

  /// \cond false
#ifdef CONTINUABLE_HAS_EXPERIMENTAL_COROUTINE
  /// \endcond
  /// Implements the operator for awaiting on continuables using `co_await`.
  ///
  /// The operator is only enabled if `CONTINUABLE_HAS_EXPERIMENTAL_COROUTINE`
  /// is defined and the toolchain supports experimental coroutines.
  ///
  /// The return type of the `co_await` expression is specified as following:
  /// |          Continuation type        |          co_await returns          |
  /// | : ------------------------------- | : -------------------------------- |
  /// | `continuable_base with <>`        | `void`                             |
  /// | `continuable_base with <Arg>`     | `Arg`                              |
  /// | `continuable_base with <Args...>` | `std::tuple<Args...>`              |
  ///
  /// When exceptions are used the usage is as intuitive as shown below:
  /// ```cpp
  /// // Handling the exception isn't required and
  /// // the try catch clause may be omitted.
  /// try {
  ///   std::string response = co_await http_request("github.com");
  /// } (std::exception& e) {
  ///   e.what();
  /// }
  /// ```
  ///
  /// In case the library is configured to use error codes or a custom
  /// error type the return type of the co_await expression is changed.
  /// The result is returned through an internal proxy object which may
  /// be queried for the error object.
  /// |          Continuation type        |          co_await returns          |
  /// | : ------------------------------- | : -------------------------------- |
  /// | `continuable_base with <>`        | `unspecified<void>`                |
  /// | `continuable_base with <Arg>`     | `unspecified<Arg>`                 |
  /// | `continuable_base with <Args...>` | `unspecified<std::tuple<Args...>>` |
  /// The interface of the proxy object is similar to the one proposed in
  /// the `std::expected` proposal:
  /// ```cpp
  /// if (auto&& result = co_await http_request("github.com")) {
  ///   auto value = *result;
  /// } else {
  ///   cti::error_type error = result.get_exception();
  /// }
  ///
  /// auto result = co_await http_request("github.com");
  /// bool(result);
  /// result.is_value();
  /// result.is_exception();
  /// *result; // Same as result.get_value()
  /// result.get_value();
  /// result.get_exception();
  /// ```
  ///
  /// \attention Note that it isn't possible as of now to use a continuable
  ///            as return type from coroutines as depicted below:
  /// ```cpp
  /// cti::continuable<int> do_sth() {
  ///   co_await http_request("github.com");
  ///   // ...
  ///   co_return 0;
  /// }
  /// ```
  ///            Propably this will be added in a future version of the library.
  ///
  /// \since     2.0.0
  auto operator co_await() && {
    return detail::awaiting::create_awaiter(std::move(*this).materialize());
  }
  /// \cond false
#endif // CONTINUABLE_HAS_EXPERIMENTAL_COROUTINE
  /// \endcond

 private:
  void release() noexcept {
    ownership_.release();
  }

  auto materialize() && {
    return detail::connection::materializer<continuable_base>::apply(
        std::move(*this));
  }

  Data&& consume_data() && {
    assert_acquired();
    release();
    return std::move(data_);
  }

  void assert_acquired() const {
    assert(ownership_.is_acquired() && "Tried to use a released continuable!");
  }
};

/// Creates a continuable_base from a promise/callback taking function.
///
/// \tparam Args The types (signature hint) the given promise is resolved with.
/// * **Some arguments** indicate the types the promise will be invoked with.
/// ```cpp
/// auto ct = cti::make_continuable<int, std::string>([](auto&& promise) {
///   promise.set_value(200, "<html>...</html>");
/// });
/// ```
/// * `void` **as argument** indicates that the promise will be invoked
///   with no arguments:
/// ```cpp
/// auto ct = cti::make_continuable<void>([](auto&& promise) {
///   promise.set_value();
/// });
/// ```
/// * **No arguments** Since version 3.0.0 make_continuable always requires
///   to be given valid arguments!
///   You should always give the type hint a callback is called with because
///   it's required for intermediate actions like connecting continuables.
///   You may omit the signature hint if you are erasing the type of
///   the continuable right after creation.
/// ```cpp
/// // This won't work because the arguments are missing:
/// auto ct = cti::make_continuable([](auto&& promise) {
///   promise.set_value(0.f, 'c');
/// });
///
/// // However, you are allowed to do this:
/// cti::continuable<float, char> ct = [](auto&& promise) {
///   promise.set_value(callback)(0.f, 'c');
/// };
/// ```
///
/// \param continuation The continuation the continuable is created from.
/// The continuation must be a callable type accepting a callback parameter
/// which represents the object invokable with the asynchronous result of this
/// continuable.
/// ```cpp
/// auto ct = cti::make_continuable<std::string>([](auto&& promise) {
///   promise.set_value("result");
/// });
/// ```
/// The callback may be stored or moved.
/// In some cases the callback may be copied if supported by the underlying
/// callback chain, in order to invoke the call chain multiple times.
/// It's recommended to accept any callback instead of erasing it.
/// ```cpp
/// // Good practice:
/// auto ct = cti::make_continuable<std::string>([](auto&& promise) {
///   promise.set_value("result");
/// });
///
/// // Good practice using a callable object:
/// struct Continuation {
///   template<typename T>
///   void operator() (T&& continuation) && {
///     // ...
///   }
/// }
///
/// auto ct = cti::make_continuable<std::string>(Continuation{});
///
/// // Bad practice (because of unnecessary type erasure):
/// auto ct = cti::make_continuable<std::string>(
///   [](cti::promise<std::string> promise) {
///     promise.set_value("result");
///   });
/// ```
///
/// \returns A continuable_base with unspecified template parameters which
///          wraps the given continuation.
///          In order to convert the continuable_base to a known type
///          you need to apply type erasure through the
///          \link cti::continuable continuable\endlink or
///          \link cti::promise promise\endlink facilities.
///
/// \note You should always turn the callback/promise into a r-value if possible
///       (`std::move` or `std::forward`) for qualifier correct invokation.
///       Additionally it's important to know that all continuable promises
///       are callbacks and just expose their call operator nicely through
///       \link cti::promise_base::set_value set_value \endlink and
///       \link cti::promise_base::set_exception set_exception \endlink.
///
/// \since 1.0.0
template <typename... Args, typename Continuation>
constexpr auto make_continuable(Continuation&& continuation) {
  static_assert(sizeof...(Args) > 0,
                "Since version 3.0.0 make_continuable requires an exact "
                "signature! If you did intend to create a void continuable "
                "use make_continuable<void>(...). Continuables with an exact "
                "signature may be created through make_continuable<Args...>.");

  return detail::base::attorney::create(
      std::forward<Continuation>(continuation),
      detail::hints::extract(detail::traits::identity<Args...>{}),
      detail::util::ownership{});
}

/// Returns a continuable_base with no result which instantly resolves
/// the promise with no values.
///
/// \attention Usually using this function isn't needed at all since
///            the continuable library is capable of working with
///            plain values in most cases.
///            Try not to use it since it causes unneccessary recursive
///            function calls.
///
/// \since     3.0.0
template <typename... Args>
constexpr auto make_ready_continuable() {
  return make_continuable<void>([](auto&& promise) {
    std::forward<decltype(promise)>(promise).set_value();
  });
}

/// Returns a continuable_base with one result value which instantly resolves
/// the promise with the given value.
///
/// \copydetails make_ready_continuable()
template <typename Result>
constexpr auto make_ready_continuable(Result&& result) {
  return make_continuable<std::decay_t<Result>>( // ...
      [result = std::forward<Result>(result)](auto&& promise) mutable {
        std::forward<decltype(promise)>(promise).set_value(std::move(result));
      });
}

/// Returns a continuable_base with multiple result values which instantly
/// resolves the promise with the given values.
///
/// \copydetails make_ready_continuable()
template <typename FirstResult, typename SecondResult, typename... Rest>
constexpr auto make_ready_continuable(FirstResult&& first_result,
                                      SecondResult&& second_result,
                                      Rest&&... rest) {
  return make_continuable<std::decay_t<FirstResult>, std::decay_t<SecondResult>,
                          std::decay_t<Rest>...>( // ...
      [result = std::make_tuple(std::forward<FirstResult>(first_result),
                                std::forward<SecondResult>(second_result),
                                std::forward<Rest>(rest)...)](
          auto&& promise) mutable {
        detail::traits::unpack(result,
                               std::forward<decltype(promise)>(promise));
      });
}

/// Returns a continuable_base with the parameterized result which instantly
/// resolves the promise with the given error type.
///
/// See an example below:
/// ```cpp
/// std::logic_error exception("Some issue!");
/// auto ptr = std::make_exception_ptr(exception);
/// auto ct = cti::make_exceptional_continuable<int>(ptr);
/// ```
///
/// \tparam Signature The fake signature of the returned continuable.
///
/// \since            3.0.0
template <typename... Signature, typename Exception>
constexpr auto make_exceptional_continuable(Exception&& exception) {
  static_assert(sizeof...(Signature) > 0,
                "Requires at least one type for the fake signature!");

  return make_continuable<Signature...>( // ...
      [exception = std::forward<Exception>(exception)](auto&& promise) mutable {
        std::forward<decltype(promise)>(promise).set_exception(
            std::move(exception));
      });
}
/// \}
} // namespace cti

#endif // CONTINUABLE_BASE_HPP_INCLUDED

// #include <continuable/continuable-connections.hpp>

/*

                        /~` _  _ _|_. _     _ |_ | _
                        \_,(_)| | | || ||_|(_||_)|(/_

                    https://github.com/Naios/continuable
                                   v3.0.0

  Copyright(c) 2015 - 2018 Denis Blank <denis.blank at outlook dot com>

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files(the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and / or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions :

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
**/

#ifndef CONTINUABLE_CONNECTIONS_HPP_INCLUDED
#define CONTINUABLE_CONNECTIONS_HPP_INCLUDED

#include <initializer_list>
#include <memory>
#include <utility>
#include <vector>

// #include <continuable/detail/connection-all.hpp>

// #include <continuable/detail/connection-any.hpp>

// #include <continuable/detail/connection-seq.hpp>

// #include <continuable/detail/connection.hpp>

// #include <continuable/detail/range.hpp>

/*

                        /~` _  _ _|_. _     _ |_ | _
                        \_,(_)| | | || ||_|(_||_)|(/_

                    https://github.com/Naios/continuable
                                   v3.0.0

  Copyright(c) 2015 - 2018 Denis Blank <denis.blank at outlook dot com>

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files(the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and / or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions :

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
**/

#ifndef CONTINUABLE_DETAIL_RANGE_HPP_INCLUDED
#define CONTINUABLE_DETAIL_RANGE_HPP_INCLUDED

#include <iterator>
#include <type_traits>
#include <utility>
#include <vector>

// #include <continuable/detail/traits.hpp>


namespace cti {
namespace detail {
namespace range {
/// Deduces to a true_type if the given type is an interator
template <typename T, typename = void>
struct is_iterator : std::false_type {};
template <typename T>
struct is_iterator<T,
                   traits::void_t<typename std::iterator_traits<T>::value_type>>
    : std::true_type {};

/// Moves the content of the given iterators to a persistent storage
template <typename Iterator>
auto persist_range(Iterator begin, Iterator end) {
  std::vector<typename std::iterator_traits<Iterator>::value_type> storage;
  // TODO Find out why the superior idiom below has issues with move only types:
  // storage.insert(storage.end(), std::make_move_iterator(begin),
  //                std::make_move_iterator(end));
  std::move(begin, end, std::back_inserter(storage));
  return storage;
}
} // namespace range
} // namespace detail
} // namespace cti

#endif // CONTINUABLE_DETAIL_RANGE_HPP_INCLUDED


namespace cti {
/// \defgroup Connections Connections
/// provides functions to connect \link continuable_base
/// continuable_bases\endlink through various strategies.
/// \{

/// Connects the given arguments with an all logic.
/// All continuables contained inside the given nested pack are
/// invoked at once. On completion the final handler is called
/// with the aggregated result of all continuables.
///
/// \param args Arbitrary arguments which are connected.
///             Every type is allowed as arguments, continuables may be
///             contained inside tuple like types (`std::tuple`)
///             or in homogeneous containers such as `std::vector`.
///             Non continuable arguments are preserved and passed
///             to the final result as shown below:
/// ```cpp
/// cti::when_all(
///     cti::make_ready_continuable(0, 1),
///     2, //< See this plain value
///     cti::populate(cti::make_ready_continuable(3),  // Creates a runtime
///                   cti::make_ready_continuable(4)), // sized container.
///     std::make_tuple(std::make_tuple(cti::make_ready_continuable(5))))
///       .then([](int r0, int r1, int r2, std::vector<int> r34,
///                std::tuple<std::tuple<int>> r5) {
///         // ...
///       });
/// ```
///
/// \see        continuable_base::operator&& for details.
///
/// \since      1.1.0
template <typename... Args>
auto when_all(Args&&... args) {
  return detail::connection::apply_connection(
      detail::connection::connection_strategy_all_tag{},
      std::forward<Args>(args)...);
}

/// Connects the given arguments with an all logic.
/// The content of the iterator is moved out and converted
/// to a temporary `std::vector` which is then passed to when_all.
///
/// ```cpp
/// // cti::populate just creates a std::vector from the two continuables.
/// auto v = cti::populate(cti::make_ready_continuable(0),
///                        cti::make_ready_continuable(1));
///
/// cti::when_all(v.begin(), v.end())
///   .then([](std::vector<int> r01) {
///     // ...
///   });
/// ```
///
/// \param begin The begin iterator to the range which will be moved out
///              and used as the arguments to the all connection
///
/// \param end   The end iterator to the range which will be moved out
///              and used as the arguments to the all connection
///
/// \see         when_all for details.
///
/// \attention   Prefer to invoke when_all with the whole container the
///              iterators were taken from, since this saves us
///              the creation of a temporary storage.
///
/// \since       3.0.0
template <
    typename Iterator,
    std::enable_if_t<detail::range::is_iterator<Iterator>::value>* = nullptr>
auto when_all(Iterator begin, Iterator end) {
  return when_all(detail::range::persist_range(begin, end));
}

/// Connects the given arguments with a sequential logic.
/// All continuables contained inside the given nested pack are
/// invoked one after one. On completion the final handler is called
/// with the aggregated result of all continuables.
///
/// \param args Arbitrary arguments which are connected.
///             Every type is allowed as arguments, continuables may be
///             contained inside tuple like types (`std::tuple`)
///             or in homogeneous containers such as `std::vector`.
///             Non continuable arguments are preserved and passed
///             to the final result as shown below:
/// ```cpp
/// cti::when_seq(
///     cti::make_ready_continuable(0, 1),
///     2, //< See this plain value
///     cti::populate(cti::make_ready_continuable(3),  // Creates a runtime
///                   cti::make_ready_continuable(4)), // sized container.
///     std::make_tuple(std::make_tuple(cti::make_ready_continuable(5))))
///       .then([](int r0, int r1, int r2, std::vector<int> r34,
///                std::tuple<std::tuple<int>> r5) {
///         // ...
///       });
/// ```
///
/// \see        continuable_base::operator>> for details.
///
/// \since      1.1.0
template <typename... Args>
auto when_seq(Args&&... args) {
  return detail::connection::apply_connection(
      detail::connection::connection_strategy_seq_tag{},
      std::forward<Args>(args)...);
}

/// Connects the given arguments with a sequential logic.
/// The content of the iterator is moved out and converted
/// to a temporary `std::vector` which is then passed to when_seq.
///
/// ```cpp
/// // cti::populate just creates a std::vector from the two continuables.
/// auto v = cti::populate(cti::make_ready_continuable(0),
///                        cti::make_ready_continuable(1));
///
/// cti::when_seq(v.begin(), v.end())
///   .then([](std::vector<int> r01) {
///     // ...
///   });
/// ```
///
/// \param begin The begin iterator to the range which will be moved out
///              and used as the arguments to the sequential connection
///
/// \param end   The end iterator to the range which will be moved out
///              and used as the arguments to the sequential connection
///
/// \see         when_seq for details.
///
/// \attention   Prefer to invoke when_seq with the whole container the
///              iterators were taken from, since this saves us
///              the creation of a temporary storage.
///
/// \since       3.0.0
template <
    typename Iterator,
    std::enable_if_t<detail::range::is_iterator<Iterator>::value>* = nullptr>
auto when_seq(Iterator begin, Iterator end) {
  return when_seq(detail::range::persist_range(begin, end));
}

/// Connects the given arguments with an any logic.
/// All continuables contained inside the given nested pack are
/// invoked at once. On completion of one continuable the final handler
/// is called with the result of the resolved continuable.
///
/// \param args Arbitrary arguments which are connected.
///             Every type is allowed as arguments, continuables may be
///             contained inside tuple like types (`std::tuple`)
///             or in homogeneous containers such as `std::vector`.
///             Non continuable arguments are preserved and passed
///             to the final result as shown below:
/// ```cpp
/// cti::when_any(
///     cti::make_ready_continuable(0, 1),
///     2, //< See this plain value
///     cti::populate(cti::make_ready_continuable(3),  // Creates a runtime
///                   cti::make_ready_continuable(4)), // sized container.
///     std::make_tuple(std::make_tuple(cti::make_ready_continuable(5))))
///       .then([](int r0) {
///         // ...
///       });
/// ```
///
/// \see        continuable_base::operator|| for details.
///
/// \since      1.1.0
template <typename... Args>
auto when_any(Args&&... args) {
  return detail::connection::apply_connection(
      detail::connection::connection_strategy_any_tag{},
      std::forward<Args>(args)...);
}

/// Connects the given arguments with an any logic.
/// The content of the iterator is moved out and converted
/// to a temporary `std::vector` which is then passed to when_all.
///
/// ```cpp
/// // cti::populate just creates a std::vector from the two continuables.
/// auto v = cti::populate(cti::make_ready_continuable(0),
///                        cti::make_ready_continuable(1));
///
/// cti::when_any(v.begin(), v.end())
///   .then([](int r01) {
///     // ...
///   });
/// ```
///
/// \param begin The begin iterator to the range which will be moved out
///              and used as the arguments to the all connection
///
/// \param end   The end iterator to the range which will be moved out
///              and used as the arguments to the all connection
///
/// \see         when_any for details.
///
/// \attention   Prefer to invoke when_any with the whole container the
///              iterators were taken from, since this saves us
///              the creation of a temporary storage.
///
/// \since       3.0.0
template <
    typename Iterator,
    std::enable_if_t<detail::range::is_iterator<Iterator>::value>* = nullptr>
auto when_any(Iterator begin, Iterator end) {
  return when_any(detail::range::persist_range(begin, end));
}

/// Populates a homogeneous container from the given arguments.
/// All arguments need to be convertible to the first one,
/// by default `std::vector` is used as container type.
///
/// This method mainly helps to create a homogeneous container from
/// a runtime known count of continuables which type isn't exactly known.
/// All continuables which are passed to this function should be originating
/// from the same source or a method called with the same types of arguments:
/// ```cpp
/// auto container = cti::populate(cti::make_ready_continuable(0),
///                                cti::make_ready_continuable(1)),
///
/// for (int i = 2; i < 5; ++i) {
///   // You may add more continuables to the container afterwards
///   container.emplace_back(cti::make_ready_continuable(i));
/// }
///
/// cti::when_any(std::move(container))
///   .then([](int) {
///     // ...
///   });
/// ```
/// Additionally it is possible to change the targeted container as below:
/// ```cpp
/// auto container = cti::populate<std::list>(cti::make_ready_continuable(0),
///                                           cti::make_ready_continuable(1)),
/// ```
///
/// \tparam C The container type which is used to store the arguments into.
///
/// \since    3.0.0
template <template <typename, typename> class C = std::vector, typename First,
    typename... Args>
C<std::decay_t<First>, std::allocator<std::decay_t<First>>>
populate(First&& first, Args&&... args) {
  C<std::decay_t<First>, std::allocator<std::decay_t<First>>> container;
  container.reserve(1 + sizeof...(Args));
  container.emplace_back(std::forward<First>(first));
  (void)std::initializer_list<int>{
      0, ((void)container.emplace_back(std::forward<Args>(args)), 0)...};
  return container; // RVO
}
/// \}
} // namespace cti

#endif // CONTINUABLE_CONNECTIONS_HPP_INCLUDED

// #include <continuable/continuable-promise-base.hpp>

// #include <continuable/continuable-promisify.hpp>

/*

                        /~` _  _ _|_. _     _ |_ | _
                        \_,(_)| | | || ||_|(_||_)|(/_

                    https://github.com/Naios/continuable
                                   v3.0.0

  Copyright(c) 2015 - 2018 Denis Blank <denis.blank at outlook dot com>

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files(the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and / or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions :

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
**/

#ifndef CONTINUABLE_PROMISIFY_HPP_INCLUDED
#define CONTINUABLE_PROMISIFY_HPP_INCLUDED

#include <type_traits>
#include <utility>

// #include <continuable/detail/promisify.hpp>

/*

                        /~` _  _ _|_. _     _ |_ | _
                        \_,(_)| | | || ||_|(_||_)|(/_

                    https://github.com/Naios/continuable
                                   v3.0.0

  Copyright(c) 2015 - 2018 Denis Blank <denis.blank at outlook dot com>

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files(the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and / or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions :

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
**/

#ifndef CONTINUABLE_DETAIL_PROMISIFY_HPP_INCLUDED
#define CONTINUABLE_DETAIL_PROMISIFY_HPP_INCLUDED

#include <type_traits>

#if defined(CONTINUABLE_HAS_EXCEPTIONS)
#include <exception>
#endif // CONTINUABLE_HAS_EXCEPTIONS

// #include <continuable/continuable-base.hpp>

// #include <continuable/detail/traits.hpp>

// #include <continuable/detail/util.hpp>


namespace cti {
namespace detail {
namespace convert {
/// A helper class for promisifying asio and js style callback
/// taking functions into a continuable.
template <typename P>
struct promisify_default {
  P promise;

  template <typename E, typename... T>
  void operator()(E&& error, T&&... result) {
    if (error) {
#if defined(CONTINUABLE_HAS_EXCEPTIONS)
      promise.set_exception(std::make_exception_ptr(std::forward<E>(error)));
#else
      promise.set_exception(
          std::error_condition(error.value(), error.category()));
#endif // CONTINUABLE_HAS_EXCEPTIONS

    } else {
      promise.set_value(std::forward<T>(result)...);
    }
  }
};

template <typename... Result>
struct promisify_helper {
  template <template <class T> class Evaluator, typename Callable,
      typename... Args>
  static auto from(Callable&& callable, Args&&... args) {
    return make_continuable<Result...>([args = std::make_tuple(
        std::forward<Callable>(callable),
        std::forward<Args>(args)...)](
        auto&& promise) mutable {

      traits::unpack(
          std::move(args), [promise = std::forward<decltype(promise)>(promise)](
              auto&&... args) mutable {
            Evaluator<std::decay_t<decltype(promise)>> evaluator{
                std::move(promise)};

            util::invoke(std::forward<decltype(args)>(args)...,
                         std::move(evaluator));
          });
    });
  }
};
} // namespace convert
} // namespace detail
} // namespace cti

#endif // CONTINUABLE_DETAIL_PROMISIFY_HPP_INCLUDED


namespace cti {
/// \defgroup Promisify Promisify
/// provides helper methods to convert various callback styles to
/// \link continuable_base continuable_bases\endlink.
/// \{

/// Helper class for converting callback taking callable types into a
/// a continuable. Various styles are supported.
/// - `from`: Converts callback taking callable types into continuables
///           which pass an error code as first parameter and the rest of
///           the result afterwards.
///
/// \tparam Result The result of the converted continuable, this should align
///                with the arguments that are passed to the callback.
///
/// \since         3.0.0
template <typename... Result>
class promisify {
  using helper = detail::convert::promisify_helper<Result...>;

 public:
  /// Converts callback taking callable types into a continuable.
  /// This applies to calls which pass an error code as first parameter
  /// and the rest of the asynchronous result afterwards.
  ///
  /// See an example of how to promisify boost asio's async_resolve below:
  /// ```cpp
  /// auto async_resolve(std::string host, std::string service) {
  ///   return cti::promisify<asio::ip::udp::resolver::iterator>::from(
  ///       [&](auto&&... args) {
  ///         resolver_.async_resolve(std::forward<decltype(args)>(args)...);
  ///       },
  ///       std::move(host), std::move(service));
  /// }
  /// ```
  ///
  /// If the error code which is passed as first parameter is set there are
  /// two behaviours depending whether exceptions are enabled:
  /// - If exceptions are enabled the error type is passed via
  ///   an exception_ptr to the failure handler.
  /// - If exceptions are disabled the error type is converted to a
  ///   `std::error_conditon` and passed down to the error handler.
  ///
  /// \since  3.0.0
  template <typename Callable, typename... Args>
  static auto from(Callable&& callable, Args&&... args) {
    return helper::template from<detail::convert::promisify_default>(
        std::forward<Callable>(callable), std::forward<Args>(args)...);
  }
};
/// \}
} // namespace cti

#endif // CONTINUABLE_PROMISIFY_HPP_INCLUDED

// #include <continuable/continuable-trait.hpp>

/*

                        /~` _  _ _|_. _     _ |_ | _
                        \_,(_)| | | || ||_|(_||_)|(/_

                    https://github.com/Naios/continuable
                                   v3.0.0

  Copyright(c) 2015 - 2018 Denis Blank <denis.blank at outlook dot com>

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files(the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and / or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions :

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
**/

#ifndef CONTINUABLE_TRAIT_HPP_INCLUDED
#define CONTINUABLE_TRAIT_HPP_INCLUDED

#include <cstdint>

// #include <continuable/continuable-base.hpp>

// #include <continuable/continuable-promise-base.hpp>

// #include <continuable/detail/hints.hpp>

// #include <continuable/detail/types.hpp>


namespace cti {
/// \defgroup Types Types
/// provides the \link cti::continuable continuable\endlink and \link
/// cti::promise promise\endlink facility for type erasure.
/// \{

/// Trait to retrieve a continuable_base type with a given type-erasure backend.
///
/// Every object may me used as type-erasure backend as long as the
/// requirements of a `std::function` like wrapper are satisfied.
///
/// \tparam CallbackWrapper The type which is used to erase the callback.
///
/// \tparam ContinuationWrapper The type which is used to erase the
///         continuation data.
///
/// \tparam Args The current signature of the continuable.
template <template <std::size_t, typename...> class CallbackWrapper,
    template <std::size_t, typename...> class ContinuationWrapper,
    typename... Args>
class continuable_trait {

  using callback = CallbackWrapper<0U, void(Args...)&&,
                                   void(detail::types::dispatch_error_tag,
                                        detail::types::error_type) &&>;

 public:
  /// The promise type which is used to resolve continuations
  using promise =
  promise_base<callback, detail::hints::signature_hint_tag<Args...>>;

  /// The continuable type for the given parameters.
  using continuable =
  continuable_base<ContinuationWrapper<sizeof(callback), void(promise)>,
                   detail::hints::signature_hint_tag<Args...>>;
};
/// \}
} // namespace cti

#endif // CONTINUABLE_TRAIT_HPP_INCLUDED

// #include <continuable/continuable-transforms.hpp>

/*

                        /~` _  _ _|_. _     _ |_ | _
                        \_,(_)| | | || ||_|(_||_)|(/_

                    https://github.com/Naios/continuable
                                   v3.0.0

  Copyright(c) 2015 - 2018 Denis Blank <denis.blank at outlook dot com>

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files(the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and / or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions :

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
**/

#ifndef CONTINUABLE_TRANSFORMS_HPP_INCLUDED
#define CONTINUABLE_TRANSFORMS_HPP_INCLUDED

// #include <continuable/detail/transforms.hpp>

/*

                        /~` _  _ _|_. _     _ |_ | _
                        \_,(_)| | | || ||_|(_||_)|(/_

                    https://github.com/Naios/continuable
                                   v3.0.0

  Copyright(c) 2015 - 2018 Denis Blank <denis.blank at outlook dot com>

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files(the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and / or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions :

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
**/

#ifndef CONTINUABLE_DETAIL_TRANSFORMS_HPP_INCLUDED
#define CONTINUABLE_DETAIL_TRANSFORMS_HPP_INCLUDED

#include <future>

// #include <continuable/detail/base.hpp>

// #include <continuable/detail/features.hpp>

// #include <continuable/detail/hints.hpp>

// #include <continuable/detail/types.hpp>

// #include <continuable/detail/util.hpp>


namespace cti {
namespace detail {
/// Provides helper functions to transform continuations to other types
namespace transforms {
/// Provides helper functions and typedefs for converting callback arguments
/// to their types a promise can accept.
template <typename... Args>
struct future_trait {
  /// The promise type used to create the future
  using promise_t = std::promise<std::tuple<Args...>>;
  /// Boxes the argument pack into a tuple
  static void resolve(promise_t& promise, Args... args) {
    promise.set_value(std::make_tuple(std::move(args)...));
  }
};
template <>
struct future_trait<> {
  /// The promise type used to create the future
  using promise_t = std::promise<void>;
  /// Boxes the argument pack into void
  static void resolve(promise_t& promise) {
    promise.set_value();
  }
};
template <typename First>
struct future_trait<First> {
  /// The promise type used to create the future
  using promise_t = std::promise<First>;
  /// Boxes the argument pack into nothing
  static void resolve(promise_t& promise, First first) {
    promise.set_value(std::move(first));
  }
};

template <typename Hint>
class promise_callback;

template <typename... Args>
class promise_callback<hints::signature_hint_tag<Args...>>
    : public future_trait<Args...> {

  typename future_trait<Args...>::promise_t promise_;

 public:
  constexpr promise_callback() = default;
  promise_callback(promise_callback const&) = delete;
  constexpr promise_callback(promise_callback&&) = default;
  promise_callback& operator=(promise_callback const&) = delete;
  promise_callback& operator=(promise_callback&&) = delete;

  /// Resolves the promise
  void operator()(Args... args) {
    this->resolve(promise_, std::move(args)...);
  }

  /// Resolves the promise through the exception
  void operator()(types::dispatch_error_tag, types::error_type error) {
#if defined(CONTINUABLE_HAS_EXCEPTIONS)
    promise_.set_exception(error);
#else
    (void)error;

    // Can't forward a std::error_condition or custom error type
    // to a std::promise. Handle the error first in order
    // to prevent this trap!
    util::trap();
#endif // CONTINUABLE_HAS_EXCEPTIONS
  }

  /// Returns the future from the promise
  auto get_future() {
    return promise_.get_future();
  }
};

/// Transforms the continuation to a future
template <typename Data, typename Annotation>
auto as_future(continuable_base<Data, Annotation>&& continuable) {
  // Create the promise which is able to supply the current arguments
  constexpr auto const hint =
      hints::hint_of(traits::identify<decltype(continuable)>{});

  promise_callback<std::decay_t<decltype(hint)>> callback;
  (void)hint;

  // Receive the future
  auto future = callback.get_future();

  // Dispatch the continuation with the promise resolving callback
  std::move(continuable).next(std::move(callback)).done();

  return future;
}
} // namespace transforms
} // namespace detail
} // namespace cti

#endif // CONTINUABLE_DETAIL_TRANSFORMS_HPP_INCLUDED

// #include <continuable/detail/types.hpp>


namespace cti {
/// \defgroup Transforms Transforms
/// provides utilities to convert
/// \link continuable_base continuable_bases\endlink to other
/// types such as (`std::future`).
/// \{

/// A callable tag object which marks a wrapped callable object
/// as continuable transformation which enables some useful overloads.
///
/// \since 3.0.0
template <typename T>
using transform = detail::types::transform<T>;

/// Wraps the given callable object into a transform class.
///
/// \since 3.0.0
template <typename T>
auto make_transform(T&& callable) {
  return transform<std::decay_t<T>>(std::forward<T>(callable));
}

/// The namespace transforms declares callable objects that transform
/// any continuable_base to an object or to a continuable_base itself.
///
/// Transforms can be applied to continuables through using
/// the cti::continuable_base::apply method accordingly.
namespace transforms {
/// Returns a transform that if applied to a continuable,
/// it will start the continuation chain and returns the asynchronous
/// result as `std::future<...>`.
///
/// \returns Returns a `std::future<...>` which becomes ready as soon
///          as the the continuation chain has finished.
///          The signature of the future depends on the result type:
/// |          Continuation type        |             Return type            |
/// | : ------------------------------- | : -------------------------------- |
/// | `continuable_base with <>`        | `std::future<void>`                |
/// | `continuable_base with <Arg>`     | `std::future<Arg>`                 |
/// | `continuable_base with <Args...>` | `std::future<std::tuple<Args...>>` |
///
/// \attention If exceptions are used, exceptions that are thrown, are forwarded
///            to the returned future. If there are no exceptions supported,
///            you shall not pass any errors to the end of the asynchronous
///            call chain!
///            Otherwise this will yield a trap that causes application exit.
///
/// \since 2.0.0
inline auto futurize() {
  return make_transform([](auto&& continuable) {
    using detail::transforms::as_future;
    return as_future(std::forward<decltype(continuable)>(continuable));
  });
}

/// Returns a transform that if applied to a continuable, it will ignores all
/// error which ocured until the point the transform was applied.
///
/// \returns Returns a continuable with the same signature as applied to.
///
/// \attention This can be used to create a continuable which doesn't resolve
///            the continuation on errors.
///
/// \since 2.0.0
inline auto flatten() {
  return make_transform([](auto&& continuable) {
    return std::forward<decltype(continuable)>(continuable).fail([](auto&&) {});
  });
}
/// \}
} // namespace transforms
} // namespace cti

#endif // CONTINUABLE_TRANSFORMS_HPP_INCLUDED

// #include <continuable/continuable-traverse-async.hpp>

// #include <continuable/continuable-traverse.hpp>

// #include <continuable/continuable-types.hpp>

/*

                        /~` _  _ _|_. _     _ |_ | _
                        \_,(_)| | | || ||_|(_||_)|(/_

                    https://github.com/Naios/continuable
                                   v3.0.0

  Copyright(c) 2015 - 2018 Denis Blank <denis.blank at outlook dot com>

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files(the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and / or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions :

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
**/

#ifndef CONTINUABLE_TYPES_HPP_INCLUDED
#define CONTINUABLE_TYPES_HPP_INCLUDED

#include <cstdint>

// #include <function2/function2.hpp>

//  Copyright 2015-2017 Denis Blank <denis.blank at outlook dot com>
//     Distributed under the Boost Software License, Version 1.0
//       (See accompanying file LICENSE_1_0.txt or copy at
//             http://www.boost.org/LICENSE_1_0.txt)

#ifndef FU2_INCLUDED_FUNCTION2_HPP__
#define FU2_INCLUDED_FUNCTION2_HPP__

#include <cassert>
#include <cstdlib>
#include <memory>
#include <tuple>
#include <type_traits>
#include <utility>

// Defines:
// - FU2_MACRO_DISABLE_EXCEPTIONS
#if defined(_MSC_VER)
#if !defined(_HAS_EXCEPTIONS) || (_HAS_EXCEPTIONS == 0)
#define FU2_MACRO_DISABLE_EXCEPTIONS
#endif
#elif defined(__clang__)
#if !(__EXCEPTIONS && __has_feature(cxx_exceptions))
#define FU2_MACRO_DISABLE_EXCEPTIONS
#endif
#elif defined(__GNUC__)
#if !__EXCEPTIONS
#define FU2_MACRO_DISABLE_EXCEPTIONS
#endif
#endif

#if !defined(FU2_NO_FUNCTIONAL_HEADER) || !defined(FU2_MACRO_DISABLE_EXCEPTIONS)
#include <functional>
#endif

#if !defined(FU2_MACRO_DISABLE_EXCEPTIONS)
#include <exception>
#endif

namespace fu2 {
inline namespace v5 {
namespace detail {
template <typename...>
struct identity {};

// Equivalent to C++17's std::void_t which is targets a bug in GCC,
// that prevents correct SFINAE behavior.
// See http://stackoverflow.com/questions/35753920 for details.
template <typename...>
struct deduce_to_void : std::common_type<void> {};

template <typename... T>
using void_t = typename deduce_to_void<T...>::type;

// Copy enabler helper class
template <bool /*Copyable*/>
struct copyable {};
template <>
struct copyable<false> {
  copyable() = default;
  copyable(copyable const&) = delete;
  copyable(copyable&&) = default;
  copyable& operator=(copyable const&) = delete;
  copyable& operator=(copyable&&) = default;
};

/// Configuration trait to configure the function_base class.
template <bool Owning, bool Copyable, std::size_t Capacity>
struct config {
  // Is true if the function is copyable.
  static constexpr auto const is_owning = Owning;

  // Is true if the function is copyable.
  static constexpr auto const is_copyable = Copyable;

  // The internal capacity of the function
  // used in small functor optimization.
  static constexpr auto const capacity = Capacity;
};

/// A config which isn't compatible to other configs
template <bool Throws, bool HasStrongExceptGuarantee, typename... Args>
struct property {
  // Is true when the function throws an exception on empty invocation.
  static constexpr auto const is_throwing = Throws;

  // Is true when the function throws an exception on empty invocation.
  static constexpr auto const is_strong_exception_guaranteed = Throws;
};

/// Provides utilities for invocing callable objects
namespace invocation {
/// Invokes the given callable object with the given arguments
template <typename Callable, typename... Args>
constexpr auto invoke(Callable&& callable, Args&&... args) noexcept(
noexcept(std::forward<Callable>(callable)(std::forward<Args>(args)...)))
-> decltype(std::forward<Callable>(callable)(std::forward<Args>(args)...)) {

  return std::forward<Callable>(callable)(std::forward<Args>(args)...);
}
/// Invokes the given member function pointer by reference
template <typename T, typename Type, typename Self, typename... Args>
constexpr auto invoke(Type T::*member, Self&& self, Args&&... args) noexcept(
noexcept((std::forward<Self>(self).*member)(std::forward<Args>(args)...)))
-> decltype((std::forward<Self>(self).*
    member)(std::forward<Args>(args)...)) {
  return (std::forward<Self>(self).*member)(std::forward<Args>(args)...);
}
/// Invokes the given member function pointer by pointer
template <typename T, typename Type, typename Self, typename... Args>
constexpr auto invoke(Type T::*member, Self&& self, Args&&... args) noexcept(
noexcept((std::forward<Self>(self)->*member)(std::forward<Args>(args)...)))
-> decltype(
(std::forward<Self>(self)->*member)(std::forward<Args>(args)...)) {
  return (std::forward<Self>(self)->*member)(std::forward<Args>(args)...);
}

/// Deduces to a true type if the callable object can be invoked with
/// the given arguments.
/// We don't use invoke here because MSVC can't evaluate the nested expression
/// SFINAE here.
template <typename T, typename Args, typename = void>
struct can_invoke : std::false_type {};
template <typename T, typename... Args>
struct can_invoke<T, identity<Args...>,
                  decltype((void)std::declval<T>()(std::declval<Args>()...))>
    : std::true_type {};
template <typename Pointer, typename T, typename... Args>
struct can_invoke<Pointer, identity<T&, Args...>,
                  decltype((void)((std::declval<T&>().*std::declval<Pointer>())(
                      std::declval<Args>()...)))> : std::true_type {};
template <typename Pointer, typename T, typename... Args>
struct can_invoke<Pointer, identity<T&&, Args...>,
                  decltype(
                  (void)((std::declval<T&&>().*std::declval<Pointer>())(
                      std::declval<Args>()...)))> : std::true_type {};
template <typename Pointer, typename T, typename... Args>
struct can_invoke<Pointer, identity<T*, Args...>,
                  decltype(
                  (void)((std::declval<T*>()->*std::declval<Pointer>())(
                      std::declval<Args>()...)))> : std::true_type {};
} // end namespace invocation

namespace overloading {
template <typename... Args>
struct overload_impl;
template <typename Current, typename Next, typename... Rest>
struct overload_impl<Current, Next, Rest...> : Current,
                                               overload_impl<Next, Rest...> {
  explicit overload_impl(Current current, Next next, Rest... rest)
      : Current(std::move(current)), overload_impl<Next, Rest...>(
      std::move(next), std::move(rest)...) {
  }

  using Current::operator();
  using overload_impl<Next, Rest...>::operator();
};
template <typename Current>
struct overload_impl<Current> : Current {
  explicit overload_impl(Current current) : Current(std::move(current)) {
  }

  using Current::operator();
};

template <typename... T>
constexpr auto overload(T&&... callables) {
  return overload_impl<std::decay_t<T>...>{std::forward<T>(callables)...};
}
} // namespace overloading

/// Declares the namespace which provides the functionality to work with a
/// type-erased object.
namespace type_erasure {
/// Store the allocator inside the box
template <typename T, typename Allocator>
struct box : Allocator {
  T value_;

  explicit box(T value, Allocator allocator)
      : Allocator(std::move(allocator)), value_(std::move(value)) {
  }

  /// Allocates space through the boxed allocator
  box* box_allocate() const {
    using real_allocator = typename std::allocator_traits<
        std::decay_t<Allocator>>::template rebind_alloc<box<T, Allocator>>;
    real_allocator allocator(*static_cast<Allocator const*>(this));

    return static_cast<box*>(
        std::allocator_traits<real_allocator>::allocate(allocator, 1U));
  }

  /// Destroys the box through the given allocator
  static void box_deallocate(box* me) {
    using real_allocator = typename std::allocator_traits<
        std::decay_t<Allocator>>::template rebind_alloc<box<T, Allocator>>;
    real_allocator allocator(*static_cast<Allocator const*>(me));

    me->~box();
    std::allocator_traits<real_allocator>::deallocate(allocator, me, 1U);
  }
};

/// Creates a box containing the given value and allocator
template <typename T, typename Allocator = std::allocator<std::decay_t<T>>>
auto make_box(T&& value, Allocator&& allocator = Allocator{}) {
  return box<std::decay_t<T>, std::decay_t<Allocator>>{
      std::forward<T>(value), std::forward<Allocator>(allocator)};
}

template <typename T>
struct is_box : std::false_type {};
template <typename T, typename Allocator>
struct is_box<box<T, Allocator>> : std::true_type {};

/// Provides access to the pointer to a heal allocated erased object
/// as well to the inplace storage.
typedef union {
  /// The pointer we use if the object is on the heap
  void* ptr_;
  /// The first field of the inplace storage
  std::size_t inplace_storage_;
} data_accessor;

/// See opcode::op_fetch_empty
constexpr void write_empty(data_accessor* accessor, bool empty) noexcept {
  accessor->inplace_storage_ = std::size_t(empty);
}

template <typename From, typename To>
using transfer_const_t =
std::conditional_t<std::is_const<std::remove_pointer_t<From>>::value,
                   std::add_const_t<To>, To>;
template <typename From, typename To>
using transfer_volatile_t =
std::conditional_t<std::is_volatile<std::remove_pointer_t<From>>::value,
                   std::add_volatile_t<To>, To>;

/// The retriever when the object is allocated inplace
template <typename T, typename Accessor>
constexpr auto retrieve(std::true_type /*is_inplace*/, Accessor from,
                        std::size_t from_capacity) {
  using Type = transfer_const_t<Accessor, transfer_volatile_t<Accessor, void>>*;

  /// Process the command by using the data inside the internal capacity
  auto storage = &(from->inplace_storage_);
  auto inplace = const_cast<void*>(static_cast<Type>(storage));
  return Type(std::align(alignof(T), sizeof(T), inplace, from_capacity));
}

/// The retriever which is used when the object is allocated
/// through the allocator
template <typename T, typename Accessor>
constexpr auto retrieve(std::false_type /*is_inplace*/, Accessor from,
                        std::size_t /*from_capacity*/) {

  return from->ptr_;
}

/// For allowing private access to erasure
struct erasure_attorney {
  /// Invoke the function of the erasure at the given index
  ///
  /// We define this out of class to be able to forward the qualified
  /// erasure correctly.
  template <std::size_t Index, typename Erasure, typename... Args>
  static constexpr auto invoke(Erasure&& erasure, Args&&... args) noexcept(
  noexcept(std::forward<Erasure>(erasure).vtable_.template invoke<Index>(
      erasure.opaque_ptr(), erasure.capacity(),
      std::forward<Args>(args)...))) {
    // Add data pointer and the capacity to the arguments
    return erasure.vtable_.template invoke<Index>(
        erasure.opaque_ptr(), erasure.capacity(), std::forward<Args>(args)...);
  }
};

namespace invocation_table {
#if defined(FU2_NO_FUNCTIONAL_HEADER)
struct bad_function_call : std::exception {
  bad_function_call() noexcept {
  }

  char const* what() const noexcept override {
    return "bad function call";
  }
};
#elif !defined(FU2_MACRO_DISABLE_EXCEPTIONS)
using std::bad_function_call;
#endif

#define FU2_EXPAND_QUALIFIERS(F)                                               \
  F(, , , &)                                                                   \
  F(const, , , &)                                                              \
  F(, volatile, , &)                                                           \
  F(const, volatile, , &)                                                      \
  F(, , &, &)                                                                  \
  F(const, , &, &)                                                             \
  F(, volatile, &, &)                                                          \
  F(const, volatile, &, &)                                                     \
  F(, , &&, &&)                                                                \
  F(const, , &&, &&)                                                           \
  F(, volatile, &&, &&)                                                        \
  F(const, volatile, &&, &&)

/// Calls std::abort on empty function calls
[[noreturn]] inline void throw_or_abort(std::false_type /*is_throwing*/) {
  std::abort();
}
/// Throws bad_function_call on empty funciton calls
[[noreturn]] inline void throw_or_abort(std::true_type /*is_throwing*/) {
#ifdef FU2_MACRO_DISABLE_EXCEPTIONS
  throw_or_abort(std::false_type{});
#else
  throw bad_function_call{};
#endif
}

template <typename T>
struct function_trait;

#define FU2_DEFINE_FUNCTION_TRAIT(CONST, VOLATILE, OVL_REF, REF)               \
  template <typename Ret, typename... Args>                                    \
  struct function_trait<Ret(Args...) CONST VOLATILE OVL_REF> {                 \
    using pointer_type = Ret (*)(data_accessor CONST VOLATILE*,                \
                                 std::size_t capacity, Args...);               \
    template <typename T, bool IsInplace>                                      \
    struct internal_invoker {                                                  \
      static Ret invoke(data_accessor CONST VOLATILE* data,                    \
                        std::size_t capacity, Args... args) {                  \
        auto obj = retrieve<T>(std::integral_constant<bool, IsInplace>{},      \
                               data, capacity);                                \
        auto box = static_cast<T CONST VOLATILE*>(obj);                        \
        return invocation::invoke(                                             \
            static_cast<std::decay_t<decltype(box->value_)> CONST VOLATILE     \
                            REF>(box->value_),                                 \
            std::forward<Args>(args)...);                                      \
      }                                                                        \
    };                                                                         \
                                                                               \
    template <typename T>                                                      \
    using callable = T CONST VOLATILE REF;                                     \
                                                                               \
    using arguments = identity<Args...>;                                       \
                                                                               \
    template <bool Throws>                                                     \
    struct empty_invoker {                                                     \
      static Ret invoke(data_accessor CONST VOLATILE* /*data*/,                \
                        std::size_t /*capacity*/, Args... /*args*/) {          \
        throw_or_abort(std::integral_constant<bool, Throws>{});                \
      }                                                                        \
    };                                                                         \
  };

FU2_EXPAND_QUALIFIERS(FU2_DEFINE_FUNCTION_TRAIT)
#undef FU2_DEFINE_FUNCTION_TRAIT

/// Deduces to the function pointer to the given signature
template <typename Signature>
using function_pointer_of = typename function_trait<Signature>::pointer_type;

template <typename... Args>
struct invoke_table;

/// We optimize the VTable in case there is a single function overload
template <typename First>
struct invoke_table<First> {
  using type = function_pointer_of<First>;

  /// Return the function pointer itself
  template <std::size_t Index>
  static constexpr auto fetch(type pointer) noexcept {
    static_assert(Index == 0U, "The index should be 0 here!");
    return pointer;
  }

  /// Returns the thunk of an single overloaded callable
  template <typename T, bool IsInplace>
  static constexpr type get_invocation_table_of() noexcept {
    return &function_trait<First>::template internal_invoker<T,
                                                             IsInplace>::invoke;
  }
  /// Returns the thunk of an empty single overloaded callable
  template <bool IsThrowing>
  static constexpr type get_empty_invocation_table() noexcept {
    return &function_trait<First>::template empty_invoker<IsThrowing>::invoke;
  }
};
/// We generate a table in case of multiple function overloads
template <typename First, typename Second, typename... Args>
struct invoke_table<First, Second, Args...> {
  using type =
  std::tuple<function_pointer_of<First>, function_pointer_of<Second>,
             function_pointer_of<Args>...> const*;

  /// Return the function pointer at the particular index
  template <std::size_t Index>
  static constexpr auto fetch(type table) noexcept {
    return std::get<Index>(*table);
  }

  /// The invocation vtable for a present object
  template <typename T, bool IsInplace>
  struct invocation_vtable : public std::tuple<function_pointer_of<First>,
                                               function_pointer_of<Second>,
                                               function_pointer_of<Args>...> {
    constexpr invocation_vtable() noexcept
        : std::tuple<function_pointer_of<First>, function_pointer_of<Second>,
                     function_pointer_of<Args>...>(std::make_tuple(
        &function_trait<First>::template internal_invoker<
            T, IsInplace>::invoke,
        &function_trait<Second>::template internal_invoker<
            T, IsInplace>::invoke,
        &function_trait<Args>::template internal_invoker<
            T, IsInplace>::invoke...)) {
    }
  };

  /// Returns the thunk of an multi overloaded callable
  template <typename T, bool IsInplace>
  static type get_invocation_table_of() noexcept {
    static invocation_vtable<T, IsInplace> const table;
    return &table;
  }

  /// The invocation table for an empty wrapper
  template <bool IsThrowing>
  struct empty_vtable : public std::tuple<function_pointer_of<First>,
                                          function_pointer_of<Second>,
                                          function_pointer_of<Args>...> {
    constexpr empty_vtable() noexcept
        : std::tuple<function_pointer_of<First>, function_pointer_of<Second>,
                     function_pointer_of<Args>...>(
        std::make_tuple(&function_trait<First>::template empty_invoker<
                            IsThrowing>::invoke,
                        &function_trait<Second>::template empty_invoker<
                            IsThrowing>::invoke,
                        &function_trait<Args>::template empty_invoker<
                            IsThrowing>::invoke...)) {
    }
  };

  /// Returns the thunk of an multi single overloaded callable
  template <bool IsThrowing>
  static type get_empty_invocation_table() noexcept {
    static empty_vtable<IsThrowing> const table;
    return &table;
  }
};

template <std::size_t Index, typename Function, typename... Signatures>
struct operator_impl;

#define FU2_DEFINE_FUNCTION_TRAIT(CONST, VOLATILE, OVL_REF, REF)               \
  template <std::size_t Index, typename Function, typename Ret,                \
            typename... Args, typename Next, typename... Signatures>           \
  struct operator_impl<Index, Function, Ret(Args...) CONST VOLATILE OVL_REF,   \
                       Next, Signatures...>                                    \
      : operator_impl<Index + 1, Function, Next, Signatures...> {              \
                                                                               \
    using operator_impl<Index + 1, Function, Next, Signatures...>::operator(); \
                                                                               \
    Ret operator()(Args... args) CONST VOLATILE OVL_REF {                      \
      auto function = static_cast<Function CONST VOLATILE*>(this);             \
      return erasure_attorney::invoke<Index>(                                  \
          static_cast<std::decay_t<decltype(function->erasure_)> CONST         \
                          VOLATILE REF>(function->erasure_),                   \
          std::forward<Args>(args)...);                                        \
    }                                                                          \
  };                                                                           \
  template <std::size_t Index, typename Function, typename Ret,                \
            typename... Args>                                                  \
  struct operator_impl<Index, Function, Ret(Args...) CONST VOLATILE OVL_REF> { \
                                                                               \
    Ret operator()(Args... args) CONST VOLATILE OVL_REF {                      \
      auto function = static_cast<Function CONST VOLATILE*>(this);             \
      return erasure_attorney::invoke<Index>(                                  \
          static_cast<std::decay_t<decltype(function->erasure_)> CONST         \
                          VOLATILE REF>(function->erasure_),                   \
          std::forward<Args>(args)...);                                        \
    }                                                                          \
  };

FU2_EXPAND_QUALIFIERS(FU2_DEFINE_FUNCTION_TRAIT)
#undef FU2_DEFINE_FUNCTION_TRAIT
#undef FU2_EXPAND_QUALIFIERS
} // namespace invocation_table

namespace tables {
/// Identifies the action which is dispatched on the erased object
enum class opcode {
  op_move,         //< Move the object and set the vtable
  op_copy,         //< Copy the object and set the vtable
  op_destroy,      //< Destroy the object and reset the vtable
  op_weak_destroy, //< Destroy the object without resetting the vtable
  op_fetch_empty,  //< Stores true or false into the to storage
  //< to indicate emptiness
};

/// Abstraction for a vtable together with a command table
/// TODO Add optimization for a single formal argument
/// TODO Add optimization to merge both tables if the function is size optimized
template <typename Property>
class vtable;
template <bool IsThrowing, bool HasStrongExceptGuarantee,
    typename... FormalArgs>
class vtable<property<IsThrowing, HasStrongExceptGuarantee, FormalArgs...>> {
  using command_function_t = void (*)(vtable* /*this*/, opcode /*op*/,
                                      data_accessor* /*from*/,
                                      std::size_t /*from_capacity*/,
                                      data_accessor* /*to*/,
                                      std::size_t /*to_capacity*/);

  using invoke_table_t = invocation_table::invoke_table<FormalArgs...>;

  command_function_t cmd_;
  typename invoke_table_t::type vtable_;

  template <typename T>
  struct trait {
    static_assert(is_box<T>::value,
                  "The trait must be specialized with a box!");

    /// The command table
    template <bool IsInplace>
    static void process_cmd(vtable* to_table, opcode op, data_accessor* from,
                            std::size_t from_capacity, data_accessor* to,
                            std::size_t to_capacity) {

      switch (op) {
        case opcode::op_move: {
          /// Retrieve the pointer to the object
          auto box = static_cast<T*>(retrieve<T>(
              std::integral_constant<bool, IsInplace>{}, from, from_capacity));
          assert(box && "The object must not be over aligned or null!");

          if (!IsInplace) {
            // Just swap both pointers if we allocated on the heap
            to->ptr_ = from->ptr_;

#ifndef _NDEBUG
            // We don't need to null the pointer since we know that
            // we don't own the data anymore through the vtable
            // which is set to empty.
            from->ptr_ = nullptr;
#endif

            to_table->template set_allocated<T>();

          }
            // The object is allocated inplace
          else {
            construct(std::true_type{}, std::move(*box), to_table, to,
                      to_capacity);
            box->~T();
          }
          return;
        }
        case opcode::op_copy: {
          auto box = static_cast<T const*>(retrieve<T>(
              std::integral_constant<bool, IsInplace>{}, from, from_capacity));
          assert(box && "The object must not be over aligned or null!");

          assert(std::is_copy_constructible<T>::value &&
                     "The box is required to be copyable here!");

          // Try to allocate the object inplace
          construct(std::is_copy_constructible<T>{}, *box, to_table, to,
                    to_capacity);
          return;
        }
        case opcode::op_destroy:
        case opcode::op_weak_destroy: {

          assert(!to && !to_capacity && "Arg overflow!");
          auto box = static_cast<T*>(retrieve<T>(
              std::integral_constant<bool, IsInplace>{}, from, from_capacity));

          if (IsInplace) {
            box->~T();
          } else {
            T::box_deallocate(box);
          }

          if (op == opcode::op_destroy) {
            to_table->set_empty();
          }
          return;
        }
        case opcode::op_fetch_empty: {
          write_empty(to, false);
          return;
        }
      }

      // TODO Use an unreachable intrinsic
      assert(false && "Unreachable!");
      std::exit(-1);
    }

    template <typename Box>
    static void
    construct(std::true_type /*apply*/, Box&& box, vtable* to_table,
              data_accessor* to,
              std::size_t to_capacity) noexcept(HasStrongExceptGuarantee) {
      // Try to allocate the object inplace
      void* storage = retrieve<T>(std::true_type{}, to, to_capacity);
      if (storage) {
        to_table->template set_inplace<T>();
      } else {
        // Allocate the object through the allocator
        to->ptr_ = storage = box.box_allocate();
        to_table->template set_allocated<T>();
      }
      new (storage) T(std::forward<Box>(box));
    }

    template <typename Box>
    static void
    construct(std::false_type /*apply*/, Box&& /*box*/, vtable* /*to_table*/,
              data_accessor* /*to*/,
              std::size_t /*to_capacity*/) noexcept(HasStrongExceptGuarantee) {
    }
  };

  /// The command table
  static void empty_cmd(vtable* to_table, opcode op, data_accessor* /*from*/,
                        std::size_t /*from_capacity*/, data_accessor* to,
                        std::size_t /*to_capacity*/) {

    switch (op) {
      case opcode::op_move:
      case opcode::op_copy: {
        to_table->set_empty();
        break;
      }
      case opcode::op_destroy:
      case opcode::op_weak_destroy: {
        // Do nothing
        break;
      }
      case opcode::op_fetch_empty: {
        write_empty(to, true);
        break;
      }
    }
  }

 public:
  vtable() noexcept = default;

  /// Initialize an object at the given position
  template <typename T>
  static void init(vtable& table, T&& object, data_accessor* to,
                   std::size_t to_capacity) {

    trait<std::decay_t<T>>::construct(std::true_type{}, std::forward<T>(object),
                                      &table, to, to_capacity);
  }

  /// Initializes the vtable object
  void init_empty() noexcept {
    // Initialize the new command function
    set_empty();
  }

  /// Moves the object at the given position
  void move(vtable& to_table, data_accessor* from, std::size_t from_capacity,
            data_accessor* to,
            std::size_t to_capacity) noexcept(HasStrongExceptGuarantee) {
    cmd_(&to_table, opcode::op_move, from, from_capacity, to, to_capacity);
    set_empty();
  }

  /// Destroys the object at the given position
  void copy(vtable& to_table, data_accessor const* from,
            std::size_t from_capacity, data_accessor* to,
            std::size_t to_capacity) const {
    cmd_(&to_table, opcode::op_copy, const_cast<data_accessor*>(from),
         from_capacity, to, to_capacity);
  }

  /// Destroys the object at the given position
  void destroy(data_accessor* from,
               std::size_t from_capacity) noexcept(HasStrongExceptGuarantee) {
    cmd_(this, opcode::op_destroy, from, from_capacity, nullptr, 0U);
  }

  /// Destroys the object at the given position without invalidating the
  /// vtable
  void
  weak_destroy(data_accessor* from,
               std::size_t from_capacity) noexcept(HasStrongExceptGuarantee) {
    cmd_(this, opcode::op_weak_destroy, from, from_capacity, nullptr, 0U);
  }

  /// Returns true when the vtable doesn't hold any erased object
  bool empty() const noexcept {
    data_accessor data;
    cmd_(nullptr, opcode::op_fetch_empty, nullptr, 0U, &data, 0U);
    return bool(data.inplace_storage_);
  }

  /// Invoke the function at the given index
  template <std::size_t Index, typename... Args>
  constexpr auto invoke(Args&&... args) const {
    auto thunk = invoke_table_t::template fetch<Index>(vtable_);
    return thunk(std::forward<Args>(args)...);
  }
  /// Invoke the function at the given index
  template <std::size_t Index, typename... Args>
  constexpr auto invoke(Args&&... args) const volatile {
    auto thunk = invoke_table_t::template fetch<Index>(vtable_);
    return thunk(std::forward<Args>(args)...);
  }

 private:
  template <typename T>
  void set_inplace() noexcept {
    using type = std::decay_t<T>;
    vtable_ = invoke_table_t::template get_invocation_table_of<type, true>();
    cmd_ = &trait<type>::template process_cmd<true>;
  }

  template <typename T>
  void set_allocated() noexcept {
    using type = std::decay_t<T>;
    vtable_ = invoke_table_t::template get_invocation_table_of<type, false>();
    cmd_ = &trait<type>::template process_cmd<false>;
  }

  void set_empty() noexcept {
    vtable_ = invoke_table_t::template get_empty_invocation_table<IsThrowing>();
    cmd_ = &empty_cmd;
  }
};
} // namespace tables

/// A union which makes the pointer to the heap object share the
/// same space with the internal capacity.
/// The storage type is distinguished by multiple versions of the
/// control and vtable.
template <std::size_t Capacity, typename = void>
struct internal_capacity {
  /// We extend the union through a technique similar to the tail object hack
  typedef union {
    /// Tag to access the structure in a type-safe way
    data_accessor accessor_;
    /// The internal capacity we use to allocate in-place
    std::aligned_storage_t<Capacity> capacity_;
  } type;
};
template <std::size_t Capacity>
struct internal_capacity<Capacity,
                         std::enable_if_t<(Capacity < sizeof(void*))>> {
  typedef struct {
    /// Tag to access the structure in a type-safe way
    data_accessor accessor_;
  } type;
};

template <std::size_t Capacity>
class internal_capacity_holder {
  // Tag to access the structure in a type-safe way
  typename internal_capacity<Capacity>::type storage_;

 public:
  constexpr internal_capacity_holder() = default;

  constexpr data_accessor* opaque_ptr() noexcept {
    return &storage_.accessor_;
  }
  constexpr data_accessor const* opaque_ptr() const noexcept {
    return &storage_.accessor_;
  }
  constexpr data_accessor volatile* opaque_ptr() volatile noexcept {
    return &storage_.accessor_;
  }
  constexpr data_accessor const volatile* opaque_ptr() const volatile noexcept {
    return &storage_.accessor_;
  }

  static constexpr std::size_t capacity() noexcept {
    return sizeof(storage_);
  }
};

/// A copyable owning erasure
template <typename Config, typename Property>
class erasure : internal_capacity_holder<Config::capacity> {
  friend struct erasure_attorney;

  template <typename, typename>
  friend class erasure;

  using VTable = tables::vtable<Property>;

  VTable vtable_;

 public:
  /// Returns the capacity of this erasure
  static constexpr std::size_t capacity() noexcept {
    return internal_capacity_holder<Config::capacity>::capacity();
  }

  constexpr erasure() noexcept {
    vtable_.init_empty();
  }

  constexpr erasure(std::nullptr_t) noexcept {
    vtable_.init_empty();
  }

  constexpr erasure(erasure&& right) noexcept(
  Property::is_strong_exception_guaranteed) {
    right.vtable_.move(vtable_, right.opaque_ptr(), right.capacity(),
                       this->opaque_ptr(), capacity());
  }

  constexpr erasure(erasure const& right) {
    right.vtable_.copy(vtable_, right.opaque_ptr(), right.capacity(),
                       this->opaque_ptr(), capacity());
  }

  template <typename OtherConfig>
  constexpr erasure(erasure<OtherConfig, Property> right) noexcept(
  Property::is_strong_exception_guaranteed) {
    right.vtable_.move(vtable_, right.opaque_ptr(), right.capacity(),
                       this->opaque_ptr(), capacity());
  }

  template <typename T,
      std::enable_if_t<is_box<std::decay_t<T>>::value>* = nullptr>
  constexpr erasure(T&& object) {
    VTable::init(vtable_, std::forward<T>(object), this->opaque_ptr(),
                 capacity());
  }

  ~erasure() {
    vtable_.weak_destroy(this->opaque_ptr(), capacity());
  }

  constexpr erasure&
  operator=(std::nullptr_t) noexcept(Property::is_strong_exception_guaranteed) {
    vtable_.destroy(this->opaque_ptr(), capacity());
    return *this;
  }

  constexpr erasure& operator=(erasure&& right) noexcept(
  Property::is_strong_exception_guaranteed) {
    vtable_.weak_destroy(this->opaque_ptr(), capacity());
    right.vtable_.move(vtable_, right.opaque_ptr(), right.capacity(),
                       this->opaque_ptr(), capacity());
    return *this;
  }

  constexpr erasure& operator=(erasure const& right) {
    vtable_.weak_destroy(this->opaque_ptr(), capacity());
    right.vtable_.copy(vtable_, right.opaque_ptr(), right.capacity(),
                       this->opaque_ptr(), capacity());
    return *this;
  }

  template <typename OtherConfig>
  constexpr erasure& operator=(erasure<OtherConfig, Property> right) noexcept(
  Property::is_strong_exception_guaranteed) {
    vtable_.weak_destroy(this->opaque_ptr(), capacity());
    right.vtable_.move(vtable_, right.opaque_ptr(), right.capacity(),
                       this->opaque_ptr(), capacity());
    return *this;
  }

  template <typename T,
      std::enable_if_t<is_box<std::decay_t<T>>::value>* = nullptr>
  constexpr erasure& operator=(T&& object) {
    vtable_.weak_destroy(this->opaque_ptr(), capacity());
    VTable::init(vtable_, std::forward<T>(object), this->opaque_ptr(),
                 capacity());
    return *this;
  }

  /// Returns true when the erasure doesn't hold any erased object
  constexpr bool empty() const noexcept {
    return vtable_.empty();
  }
};
} // namespace type_erasure

/// Deduces to a true_type if the type T provides the given signature
template <typename T, typename Signature,
    typename trait =
    type_erasure::invocation_table::function_trait<Signature>>
struct accepts_one
    : invocation::can_invoke<typename trait::template callable<T>,
                             typename trait::arguments> {};

/// Deduces to a true_type if the type T provides all signatures
template <typename T, typename Signatures, typename = void>
struct accepts_all : std::false_type {};
template <typename T, typename... Signatures>
struct accepts_all<
    T, identity<Signatures...>,
    void_t<std::enable_if_t<accepts_one<T, Signatures>::value>...>>
    : std::true_type {};

template <typename Config, typename T>
struct assert_wrong_copy_assign {
  static_assert(!Config::is_copyable ||
                    std::is_copy_constructible<std::decay_t<T>>::value,
                "Can't wrap a non copyable object into a unique function!");

  using type = void;
};

template <bool IsStrongExceptGuaranteed, typename T>
struct assert_no_strong_except_guarantee {
  static_assert(
      !IsStrongExceptGuaranteed ||
          (std::is_nothrow_move_constructible<T>::value &&
              std::is_nothrow_destructible<T>::value),
      "Can't wrap a object an object that has no strong exception guarantees "
      "if this is required by the wrapper!");

  using type = void;
};

/// SFINAES out if the given callable is not copyable correct to the left one.
template <typename LeftConfig, typename RightConfig>
using enable_if_copyable_correct_t =
std::enable_if_t<(!LeftConfig::is_copyable || RightConfig::is_copyable)>;

template <typename Config, typename Property>
class function;

template <typename Config, bool IsThrowing, bool HasStrongExceptGuarantee,
    typename... Args>
class function<Config, property<IsThrowing, HasStrongExceptGuarantee, Args...>>
    : public type_erasure::invocation_table::operator_impl<
        0U,
        function<Config,
                 property<IsThrowing, HasStrongExceptGuarantee, Args...>>,
        Args...>,
      public copyable<Config::is_copyable> {

  template <typename, typename>
  friend class function;

  template <std::size_t, typename, typename...>
  friend struct type_erasure::invocation_table::operator_impl;

  using my_property = property<IsThrowing, HasStrongExceptGuarantee, Args...>;

  template <typename T>
  using enable_if_can_accept_all_t =
  std::enable_if_t<accepts_all<std::decay_t<T>, identity<Args...>>::value>;

  template <typename Function>
  struct is_convertible_to_this : std::false_type {};
  template <typename RightConfig>
  struct is_convertible_to_this<function<RightConfig, my_property>>
      : std::true_type {};

  template <typename T>
  using enable_if_not_convertible_to_this =
  std::enable_if_t<!is_convertible_to_this<std::decay_t<T>>::value>;

  template <typename T>
  using assert_wrong_copy_assign_t =
  typename assert_wrong_copy_assign<Config, std::decay_t<T>>::type;

  template <typename T>
  using assert_no_strong_except_guarantee_t =
  typename assert_no_strong_except_guarantee<HasStrongExceptGuarantee,
                                             std::decay_t<T>>::type;

  type_erasure::erasure<Config, my_property> erasure_;

 public:
  /// Default constructor which constructs the function empty
  function() = default;

  explicit constexpr function(function const& /*right*/) = default;
  explicit constexpr function(function&& /*right*/) = default;

  /// Copy construction from another copyable function
  template <typename RightConfig,
      std::enable_if_t<RightConfig::is_copyable>* = nullptr,
      enable_if_copyable_correct_t<Config, RightConfig>* = nullptr>
  constexpr function(function<RightConfig, my_property> const& right)
      : erasure_(right.erasure_) {
  }

  /// Move construction from another function
  template <typename RightConfig,
      enable_if_copyable_correct_t<Config, RightConfig>* = nullptr>
  constexpr function(function<RightConfig, my_property>&& right)
      : erasure_(std::move(right.erasure_)) {
  }

  /// Construction from a callable object which overloads the `()` operator
  template <typename T, typename Allocator = std::allocator<std::decay_t<T>>,
      enable_if_not_convertible_to_this<T>* = nullptr,
      enable_if_can_accept_all_t<T>* = nullptr,
      assert_wrong_copy_assign_t<T>* = nullptr,
      assert_no_strong_except_guarantee_t<T>* = nullptr>
  constexpr function(T callable, Allocator&& allocator = Allocator{})
      : erasure_(type_erasure::make_box(std::forward<T>(callable),
                                        std::forward<Allocator>(allocator))) {
  }

  /// Empty constructs the function
  constexpr function(std::nullptr_t np) : erasure_(np) {
  }

  function& operator=(function const& /*right*/) = default;
  function& operator=(function&& /*right*/) = default;

  /// Copy assigning from another copyable function
  template <typename RightConfig,
      std::enable_if_t<RightConfig::is_copyable>* = nullptr,
      enable_if_copyable_correct_t<Config, RightConfig>* = nullptr>
  function& operator=(function<RightConfig, my_property> const& right) {
    erasure_ = right.erasure_;
    return *this;
  }

  /// Move assigning from another function
  template <typename RightConfig,
      enable_if_copyable_correct_t<Config, RightConfig>* = nullptr>
  function& operator=(function<RightConfig, my_property>&& right) {
    erasure_ = std::move(right.erasure_);
    return *this;
  }

  /// Move assigning from a callable object
  template <typename T, // ...
      enable_if_not_convertible_to_this<T>* = nullptr,
      enable_if_can_accept_all_t<T>* = nullptr,
      assert_wrong_copy_assign_t<T>* = nullptr,
      assert_no_strong_except_guarantee_t<T>* = nullptr>
  function& operator=(T&& callable) {
    erasure_ = type_erasure::make_box(std::forward<T>(callable));
    return *this;
  }

  /// Clears the function
  function& operator=(std::nullptr_t np) {
    erasure_ = np;
    return *this;
  }

  /// Returns true when the function is empty
  bool empty() const noexcept {
    return erasure_.empty();
  }

  /// Returns true when the function isn't empty
  explicit operator bool() const noexcept {
    return !empty();
  }

  /// Assigns a new target with an optional allocator
  template <typename T, typename Allocator = std::allocator<std::decay_t<T>>,
      enable_if_not_convertible_to_this<T>* = nullptr,
      enable_if_can_accept_all_t<T>* = nullptr,
      assert_wrong_copy_assign_t<T>* = nullptr,
      assert_no_strong_except_guarantee_t<T>* = nullptr>
  void assign(T&& callable, Allocator&& allocator = Allocator{}) {
    erasure_ = type_erasure::make_box(std::forward<T>(callable),
                                      std::forward<Allocator>(allocator));
  }

  /// Swaps this function with the given function
  void swap(function& other) noexcept(HasStrongExceptGuarantee) {
    if (&other == this) {
      return;
    }

    function cache = std::move(other);
    other = std::move(*this);
    *this = std::move(cache);
  }

  /// Swaps the left function with the right one
  friend void swap(function& left,
                   function& right) noexcept(HasStrongExceptGuarantee) {
    left.swap(right);
  }

  /// Calls the wrapped callable object
  using type_erasure::invocation_table::operator_impl<
      0U, function<Config, my_property>, Args...>::operator();
};

template <typename Config, typename Property>
bool operator==(function<Config, Property> const& f, std::nullptr_t) {
  return !bool(f);
}

template <typename Config, typename Property>
bool operator!=(function<Config, Property> const& f, std::nullptr_t) {
  return bool(f);
}

template <typename Config, typename Property>
bool operator==(std::nullptr_t, function<Config, Property> const& f) {
  return !bool(f);
}

template <typename Config, typename Property>
bool operator!=(std::nullptr_t, function<Config, Property> const& f) {
  return bool(f);
}

// Internal size of an empty function object
using empty_size = std::integral_constant<
    std::size_t, sizeof(function<detail::config<true, true, 0UL>,
                                 detail::property<true, false, void() const>>)>;

// Default capacity for small functor optimization
using default_capacity = std::integral_constant<
    std::size_t,
    // Aim to size the function object to 32UL
    (empty_size::value < 32UL) ? (32UL - empty_size::value) : 16UL>;
} // namespace detail
} // namespace v5

/// Adaptable function wrapper base for arbitrary functional types.
template <
    /// This is a placeholder for future non owning support
    bool IsOwning,
    /// Defines whether the function is copyable or not
    bool IsCopyable,
    /// Defines the internal capacity of the function
    /// for small functor optimization.
    std::size_t Capacity,
    /// Defines whether the function throws an exception on empty function call,
    /// `std::abort` is called otherwise.
    bool IsThrowing,
    /// Defines whether all objects satisfy the strong exception guarantees,
    /// which means the function type will satisfy the strong exception
    /// guarantees too.
    bool HasStrongExceptGuarantee,
    /// Defines the signature of the function wrapper
    typename... Signatures>
using function_base = detail::function<
    detail::config<IsOwning, IsCopyable, Capacity>,
    detail::property<IsThrowing, HasStrongExceptGuarantee, Signatures...>>;

/// Copyable function wrapper for arbitrary functional types.
template <typename... Signatures>
using function = function_base<true, true, detail::default_capacity::value,
                               true, false, Signatures...>;

/// Non copyable function wrapper for arbitrary functional types.
template <typename... Signatures>
using unique_function =
function_base<true, false, detail::default_capacity::value, true, false,
              Signatures...>;

#if !defined(FU2_MACRO_DISABLE_EXCEPTIONS)
/// Exception type that is thrown when invoking empty function objects
/// and exception support isn't disabled.
///
/// Exception suport is enabled if
/// the template parameter 'Throwing' is set to true (default).
///
/// This type will default to std::bad_function_call if the
/// functional header is used, otherwise the library provides its own type.
///
/// You may disable the inclusion of the functionl header
/// through defining `FU2_NO_FUNCTIONAL_HEADER`.
///
using detail::type_erasure::invocation_table::bad_function_call;
#endif

/// Returns a callable object, which unifies all callable objects
/// that were passed to this function.
///
///   ```cpp
///   auto overloaded = fu2::overload([](std::true_type) { return true; },
///                                   [](std::false_type) { return false; });
///   ```
///
/// \param  callables A pack of callable objects with arbitrary signatures.
///
/// \returns          A callable object which exposes the
///
template <typename... T>
constexpr auto overload(T&&... callables) {
  return detail::overloading::overload(std::forward<T>(callables)...);
}
} // namespace fu2

#undef FU2_MACRO_DISABLE_EXCEPTIONS

#endif // FU2_INCLUDED_FUNCTION2_HPP__


// #include <continuable/continuable-trait.hpp>


namespace cti {
/// \defgroup Types Types
/// provides the \link cti::continuable continuable\endlink and \link
/// cti::promise promise\endlink facility for type erasure.
/// \{

// clang-format off
namespace detail {
/// A function which isn't size adjusted and move only
template<std::size_t, typename... Args>
using unique_function_adapter = fu2::unique_function<Args...>;
/// A function which is size adjusted and move only
template<std::size_t Size, typename... Args>
using unique_function_adjustable = fu2::function_base<true, false, Size,
                                                      true, false, Args...>;

/// We adjust the internal capacity of the outer function wrapper so
/// we don't have to allocate twice when using `continuable<...>`.
template<typename... Args>
using unique_trait_of = continuable_trait<
    unique_function_adapter,
    unique_function_adjustable,
    Args...
>;
} // namespace detail

/// Defines a non-copyable continuation type which uses the
/// function2 backend for type erasure.
///
/// Usable like: `continuable<int, float>`
template <typename... Args>
using continuable = typename detail::unique_trait_of<
    Args...
>::continuable;

/// Defines a non-copyable promise type which is using the
/// function2 backend for type erasure.
///
/// Usable like: `promise<int, float>`
template <typename... Args>
using promise = typename detail::unique_trait_of<
    Args...
>::promise;

// TODO channel
// TODO sink

// clang-format on
/// \}
} // namespace cti

#endif // CONTINUABLE_TYPES_HPP_INCLUDED


#endif // CONTINUABLE_HPP_INCLUDED
