if (POLICY CMP0025)
  cmake_policy(SET CMP0025 NEW)
endif ()

cmake_minimum_required(VERSION 3.12)

find_program(CCACHE_FOUND ccache)
if(CCACHE_FOUND)
  set_property(GLOBAL PROPERTY RULE_LAUNCH_COMPILE ccache)
  set_property(GLOBAL PROPERTY RULE_LAUNCH_LINK ccache)
endif(CCACHE_FOUND)

set(
    CMAKE_TOOLCHAIN_FILE
    "${CMAKE_SOURCE_DIR}/cmake/toolchain-cxx17.cmake"
    CACHE
    FILEPATH
    "Default toolchain"
)

include("cmake/3rdparty/Hunter/init.cmake")
HunterGate(
    URL "https://github.com/ruslo/hunter/archive/v0.23.169.tar.gz"
    SHA1 "60de9f275143ab38df559986c04e3726683e7de7"
    FILEPATH ${CMAKE_SOURCE_DIR}/cmake/3rdparty/Hunter/config.cmake
)

project(continuable_by_example)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

include(CheckCXXCompilerFlag)
include(cmake/dependencies.cmake)
include(cmake/functions.cmake)

include_directories(
    SYSTEM
    include
)

#add_executable(example1
#    echo.cpp
#    )
#target_link_libraries(example1
#    Boost::boost
#    )


add_executable(fiber
    fiber.cpp
    )
target_link_libraries(fiber
    Boost::boost
    Boost::fiber
    Boost::context
    )

add_executable(fiber_yield
    fiber_yield.cpp
    )
target_link_libraries(fiber_yield
    Boost::boost
    Boost::fiber
    Boost::context
    )

#add_executable(example3
#    coroutine.cpp
#    )
#target_link_libraries(example3
#    Boost::boost
#    Boost::coroutine
#    )
