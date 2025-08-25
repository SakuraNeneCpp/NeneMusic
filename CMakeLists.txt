cmake_minimum_required(VERSION 3.16)

project(pm VERSION 0.1.0 LANGUAGES CXX)

# Options
option(PM_ENABLE_SANITIZERS "Enable Address/UB sanitizers in Debug builds (GCC/Clang)" ON)
option(PM_CLANG_TIDY "Enable clang-tidy if available" OFF)
option(PM_ENABLE_LTO "Enable link-time optimization in Release" OFF)

# Export compile_commands.json for IntelliSense
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Executable
add_executable(pm pm.cpp)

# C++ standard & output dir
set_target_properties(pm PROPERTIES
  CXX_STANDARD 20
  CXX_STANDARD_REQUIRED YES
  CXX_EXTENSIONS NO
  RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin"
)

# Warnings and platform specifics
if(MSVC)
  target_compile_options(pm PRIVATE /W4 /permissive- /utf-8)
  target_compile_definitions(pm PRIVATE WIN32_LEAN_AND_MEAN _CRT_SECURE_NO_WARNINGS)
else()
  target_compile_options(pm PRIVATE -Wall -Wextra -Wpedantic)
  # For even stricter builds, uncomment:
  # target_compile_options(pm PRIVATE -Wconversion -Wsign-conversion)
endif()

# Sanitizers (non-MSVC)
if(PM_ENABLE_SANITIZERS AND NOT MSVC)
  target_compile_options(pm PRIVATE -fsanitize=address,undefined)
  target_link_options(pm PRIVATE -fsanitize=address,undefined)
endif()

# Optional LTO/IPO
if(PM_ENABLE_LTO)
  include(CheckIPOSupported)
  check_ipo_supported(RESULT ipo_ok OUTPUT ipo_msg)
  if(ipo_ok)
    set_property(TARGET pm PROPERTY INTERPROCEDURAL_OPTIMIZATION_RELEASE TRUE)
  else()
    message(WARNING "IPO/LTO not supported: ${ipo_msg}")
  endif()
endif()

# Optional clang-tidy
if(PM_CLANG_TIDY)
  find_program(CLANG_TIDY_EXE NAMES clang-tidy)
  if(CLANG_TIDY_EXE)
    set_target_properties(pm PROPERTIES CXX_CLANG_TIDY "${CLANG_TIDY_EXE}")
  endif()
endif()

# GCC 8 and older require stdc++fs for <filesystem>
if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU" AND CMAKE_CXX_COMPILER_VERSION VERSION_LESS 9)
  target_link_libraries(pm PRIVATE stdc++fs)
endif()

# Default build type
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  set(CMAKE_BUILD_TYPE Release CACHE STRING "Build type" FORCE)
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release" "RelWithDebInfo" "MinSizeRel")
endif()

# Install
include(GNUInstallDirs)
install(TARGETS pm RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
