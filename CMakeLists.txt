cmake_minimum_required (VERSION 3.0)
project (arraystore)

find_package (Threads REQUIRED)


##  ----------------------------------------------------------------------
##  Compiler settings

set (CMAKE_CXX_STANDARD 11)
set (CMAKE_C_STANDARD   99)


##  ----------------------------------------------------------------------
##  Bundled LMDB dependency

set (LMDB_DIR libs/lmdb/libraries/liblmdb)

set (SRC_LMDB ${LMDB_DIR}/mdb.c
              ${LMDB_DIR}/midl.c)

### mingw/msys pthreads impl doesn't support robust mode
if (MINGW OR MSYS)
  set_source_files_properties (${SRC_LMDB}
    PROPERTIES COMPILE_DEFINITIONS MDB_USE_ROBUST=0)
endif ()
        

##  ----------------------------------------------------------------------
##  Our code

set (SRC_CORE src/impl.cpp)
set (SRC_TEST src/tests.cpp)

### More warnings, and avoid cpp runtime dependency
### TODO add similar MSVC flags
if (   "${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang"
    OR "${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU")
  set_source_files_properties (${SRC_CORE} ${SRC_TEST}
    PROPERTIES COMPILE_FLAGS
        "-Werror -Wall -Wextra -pedantic -fno-rtti -fno-exceptions")
endif ()


##  ----------------------------------------------------------------------
##  Exported library

add_library (arraystore ${SRC_CORE} ${SRC_LMDB})

target_include_directories (arraystore
  PUBLIC include
  PRIVATE ${LMDB_DIR})

target_link_libraries (arraystore
  PUBLIC ${CMAKE_THREAD_LIBS_INIT})

set_target_properties (arraystore
  PROPERTIES PUBLIC_HEADER include/arraystore.h
             POSITION_INDEPENDENT_CODE ON)

           
##  ----------------------------------------------------------------------
##  System-wide installation

include (GNUInstallDirs)

install (
  TARGETS arraystore
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
  PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})


##  ----------------------------------------------------------------------
##  Project tests

add_executable (run_tests ${SRC_TEST})

target_link_libraries (run_tests PRIVATE arraystore)
target_include_directories (run_tests
  PRIVATE ${LMDB_DIR})


##  ----------------------------------------------------------------------
##  Easy running of project tests

### Path to scratch DB
set (TEST_DB_PATH ${CMAKE_BINARY_DIR}/TEST_SCRATCH_DB.db)

### Run all project tests
add_custom_target (check
  DEPENDS run_tests
  COMMAND rm -f ${TEST_DB_PATH} ${TEST_DB_PATH}-lock &&
          ./run_tests ${TEST_DB_PATH}
)

### Run all project tests under valgrind
add_custom_target (memcheck
  DEPENDS run_tests
  COMMAND rm -f ${TEST_DB_PATH} ${TEST_DB_PATH}-lock &&
          valgrind --leak-check=full ./run_tests ${TEST_DB_PATH}
)


##  ----------------------------------------------------------------------
##  C++ bindings (header only)

### The library itself

add_library (arraystorexx INTERFACE)
target_include_directories (arraystorexx
  INTERFACE bindings/cpp/include)
target_link_libraries (arraystorexx
  INTERFACE arraystore)

### Self tests

add_executable (run_tests_cpp bindings/cpp/src/run_tests.cpp)
target_link_libraries (run_tests_cpp
  PRIVATE arraystorexx)
target_include_directories (run_tests_cpp
  PRIVATE ${LMDB_DIR})  # tests use lmdb funs directly

### Running self tests

set (CPP_TEST_DB_PATH ${CMAKE_BINARY_DIR}/CPP_TEST_SCRATCH_DB.db)

add_custom_target (cpp-memcheck
  DEPENDS run_tests_cpp
  COMMAND rm -f ${CPP_TEST_DB_PATH} ${CPP_TEST_DB_PATH}-lock &&
          valgrind --leak-check=full ./run_tests_cpp ${CPP_TEST_DB_PATH})

