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

### TODO add similar MSVC warning flags
if (   "${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang"
    OR "${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU")
  set_source_files_properties (${SRC_CORE} ${SRC_TEST}
    PROPERTIES COMPILE_FLAGS "-Werror -Wall -Wextra -pedantic")
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

### Avoid cpp runtime dependency
### TODO find out the equivalent for msvc
if (   "${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang"
    OR "${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU")
  set_target_properties (arraystore
    PROPERTIES COMPILE_FLAGS "-fno-rtti -fno-exceptions")
endif ()

           
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
