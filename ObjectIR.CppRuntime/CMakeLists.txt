cmake_minimum_required(VERSION 3.16)
project(ObjectIR.CppRuntime VERSION 1.0.0 LANGUAGES CXX)

# Set C++ standard
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Add the library
add_library(objectir_runtime 
    src/objectir_runtime.cpp
)

# Public include directory
target_include_directories(objectir_runtime PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}/include
)

# Set library properties
set_target_properties(objectir_runtime PROPERTIES
    VERSION ${PROJECT_VERSION}
    SOVERSION 1
)

# Enable testing
enable_testing()

# Add example executables
add_executable(calculator_example examples/calculator_example.cpp)
target_link_libraries(calculator_example PRIVATE objectir_runtime)

add_executable(todoapp_example examples/todoapp_example.cpp)
target_link_libraries(todoapp_example PRIVATE objectir_runtime)

# Install configuration
install(TARGETS objectir_runtime
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
)

install(DIRECTORY include/ DESTINATION include)
