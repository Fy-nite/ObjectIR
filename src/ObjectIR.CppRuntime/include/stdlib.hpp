#pragma once

#include "objectir_runtime.hpp"
#include <memory>

namespace ObjectIR {

/// Registers built-in standard library types and methods with the virtual machine
/// This includes System.Console, System.String, System.Convert, and related methods
void RegisterStandardLibrary(std::shared_ptr<VirtualMachine> vm);

} // namespace ObjectIR
