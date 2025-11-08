#pragma once

#include "objectir_runtime.hpp"
#include <fstream>
#include <iostream>
#include <string>
#include <vector>
#include <memory>

namespace ObjectIR {

// ============================================================================
// System.IO.Stream - Base class for stream operations
// ============================================================================

Value Stream_Dispose(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value Stream_CanRead(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value Stream_CanWrite(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value Stream_CanSeek(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value Stream_Length(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value Stream_Position(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value Stream_SetPosition(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value Stream_Read(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value Stream_Write(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value Stream_Flush(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value Stream_Close(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);

// ============================================================================
// System.IO.FileStream - File-based stream implementation
// ============================================================================

Value FileStream_ctor(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value FileStream_Dispose(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value FileStream_CanRead(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value FileStream_CanWrite(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value FileStream_CanSeek(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value FileStream_Length(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value FileStream_Position(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value FileStream_SetPosition(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value FileStream_Read(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value FileStream_Write(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value FileStream_Flush(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value FileStream_Close(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);

// ============================================================================
// System.IO.StreamReader - Text reader for streams
// ============================================================================

Value StreamReader_ctor(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value StreamReader_ReadLine(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value StreamReader_ReadToEnd(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value StreamReader_Close(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);

// ============================================================================
// System.IO.StreamWriter - Text writer for streams
// ============================================================================

Value StreamWriter_ctor(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value StreamWriter_Write(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value StreamWriter_WriteLine(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value StreamWriter_Flush(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value StreamWriter_Close(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);

// ============================================================================
// System.IO.File - Static file operations
// ============================================================================

Value File_Exists(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value File_ReadAllText(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value File_WriteAllText(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value File_ReadAllLines(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value File_WriteAllLines(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value File_Delete(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);

} // namespace ObjectIR