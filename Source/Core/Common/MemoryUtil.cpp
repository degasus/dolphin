// Copyright 2008 Dolphin Emulator Project
// Licensed under GPLv2+
// Refer to the license.txt file included.

#include <cstddef>
#include <cstdlib>
#include <string>

#include "Common/CommonFuncs.h"
#include "Common/CommonTypes.h"
#include "Common/Logging/Log.h"
#include "Common/MemoryUtil.h"
#include "Common/MsgHandler.h"

#ifdef _WIN32
#include <psapi.h>
#include <windows.h>
#include "Common/StringUtil.h"
#else
#include <stdio.h>
#include <sys/mman.h>
#include <sys/types.h>
#if defined __APPLE__ || defined __FreeBSD__ || defined __OpenBSD__
#include <sys/sysctl.h>
#else
#include <sys/sysinfo.h>
#endif
#endif

// Valgrind doesn't support MAP_32BIT.
// Uncomment the following line to be able to run Dolphin in Valgrind.
//#undef MAP_32BIT

namespace Common
{
#if !defined(_WIN32) && defined(_M_X86_64) && !defined(MAP_32BIT)
#include <unistd.h>
static uintptr_t RoundPage(uintptr_t addr)
{
  uintptr_t mask = getpagesize() - 1;
  return (addr + mask) & ~mask;
}
#endif

void* AllocateMemoryPages(size_t size)
{
#ifdef _WIN32
  void* ptr = VirtualAlloc(0, size, MEM_COMMIT, PAGE_READWRITE);
#else
  void* ptr = mmap(nullptr, size, PROT_READ | PROT_WRITE, MAP_ANON | MAP_PRIVATE, -1, 0);

  if (ptr == MAP_FAILED)
    ptr = nullptr;
#endif

  if (ptr == nullptr)
    PanicAlert("Failed to allocate raw memory");

  return ptr;
}

void* AllocateAlignedMemory(size_t size, size_t alignment)
{
#ifdef _WIN32
  void* ptr = _aligned_malloc(size, alignment);
#else
  void* ptr = nullptr;
  if (posix_memalign(&ptr, alignment, size) != 0)
    ERROR_LOG(MEMMAP, "Failed to allocate aligned memory");
#endif

  if (ptr == nullptr)
    PanicAlert("Failed to allocate aligned memory");

  return ptr;
}

void FreeMemoryPages(void* ptr, size_t size)
{
  if (ptr)
  {
    bool error_occurred = false;

#ifdef _WIN32
    if (!VirtualFree(ptr, 0, MEM_RELEASE))
      error_occurred = true;
#else
    int retval = munmap(ptr, size);

    if (retval != 0)
      error_occurred = true;
#endif

    if (error_occurred)
      PanicAlert("FreeMemoryPages failed!\n%s", GetLastErrorMsg().c_str());
  }
}

void FreeAlignedMemory(void* ptr)
{
  if (ptr)
  {
#ifdef _WIN32
    _aligned_free(ptr);
#else
    free(ptr);
#endif
  }
}

#ifdef _WIN32
static DWORD GetW32ProtectMode(bool read, bool write, bool exec)
{
  switch ((read << 0) | (write << 1) | (exec << 2))
  {
  case 0:
    return PAGE_NOACCESS;
  case 1:
    return PAGE_READONLY;
  case 2:
    return PAGE_NOACCESS;  // write-only is not supported
  case 3:
    return PAGE_READWRITE;
  case 4:
    return PAGE_EXECUTE;
  case 5:
    return PAGE_EXECUTE_READ;
  case 6:
    return PAGE_NOACCESS;  // write-only is not supported
  case 7:
    return PAGE_EXECUTE_READWRITE;
  }
  return 0;
}
#endif

bool MemProtect(void* ptr, size_t size, bool read, bool write, bool exec)
{
  bool error_occurred = false;
#ifdef _WIN32
  DWORD newValue = GetW32ProtectMode(read, write, exec);
  DWORD oldValue;
  if (!VirtualProtect(ptr, size, newValue, &oldValue))
    error_occurred = true;
#else
  int prot = (read ? PROT_READ : 0) | (write ? PROT_WRITE : 0) | (exec ? PROT_EXEC : 0);
  int retval = mprotect(ptr, size, prot);

  if (retval != 0)
    error_occurred = true;
#endif

  if (error_occurred)
    PanicAlert("MemProtect failed for %p with size 0x%zx!\n%s", ptr, size,
               GetLastErrorMsg().c_str());

  return error_occurred;
}

std::string MemUsage()
{
#ifdef _WIN32
#pragma comment(lib, "psapi")
  DWORD processID = GetCurrentProcessId();
  HANDLE hProcess;
  PROCESS_MEMORY_COUNTERS pmc;
  std::string Ret;

  // Print information about the memory usage of the process.

  hProcess = OpenProcess(PROCESS_QUERY_INFORMATION | PROCESS_VM_READ, FALSE, processID);
  if (nullptr == hProcess)
    return "MemUsage Error";

  if (GetProcessMemoryInfo(hProcess, &pmc, sizeof(pmc)))
    Ret = StringFromFormat("%s K", ThousandSeparate(pmc.WorkingSetSize / 1024, 7).c_str());

  CloseHandle(hProcess);
  return Ret;
#else
  return "";
#endif
}

size_t MemPhysical()
{
#ifdef _WIN32
  MEMORYSTATUSEX memInfo;
  memInfo.dwLength = sizeof(MEMORYSTATUSEX);
  GlobalMemoryStatusEx(&memInfo);
  return memInfo.ullTotalPhys;
#elif defined __APPLE__ || defined __FreeBSD__ || defined __OpenBSD__
  int mib[2];
  size_t physical_memory;
  mib[0] = CTL_HW;
#ifdef __APPLE__
  mib[1] = HW_MEMSIZE;
#elif defined __FreeBSD__
  mib[1] = HW_REALMEM;
#elif defined __OpenBSD__
  mib[1] = HW_PHYSMEM;
#endif
  size_t length = sizeof(size_t);
  sysctl(mib, 2, &physical_memory, &length, NULL, 0);
  return physical_memory;
#else
  struct sysinfo memInfo;
  sysinfo(&memInfo);
  return (size_t)memInfo.totalram * memInfo.mem_unit;
#endif
}

}  // namespace Common
