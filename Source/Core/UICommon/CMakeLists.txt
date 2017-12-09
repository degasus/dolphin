set(SRCS
  CommandLineParse.cpp
  Disassembler.cpp
  UICommon.cpp
  USBUtils.cpp
  VideoUtils.cpp
)

if(USE_X11)
  set(SRCS ${SRCS} X11Utils.cpp)
endif()

set(LIBS common cpp-optparse)
if(LIBUSB_FOUND)
  set(LIBS ${LIBS} ${LIBUSB_LIBRARIES})
endif()

add_dolphin_library(uicommon "${SRCS}" "${LIBS}")

if(ENABLE_LLVM AND LLVM_FOUND)
  target_link_libraries(uicommon PRIVATE LLVM)
endif()
