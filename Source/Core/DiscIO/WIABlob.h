// Copyright 2018 Dolphin Emulator Project
// Licensed under GPLv2+
// Refer to the license.txt file included.

#pragma once

#include <array>
#include <memory>

#include "Common/CommonTypes.h"
#include "Common/File.h"
#include "Common/Swap.h"
#include "DiscIO/Blob.h"

namespace DiscIO
{
constexpr u32 WIA_MAGIC = 0x01414957;  // "WIA\x1" (byteswapped to little endian)

class WIAFileReader : public BlobReader
{
public:
  ~WIAFileReader();

  static std::unique_ptr<WIAFileReader> Create(File::IOFile file, const std::string& path);

  BlobType GetBlobType() const override { return BlobType::WIA; }
  u64 GetRawSize() const override { return Common::swap64(m_header_1.wia_file_size); }
  u64 GetDataSize() const override { return Common::swap64(m_header_1.iso_file_size); }
  bool IsDataSizeAccurate() const override { return true; }
  bool Read(u64 offset, u64 size, u8* out_ptr) override;

private:
  explicit WIAFileReader(File::IOFile file, const std::string& path);
  bool Initialize(const std::string& path);

  static std::string VersionToString(u32 version);

  using SHA1 = std::array<u8, 20>;
  using WiiKey = std::array<u8, 16>;

#pragma pack(push, 1)
  struct WIAHeader1
  {
    u32 magic;
    u32 version;
    u32 version_compatible;
    u32 header2_size;
    SHA1 header2_hash;
    u64 iso_file_size;
    u64 wia_file_size;
    SHA1 header1_hash;
  };
  static_assert(sizeof(WIAHeader1) == 0x48, "Wrong size for WIA header 1");

  struct WIAHeader2
  {
    u32 disc_type;
    u32 compression_type;
    u32 compression_level;  // Informative only
    u32 chunk_size;

    std::array<u8, 0x80> disc_header;

    u32 number_of_partition_entries;
    u32 partition_entry_size;
    u64 partition_entries_offset;
    SHA1 partition_entries_hash;

    u32 number_of_raw_data_entries;
    u64 raw_data_entries_offset;
    u32 raw_data_entries_size;

    u32 number_of_group_entries;
    u64 group_entries_offset;
    u32 group_entries_size;
  };
  static_assert(sizeof(WIAHeader2) == 0xd4, "Wrong size for WIA header 2");

  struct PartitionDataEntry
  {
    u32 first_sector;
    u32 number_of_sectors;
    u32 group_index;
    u32 number_of_groups;
  };
  static_assert(sizeof(PartitionDataEntry) == 0x10, "Wrong size for WIA partition data entry");

  struct PartitionEntry
  {
    WiiKey partition_key;
    std::array<PartitionDataEntry, 2> data_entries;
  };
  static_assert(sizeof(PartitionEntry) == 0x30, "Wrong size for WIA partition entry");

  struct RawDataEntry
  {
    u64 data_offset;
    u64 data_size;
    u32 group_index;
    u32 number_of_groups;
  };
  static_assert(sizeof(RawDataEntry) == 0x18, "Wrong size for WIA raw data entry");

  struct GroupEntry
  {
    u32 data_offset;  // >> 2
    u32 data_size;
  };
  static_assert(sizeof(GroupEntry) == 0x08, "Wrong size for WIA group entry");

  struct HashExceptionEntry
  {
    u16 offset;
    SHA1 hash;
  };
  static_assert(sizeof(HashExceptionEntry) == 0x16, "Wrong size for WIA hash exception entry");
#pragma pack(pop)

  bool m_valid;

  File::IOFile m_file;

  WIAHeader1 m_header_1;
  WIAHeader2 m_header_2;
  std::vector<PartitionEntry> m_partition_entries;
  std::vector<RawDataEntry> m_raw_data_entries;
  std::vector<GroupEntry> m_group_entries;

  static constexpr u32 WIA_VERSION = 0x01000000;
  static constexpr u32 WIA_VERSION_WRITE_COMPATIBLE = 0x00090000;
  static constexpr u32 WIA_VERSION_READ_COMPATIBLE = 0x00080000;
};

}  // namespace DiscIO
