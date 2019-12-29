// Copyright 2018 Dolphin Emulator Project
// Licensed under GPLv2+
// Refer to the license.txt file included.

#include "DiscIO/WIABlob.h"

#include <algorithm>
#include <memory>

#include "Common/Align.h"
#include "Common/CommonTypes.h"
#include "Common/File.h"
#include "Common/Logging/Log.h"
#include "Common/StringUtil.h"
#include "Common/Swap.h"

namespace DiscIO
{
WIAFileReader::WIAFileReader(File::IOFile file, const std::string& path) : m_file(std::move(file))
{
  m_valid = Initialize(path);
}

WIAFileReader::~WIAFileReader() = default;

bool WIAFileReader::Initialize(const std::string& path)
{
  if (!m_file.Seek(0, SEEK_SET) || !m_file.ReadArray(&m_header_1, 1))
    return false;

  if (m_header_1.magic != WIA_MAGIC)
    return false;

  const u32 version = Common::swap32(m_header_1.version);
  const u32 version_compatible = Common::swap32(m_header_1.version_compatible);
  if (WIA_VERSION < version_compatible || WIA_VERSION_READ_COMPATIBLE > version)
  {
    ERROR_LOG(DISCIO, "Unsupported WIA version %s in %s", VersionToString(version).c_str(),
              path.c_str());
    return false;
  }

  if (Common::swap64(m_header_1.wia_file_size) != m_file.GetSize())
  {
    ERROR_LOG(DISCIO, "File size is incorrect for %s", path.c_str());
    return false;
  }

  if (Common::swap32(m_header_1.header2_size) < sizeof(WIAHeader2))
    return false;

  if (!m_file.ReadArray(&m_header_2, 1))
    return false;

  const u32 compression_type = Common::swap32(m_header_2.compression_type);
  if (compression_type != 0)
  {
    ERROR_LOG(DISCIO, "Unsupported WIA compression type %u in %s", compression_type, path.c_str());
    return false;
  }

  const u32 number_of_partition_entries = Common::swap32(m_header_2.number_of_partition_entries);
  m_partition_entries.resize(number_of_partition_entries);
  if (!m_file.Seek(Common::swap64(m_header_2.partition_entries_offset), SEEK_SET))
    return false;
  if (!m_file.ReadArray(m_partition_entries.data(), number_of_partition_entries))
    return false;
  // TODO: Check hash

  for (const PartitionEntry& partition : m_partition_entries)
  {
    const u32 first_end = Common::swap32(partition.data_entries[0].first_sector) +
                          Common::swap32(partition.data_entries[0].number_of_sectors);
    const u32 second_start = Common::swap32(partition.data_entries[1].first_sector);
    if (first_end > second_start)
      return false;
  }

  std::sort(m_partition_entries.begin(), m_partition_entries.end(),
            [](const PartitionEntry& a, const PartitionEntry& b) {
              return Common::swap32(a.data_entries[0].first_sector) <
                     Common::swap32(b.data_entries[0].first_sector);
            });

  // TODO: Compression
  const u32 number_of_raw_data_entries = Common::swap32(m_header_2.number_of_raw_data_entries);
  m_raw_data_entries.resize(number_of_raw_data_entries);
  if (!m_file.Seek(Common::swap64(m_header_2.raw_data_entries_offset), SEEK_SET))
    return false;
  if (!m_file.ReadArray(m_raw_data_entries.data(), number_of_raw_data_entries))
    return false;

  std::sort(m_raw_data_entries.begin(), m_raw_data_entries.end(),
            [](const RawDataEntry& a, const RawDataEntry& b) {
              return Common::swap64(a.data_offset) < Common::swap64(b.data_offset);
            });

  // TODO: Compression
  const u32 number_of_group_entries = Common::swap32(m_header_2.number_of_group_entries);
  m_group_entries.resize(number_of_group_entries);
  if (!m_file.Seek(Common::swap64(m_header_2.group_entries_offset), SEEK_SET))
    return false;
  if (!m_file.ReadArray(m_group_entries.data(), number_of_group_entries))
    return false;

  return true;
}

std::unique_ptr<WIAFileReader> WIAFileReader::Create(File::IOFile file, const std::string& path)
{
  std::unique_ptr<WIAFileReader> blob(new WIAFileReader(std::move(file), path));
  return blob->m_valid ? std::move(blob) : nullptr;
}

bool WIAFileReader::Read(u64 offset, u64 size, u8* out_ptr)
{
  if (offset + size >= Common::swap64(m_header_1.iso_file_size))
    return false;

  if (offset < sizeof(WIAHeader2::disc_header))
  {
    const u64 bytes_to_read = std::min(sizeof(WIAHeader2::disc_header) - offset, size);
    std::memcpy(out_ptr, m_header_2.disc_header.data() + offset, bytes_to_read);
    offset += bytes_to_read;
    size -= bytes_to_read;
    out_ptr += bytes_to_read;
  }

  const u32 chunk_size = Common::swap32(m_header_2.chunk_size);
  for (RawDataEntry raw_data : m_raw_data_entries)
  {
    if (size == 0)
      return true;

    if (!ReadFromGroups(&offset, &size, &out_ptr, chunk_size, Common::swap64(raw_data.data_offset),
                        Common::swap64(raw_data.data_size), Common::swap32(raw_data.group_index),
                        Common::swap32(raw_data.number_of_groups), false))
    {
      return false;
    }
  }

  std::fill_n(out_ptr, static_cast<size_t>(size), 0);
  return true;
}

bool WIAFileReader::ReadWiiDecrypted(u64 offset, u64 size, u8* out_ptr, u64 partition_data_offset)
{
  const u32 chunk_size = Common::swap32(m_header_2.chunk_size) * BLOCK_DATA_SIZE / BLOCK_TOTAL_SIZE;
  for (const PartitionEntry& partition : m_partition_entries)
  {
    const u32 partition_first_sector = Common::swap32(partition.data_entries[0].first_sector);
    if (partition_data_offset != partition_first_sector * BLOCK_TOTAL_SIZE)
      continue;

    for (const PartitionDataEntry& data : partition.data_entries)
    {
      if (size == 0)
        return true;

      const u64 data_offset =
          (Common::swap32(data.first_sector) - partition_first_sector) * BLOCK_DATA_SIZE;
      const u64 data_size = Common::swap32(data.number_of_sectors) * BLOCK_DATA_SIZE;

      if (!ReadFromGroups(&offset, &size, &out_ptr, chunk_size, data_offset, data_size,
                          Common::swap32(data.group_index), Common::swap32(data.number_of_groups),
                          true))
      {
        return false;
      }
    }

    std::fill_n(out_ptr, static_cast<size_t>(size), 0);
    return true;
  }

  return false;
}

bool WIAFileReader::ReadFromGroups(u64* offset, u64* size, u8** out_ptr, u32 chunk_size,
                                   u64 data_offset, u64 data_size, u32 group_index,
                                   u32 number_of_groups, bool exception_list)
{
  PadToAddress(data_offset, offset, size, out_ptr);

  const u64 skipped_data = data_offset % chunk_size;
  data_offset -= skipped_data;
  data_size += skipped_data;

  const u64 start_group_index = (*offset - data_offset) / chunk_size;
  for (u64 i = start_group_index; i < number_of_groups && (*size) > 0; ++i)
  {
    const u64 total_group_index = group_index + i;
    if (total_group_index >= m_group_entries.size())
      return false;

    const GroupEntry group = m_group_entries[total_group_index];
    const u64 group_offset_in_partition = data_offset + i * chunk_size;
    const u64 offset_in_group = *offset - group_offset_in_partition;

    // TODO: Compression

    u64 group_offset_in_file = static_cast<u64>(Common::swap32(group.data_offset)) << 2;

    if (exception_list)
    {
      u16 exceptions;
      if (!m_file.Seek(group_offset_in_file, SEEK_SET) || !m_file.ReadArray(&exceptions, 1))
        return false;

      group_offset_in_file += Common::AlignUp(
          sizeof(exceptions) + Common::swap16(exceptions) * sizeof(HashExceptionEntry), 4);
    }

    const u64 offset_in_file = group_offset_in_file + offset_in_group;
    const u64 bytes_to_read = std::min(chunk_size - offset_in_group, *size);
    if (!m_file.Seek(offset_in_file, SEEK_SET) || !m_file.ReadBytes(*out_ptr, bytes_to_read))
      return false;

    *offset += bytes_to_read;
    *size -= bytes_to_read;
    *out_ptr += bytes_to_read;
  }

  return true;
}

std::string WIAFileReader::VersionToString(u32 version)
{
  const u8 a = version >> 24;
  const u8 b = (version >> 16) & 0xff;
  const u8 c = (version >> 8) & 0xff;
  const u8 d = version & 0xff;

  if (d == 0 || d == 0xff)
    return StringFromFormat("%u.%02x.%02x", a, b, c);
  else
    return StringFromFormat("%u.%02x.%02x.beta%u", a, b, c, d);
}
}  // namespace DiscIO
