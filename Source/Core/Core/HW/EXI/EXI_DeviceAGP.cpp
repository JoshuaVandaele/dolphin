// Copyright 2015 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#include "Core/HW/EXI/EXI_DeviceAGP.h"

#include <algorithm>
#include <memory>
#include <string>
#include <vector>

#include "Common/Assert.h"
#include "Common/ChunkFile.h"
#include "Common/CommonTypes.h"
#include "Common/IOFile.h"
#include "Common/Logging/Log.h"
#include "Common/StringUtil.h"
#include "Core/Config/MainSettings.h"
#include "Core/HW/EXI/EXI.h"

namespace ExpansionInterface
{
CEXIAgp::CEXIAgp(Core::System& system, Slot slot) : IEXIDevice(system)
{
  ASSERT(IsMemcardSlot(slot));
  m_slot = slot;

  // Create the ROM
  m_rom_size = 0;

  LoadRom();

  m_address = 0;
}

CEXIAgp::~CEXIAgp()
{
  std::string path;
  std::string filename;
  std::string ext;
  std::string gbapath;
  SplitPath(Config::Get(Config::GetInfoForAGPCartPath(m_slot)), &path, &filename, &ext);
  gbapath = path + filename;

  SaveFileFromEEPROM(gbapath + ".sav");
}

void CEXIAgp::CRC8(const u8* data, u32 size)
{
  for (u32 it = 0; it < size; it++)
  {
    u8 crc = 0;
    m_hash = m_hash ^ data[it];
    if (m_hash & 1)
      crc ^= 0x5e;
    if (m_hash & 2)
      crc ^= 0xbc;
    if (m_hash & 4)
      crc ^= 0x61;
    if (m_hash & 8)
      crc ^= 0xc2;
    if (m_hash & 0x10)
      crc ^= 0x9d;
    if (m_hash & 0x20)
      crc ^= 0x23;
    if (m_hash & 0x40)
      crc ^= 0x46;
    if (m_hash & 0x80)
      crc ^= 0x8c;
    m_hash = crc;
  }
}

void CEXIAgp::LoadRom()
{
  // Load whole ROM dump
  std::string path;
  std::string filename;
  std::string ext;
  SplitPath(Config::Get(Config::GetInfoForAGPCartPath(m_slot)), &path, &filename, &ext);
  const std::string gbapath = path + filename;
  LoadFileToROM(gbapath + ext);
  INFO_LOG_FMT(EXPANSIONINTERFACE, "Loaded GBA rom: {} card: {}", gbapath, m_slot);
  LoadFileToEEPROM(gbapath + ".sav");
  INFO_LOG_FMT(EXPANSIONINTERFACE, "Loaded GBA sav: {} card: {}", gbapath, m_slot);
}

void CEXIAgp::LoadFileToROM(const std::string& filename)
{
  File::IOFile p_stream(filename, "rb");
  if (p_stream)
  {
    u64 filesize = p_stream.GetSize();
    m_rom_size = filesize & 0xFFFFFFFF;
    m_rom_mask = (m_rom_size - 1);

    m_rom.resize(m_rom_size);

    p_stream.ReadBytes(m_rom.data(), filesize);
  }
  else
  {
    // dummy rom data
    m_rom.resize(0x2000);
  }
}

void CEXIAgp::LoadFileToEEPROM(const std::string& filename)
{
  // Technically one of EEPROM, Flash, SRAM, FRAM
  File::IOFile p_stream(filename, "rb");
  if (p_stream)
  {
    u64 filesize = p_stream.GetSize();
    m_eeprom_size = filesize & 0xFFFFFFFF;
    m_eeprom_mask = (m_eeprom_size - 1);

    m_eeprom.resize(m_eeprom_size);
    p_stream.ReadBytes(m_eeprom.data(), filesize);
    if ((m_eeprom_size == 512) || (m_eeprom_size == 8192))
    {
      // Handle endian read - could be done with byte access in 0xAE commands instead
      for (u32 index = 0; index < (m_eeprom_size / 8); index++)
      {
        u64 new_val = 0;
        for (u32 indexb = 0; indexb < 8; indexb++)
          new_val = (new_val << 0x8) | m_eeprom[index * 8 + indexb];
        ((u64*)(m_eeprom.data()))[index] = new_val;
      }
      m_eeprom_add_end = (m_eeprom_size == 512 ? (2 + 6) : (2 + 14));
      m_eeprom_add_mask = (m_eeprom_size == 512 ? 0x3F : 0x3FF);
      m_eeprom_read_mask = (m_eeprom_size == 512 ? 0x80 : 0x8000);
      m_eeprom_status_mask = (m_rom_size == 0x2000000 ? 0x1FFFF00 : 0x1000000);
    }
    else
      m_eeprom_status_mask = 0;
  }
  else
  {
    m_eeprom_size = 0;
    m_eeprom.clear();
  }
}

void CEXIAgp::SaveFileFromEEPROM(const std::string& filename)
{
  File::IOFile p_stream(filename, "wb");
  if (p_stream)
  {
    if ((m_eeprom_size == 512) || (m_eeprom_size == 8192))
    {
      // Handle endian write - could be done with byte access in 0xAE commands instead
      std::vector<u8> temp_eeprom(m_eeprom_size);
      for (u32 index = 0; index < (m_eeprom_size / 8); index++)
      {
        u64 new_val = ((u64*)(m_eeprom.data()))[index];
        for (u32 indexb = 0; indexb < 8; indexb++)
          temp_eeprom[index * 8 + (7 - indexb)] = (new_val >> (indexb * 8)) & 0xFF;
      }
      p_stream.WriteBytes(temp_eeprom.data(), m_eeprom_size);
    }
    else
    {
      p_stream.WriteBytes(m_eeprom.data(), m_eeprom_size);
    }
  }
}

u32 CEXIAgp::ImmRead(u32 _uSize)
{
  u32 u_data = 0;
  u8 rom_val1, rom_val2, rom_val3, rom_val4;

  switch (m_current_cmd)
  {
  case 0xAE000000:       // Clock handshake?
    u_data = 0x5AAA5517;  // 17 is precalculated hash
    m_current_cmd = 0;
    break;
  case 0xAE010000:  // Init?
    u_data = (m_return_pos == 0) ? 0x01020304 :
                                  0xF0020304;  // F0 is precalculated hash, 020304 is left over
    if (m_return_pos == 1)
      m_current_cmd = 0;
    else
      m_return_pos = 1;
    break;
  case 0xAE020000:  // Read 2 bytes with 24 bit address
    if (m_eeprom_write_status && ((m_rw_offset & m_eeprom_status_mask) == m_eeprom_status_mask) &&
        (m_eeprom_status_mask != 0))
    {
      rom_val1 = 0x1;
      rom_val2 = 0x0;
    }
    else
    {
      rom_val1 = m_rom[(m_rw_offset++) & m_rom_mask];
      rom_val2 = m_rom[(m_rw_offset++) & m_rom_mask];
    }
    CRC8(&rom_val2, 1);
    CRC8(&rom_val1, 1);
    u_data = (rom_val2 << 24) | (rom_val1 << 16) | (m_hash << 8);
    m_current_cmd = 0;
    break;
  case 0xAE030000:  // read the next 4 bytes out of 0x10000 group
    if (_uSize == 1)
    {
      u_data = 0xFF000000;
      m_current_cmd = 0;
    }
    else
    {
      rom_val1 = m_rom[(m_rw_offset++) & m_rom_mask];
      rom_val2 = m_rom[(m_rw_offset++) & m_rom_mask];
      rom_val3 = m_rom[(m_rw_offset++) & m_rom_mask];
      rom_val4 = m_rom[(m_rw_offset++) & m_rom_mask];
      CRC8(&rom_val2, 1);
      CRC8(&rom_val1, 1);
      CRC8(&rom_val4, 1);
      CRC8(&rom_val3, 1);
      u_data = (rom_val2 << 24) | (rom_val1 << 16) | (rom_val4 << 8) | (rom_val3);
    }
    break;
  case 0xAE040000:  // read 1 byte from 16 bit address
    // ToDo: Flash special handling
    if (m_eeprom_size == 0)
      rom_val1 = 0xFF;
    else
      rom_val1 = (m_eeprom.data())[m_eeprom_pos];
    CRC8(&rom_val1, 1);
    u_data = (rom_val1 << 24) | (m_hash << 16);
    m_current_cmd = 0;
    break;
  case 0xAE0B0000:  // read 1 bit from DMA with 6 or 14 bit address
    // Change to byte access instead of endian file access?
    rom_val1 = EE_READ_FALSE;
    if ((m_eeprom_size != 0) && (m_eeprom_pos >= EE_IGNORE_BITS) &&
        ((((u64*)m_eeprom.data())[(m_eeprom_cmd >> 1) & m_eeprom_add_mask]) >>
         ((EE_DATA_BITS - 1) - (m_eeprom_pos - EE_IGNORE_BITS))) &
            0x1)
    {
      rom_val1 = EE_READ_TRUE;
    }
    rom_val2 = 0;
    CRC8(&rom_val2, 1);
    CRC8(&rom_val1, 1);
    u_data = (rom_val2 << 24) | (rom_val1 << 16) | (m_hash << 8);
    m_eeprom_pos++;
    m_current_cmd = 0;
    break;
  case 0xAE070000:  // complete write 1 byte from 16 bit address
  case 0xAE0C0000:  // complete write 1 bit from dma with 6 or 14 bit address
    u_data = m_hash << 24;
    m_current_cmd = 0;
    break;
  default:
    u_data = 0x0;
    m_current_cmd = 0;
    break;
  }
  DEBUG_LOG_FMT(EXPANSIONINTERFACE, "AGP read {:x}", u_data);
  return u_data;
}

void CEXIAgp::ImmWrite(u32 _uData, u32 _uSize)
{
  // 0x00 = Execute current command?
  if ((_uSize == 1) && ((_uData & 0xFF000000) == 0))
    return;

  u8 hash_cmd;
  u64 mask;
  DEBUG_LOG_FMT(EXPANSIONINTERFACE, "AGP command {:x}", _uData);
  switch (m_current_cmd)
  {
  case 0xAE020000:  // set up 24 bit address for read 2 bytes
  case 0xAE030000:  // set up 24 bit address for read (0x10000 byte group)
    // 25 bit address shifted one bit right = 24 bits
    m_rw_offset = ((_uData & 0xFFFFFF00) >> (8 - 1));
    m_return_pos = 0;
    hash_cmd = (_uData & 0xFF000000) >> 24;
    CRC8(&hash_cmd, 1);
    hash_cmd = (_uData & 0x00FF0000) >> 16;
    CRC8(&hash_cmd, 1);
    hash_cmd = (_uData & 0x0000FF00) >> 8;
    CRC8(&hash_cmd, 1);
    break;
  case 0xAE040000:  // set up 16 bit address for read 1 byte
    // ToDo: Flash special handling
    m_eeprom_pos = ((_uData & 0xFFFF0000) >> 0x10) & m_eeprom_mask;
    hash_cmd = (_uData & 0xFF000000) >> 24;
    CRC8(&hash_cmd, 1);
    hash_cmd = (_uData & 0x00FF0000) >> 16;
    CRC8(&hash_cmd, 1);
    break;
  case 0xAE070000:  // write 1 byte from 16 bit address
    // ToDo: Flash special handling
    m_eeprom_pos = ((_uData & 0xFFFF0000) >> 0x10) & m_eeprom_mask;
    if (m_eeprom_size != 0)
      ((m_eeprom.data()))[(m_eeprom_pos)] = (_uData & 0x0000FF00) >> 0x8;
    hash_cmd = (_uData & 0xFF000000) >> 24;
    CRC8(&hash_cmd, 1);
    hash_cmd = (_uData & 0x00FF0000) >> 16;
    CRC8(&hash_cmd, 1);
    hash_cmd = (_uData & 0x0000FF00) >> 8;
    CRC8(&hash_cmd, 1);
    break;
  case 0xAE0C0000:  // write 1 bit from dma with 6 or 14 bit address
    if ((m_eeprom_pos < m_eeprom_add_end) ||
        (m_eeprom_pos == ((m_eeprom_cmd & m_eeprom_read_mask) ? m_eeprom_add_end :
                                                                m_eeprom_add_end + EE_DATA_BITS)))
    {
      mask = (1ULL << (m_eeprom_add_end - std::min(m_eeprom_pos, m_eeprom_add_end)));
      if ((_uData >> 16) & 0x1)
        m_eeprom_cmd |= mask;
      else
        m_eeprom_cmd &= ~mask;
      if (m_eeprom_pos == m_eeprom_add_end + EE_DATA_BITS)
      {
        // Change to byte access instead of endian file access?
        if (m_eeprom_size != 0)
          ((u64*)(m_eeprom.data()))[(m_eeprom_cmd >> 1) & m_eeprom_add_mask] = m_eeprom_data;
        m_eeprom_write_status = true;
      }
    }
    else
    {
      mask = (1ULL << (m_eeprom_add_end + EE_DATA_BITS - 1 - m_eeprom_pos));
      if ((_uData >> 16) & 0x1)
        m_eeprom_data |= mask;
      else
        m_eeprom_data &= ~mask;
    }
    m_eeprom_pos++;
    m_return_pos = 0;
    hash_cmd = (_uData & 0xFF000000) >> 24;
    CRC8(&hash_cmd, 1);
    hash_cmd = (_uData & 0x00FF0000) >> 16;
    CRC8(&hash_cmd, 1);
    break;
  case 0xAE0B0000:
    m_eeprom_write_status = false;
    break;
  case 0xAE000000:
  case 0xAE010000:
  case 0xAE090000:                  // start DMA
    m_eeprom_write_status = false;  // ToDo: Verify with hardware which commands disable EEPROM CS
    [[fallthrough]];
  case 0xAE0A0000:  // end DMA
    m_eeprom_pos = 0;
    [[fallthrough]];
  default:
    m_current_cmd = _uData;
    m_return_pos = 0;
    m_hash = 0xFF;
    hash_cmd = (_uData & 0x00FF0000) >> 16;
    CRC8(&hash_cmd, 1);
    break;
  }
}

void CEXIAgp::DoState(PointerWrap& p)
{
  p.Do(m_slot);
  p.Do(m_address);
  p.Do(m_current_cmd);
  p.Do(m_eeprom);
  p.Do(m_eeprom_cmd);
  p.Do(m_eeprom_data);
  p.Do(m_eeprom_mask);
  p.Do(m_eeprom_pos);
  p.Do(m_eeprom_size);
  p.Do(m_eeprom_add_end);
  p.Do(m_eeprom_add_mask);
  p.Do(m_eeprom_read_mask);
  p.Do(m_eeprom_status_mask);
  p.Do(m_eeprom_write_status);
  p.Do(m_hash);
  p.Do(m_position);
  p.Do(m_return_pos);
  p.Do(m_rom);
  p.Do(m_rom_mask);
  p.Do(m_rom_size);
  p.Do(m_rw_offset);
}
}  // namespace ExpansionInterface
