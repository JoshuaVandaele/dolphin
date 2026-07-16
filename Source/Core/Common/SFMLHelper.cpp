// Copyright 2018 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#include "Common/SFMLHelper.h"

#include <SFML/Network/Dns.hpp>
#include <SFML/Network/IpAddress.hpp>
#include <SFML/Network/Packet.hpp>

#include <optional>

sf::Packet& operator>>(sf::Packet& packet, Common::BigEndianValue<u16>& data)
{
  u16 tmp;
  packet >> tmp;
  data = tmp;
  return packet;
}

sf::Packet& operator>>(sf::Packet& packet, Common::BigEndianValue<u32>& data)
{
  u32 tmp;
  packet >> tmp;
  data = tmp;
  return packet;
}

sf::Packet& operator>>(sf::Packet& packet, Common::BigEndianValue<u64>& data)
{
  u64 tmp;
  packet >> tmp;
  data = tmp;
  return packet;
}

namespace Common
{
// SFML's Uint64 type is different depending on platform,
// so we have this for cleaner code.
u64 PacketReadU64(sf::Packet& packet)
{
  u64 value;
  packet >> value;
  return value;
}

std::optional<sf::IpAddress> ResolveIPv4(const std::string& hostname,
                                         const std::vector<sf::IpAddress>& servers,
                                         std::optional<sf::Time> timeout)
{
  if (const auto addresses = sf::Dns::resolve(hostname, servers, timeout))
  {
    for (const sf::IpAddress& address : *addresses)
    {
      if (address.isV4())
      {
        return address;
      }
    }
  }
  return std::nullopt;
}
}  // namespace Common
