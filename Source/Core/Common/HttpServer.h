// Copyright 2026 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#pragma once

#include <functional>
#include <future>
#include <map>
#include <thread>

#include "Common/CommonTypes.h"
#include "Common/Network.h"

namespace Common
{
using HttpHeaders = std::map<const std::string, const std::string>;
using HttpResponse = std::pair<const HttpHeaders, const std::vector<u8>>;
using HttpRequestCallback = std::function<const HttpResponse()>;

class HttpServer final
{
public:
  explicit HttpServer(IPv4Port address, int max_clients = 10);
  ~HttpServer();

  void ServePath(const std::string& path, const HttpRequestCallback& callback);

  void Start();
  void Stop();

  IPv4Port GetAddress() const;

private:
  IPv4Port m_address;
  const int m_max_clients;
  std::map<std::string, HttpRequestCallback> m_routes;
  std::mutex m_routes_mutex;
  std::jthread m_server_thread;
  std::promise<int> m_server_ready_promise;
  std::shared_future<int> m_server_ready_future;
  std::vector<std::jthread> m_workers;
  std::mutex m_workers_mutex;

  void Serve(std::stop_token stop_token);
  void HandleClient(s32 client_sock, std::stop_token stop_token);
};
}  // namespace Common
