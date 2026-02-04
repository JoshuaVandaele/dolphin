// Copyright 2026 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#include "DolphinTool/BisectCommand.h"

#include <cstdlib>
#include <optional>
#include <string>
#include <vector>

#include <OptionParser.h>
#include <fmt/ostream.h>
#include <picojson.h>
#ifdef _WIN32
#include <archive.h>
#include <archive_entry.h>
#endif

#include "Common/CommonPaths.h"
#include "Common/FileUtil.h"
#include "Common/HttpRequest.h"
#include "Common/ScopeGuard.h"
#include "Common/StringUtil.h"
#include "Common/scmrev.h"
#include "UICommon/AutoUpdate.h"

#ifdef _WIN32
#include <windows.h>
#endif

namespace DolphinTool
{
#ifdef _WIN32
static bool ExtractDolphin(const std::string filename, const std::string destination)
{
  auto* a = archive_read_new();
  if (!a)
    return false;
  Common::ScopeGuard read_guard{[&] {
    archive_read_close(a);
    archive_read_free(a);
  }};

  auto* ext = archive_write_disk_new();
  if (!ext)
    return false;
  Common::ScopeGuard write_guard{[&] {
    archive_write_close(ext);
    archive_write_free(ext);
  }};

  archive_write_disk_set_options(ext, ARCHIVE_EXTRACT_TIME | ARCHIVE_EXTRACT_PERM |
                                          ARCHIVE_EXTRACT_ACL | ARCHIVE_EXTRACT_FFLAGS);

  archive_read_support_format_7zip(a);

  archive_write_disk_set_standard_lookup(ext);

  if (archive_read_open_filename(a, filename.c_str(), 10240) != ARCHIVE_OK)
  {
    fmt::print(std::cerr, "Failed to open {}.", filename);
    return false;
  }

  struct archive_entry* entry;
  while (archive_read_next_header(a, &entry) == ARCHIVE_OK)
  {
    const std::string orig_path = archive_entry_pathname(entry);
    // Since the releases have a top directory named "Dolphin-ARCHITECTURE",
    // we remove it to extract only its contents.
    const size_t first_slash_pos = orig_path.find('/');
    const std::string truncated_path = orig_path.substr(first_slash_pos + 1);
    const std::string full_path = destination + DIR_SEP + truncated_path;
    archive_entry_set_pathname(entry, full_path.c_str());

    if (archive_write_header(ext, entry) != ARCHIVE_OK)
    {
      fmt::print(std::cerr, "Error extracting {}: {}", orig_path, archive_error_string(ext));
      return false;
    }

    const void* buff;
    size_t size;
    la_int64_t offset;

    while (archive_read_data_block(a, &buff, &size, &offset) == ARCHIVE_OK)
    {
      if (archive_write_data_block(ext, buff, size, offset) != ARCHIVE_OK)
      {
        fmt::print(std::cerr, "Error extracting {}: {}", orig_path, archive_error_string(ext));
        return false;
      }
    }
  }

  return true;
}
#endif

// Returns a new temporary directory containing the Dolphin installation.
static std::optional<std::string> DownloadDolphin(const std::string version)
{
#ifdef _WIN32
  fmt::print(std::cerr, "Downloading Dolpin {}\n", version);

  const std::string url =
      fmt::format("{}/download/direct/{}/{}", AutoUpdateChecker::GetUpdateServerUrl(), version,
                  AutoUpdateChecker::GetPlatformID());

  Common::HttpRequest request;
  const auto response = request.Get(url);
  if (!response)
  {
    fmt::print(std::cerr, "{} HTTP GET Error: {}\n", url, request.GetLastResponseCode());
    return std::nullopt;
  }

  const std::string temp_dir = File::CreateTempDir();
  if (temp_dir.empty())
  {
    fmt::print(std::cerr, "Could not create temporary directory. Aborting.");
    return std::nullopt;
  }

  const std::string dolphin_file = temp_dir + DIR_SEP + version;
  File::WriteStringToFile(
      dolphin_file,
      std::string_view{reinterpret_cast<const char*>(response->data()), response->size()});

  if (ExtractDolphin(dolphin_file, temp_dir))
  {
    File::Delete(dolphin_file);
    return temp_dir;
  }
  File::DeleteDirRecursively(temp_dir);
#endif
  return std::nullopt;
}

static void RunDolphin(const std::string dolphin_install)
{
#ifdef _WIN32
  if (!File::CreateEmptyFile(dolphin_install + DIR_SEP + "portable.txt"))
  {
    fmt::print(cerr, "Error: Could not create portable.txt");
    return;
  }
  const std::string command_line = dolphin_install + DIR_SEP + "Dolphin.exe";

  STARTUPINFO sinfo{.cb = sizeof(sinfo)};
  PROCESS_INFORMATION pinfo;
  if (CreateProcessW(UTF8ToWString(UpdaterPath()).c_str(), UTF8ToWString(command_line).data(),
                     nullptr, nullptr, FALSE, 0, nullptr, nullptr, &sinfo, &pinfo))
  {
    CloseHandle(pinfo.hThread);
    CloseHandle(pinfo.hProcess);
  }
  else
  {
    const std::string error = Common::GetLastErrorString();
    fmt::print(cerr, "Error: Could not start Dolphin: {}", error);
    fmt::print(cerr, "       Please run it from {}", dolphin_install);
  }
#endif
}

static std::optional<std::vector<std::string>> GetBuildsBetween(const std::string version_a,
                                                                const std::string version_b)
{
  const std::string url =
      fmt::format("{}/download/buildlist", AutoUpdateChecker::GetUpdateServerUrl());

  auto request = Common::HttpRequest(std::chrono::milliseconds{0}, nullptr);
  const auto response = request.Get(url);
  if (!response)
  {
    fmt::print(std::cerr, "{} HTTP GET Error: {}\n", url, request.GetLastResponseCode());
    return std::nullopt;
  }

  picojson::value json;
  std::string err = picojson::parse(json, std::string(response->begin(), response->end()));
  if (!err.empty())
  {
    fmt::print(std::cerr, "JSON Parsing Error: {}\n", err);
    return std::nullopt;
  }

  std::vector<std::string> versions_between;
  bool a_found = false;
  bool b_found = false;
  for (const picojson::value& v : json.get<picojson::array>())
  {
    const std::string version = v.get<std::string>();
    if (!a_found)
      a_found = (version == version_a);
    if (!b_found)
      b_found = (version == version_b);
    if (!a_found && !b_found)
      continue;
    versions_between.push_back(version);
    if (a_found && b_found)
      break;
  }

  if (!a_found)
  {
    fmt::print(std::cerr, "JSON Parsing Error: Version {} was not found.\n", version_a);
    return std::nullopt;
  }

  if (!b_found)
  {
    fmt::print(std::cerr, "JSON Parsing Error: Version {} was not found.\n", version_b);
    return std::nullopt;
  }

  return versions_between;
}

int BisectCommand(const std::vector<std::string>& args)
{
  optparse::OptionParser parser;

  parser.usage("usage: bisect [options]...");

  parser.add_option("-g", "--good")
      .type("string")
      .action("store")
      .help("Working version of dolphin. Will be set to the current version if not specified.");

  parser.add_option("-b", "--bad")
      .type("string")
      .action("store")
      .help("Broken version of Dolphin. Required.");

  parser.add_option("-m", "--manual")
      .action("store_true")
      .help("Optional. Download Dolphin versions yourself.");

  const optparse::Values& options = parser.parse_args(args);

  const std::string& good_version = options["good"].empty() ? SCM_DESC_STR : options["good"];

  const std::string& bad_version = options["bad"];
  if (bad_version.empty())
  {
    fmt::print(std::cerr, "Error: No bad version set\n");
    return EXIT_FAILURE;
  }

  const bool manual = options.is_set_by_user("manual");
#ifndef _WIN32
  if (!manual)
  {
    fmt::print(
        std::cerr,
        "Error: Automatic installation is only supported on Windows. Please use --manual.\n");
    return EXIT_FAILURE;
  }
#endif

  auto builds_between = GetBuildsBetween(bad_version, good_version);
  if (!builds_between)
    return EXIT_FAILURE;

  fmt::print(std::cerr, "{} versions between {} and {}.\n", builds_between->size(), good_version,
             bad_version);

  size_t range_size = 0;
  size_t low = 0;
  size_t high = builds_between->size() - 1;
  std::optional<std::string> dolphin_install;

  Common::ScopeGuard install_guard{[&] {
    if (dolphin_install)
      File::DeleteDirRecursively(*dolphin_install);
  }};

  fmt::print(std::cout, "\nBisect started. Commands:\n");
  fmt::print(std::cout, "  g or good   -> mark the current version as GOOD\n");
  fmt::print(std::cout, "  b or bad    -> mark the current version as BAD\n");
  fmt::print(std::cout, "  r or retry  -> retry the current version\n");
  fmt::print(std::cout, "  q or quit   -> abort bisect\n\n");

  while (high - low > 1)
  {
    const size_t mid = low + (high - low) / 2;
    const std::string& candidate = builds_between->at(mid);
    const size_t new_range_size = high - low + 1;

    if (new_range_size != range_size)
    {
      range_size = new_range_size;
      const size_t steps_left = static_cast<size_t>(std::ceil(std::log2(range_size)));

      fmt::print(std::cout, "Current candidate: {}. ~{} steps left.\n", candidate, steps_left);
      dolphin_install = DownloadDolphin(candidate);
      if (!dolphin_install || manual)
        fmt::print(std::cout,
                   "Please manually download and run this version from {}/download/dev/{}\n",
                   AutoUpdateChecker::GetUpdateServerUrl(), candidate);
    }
    if (dolphin_install)
      RunDolphin(*dolphin_install);
    fmt::print(std::cout, "Mark this version as (g/b/r/q): ");

    std::string input;
    if (!std::getline(std::cin, input))
    {
      fmt::print(std::cerr, "\nAborting bisect.\n");
      return EXIT_FAILURE;
    }

    input = StripWhitespace(input);
    Common::ToLower(&input);

    if (input == "g" || input == "good")
    {
      low = mid;
      if (dolphin_install)
        File::DeleteDirRecursively(*dolphin_install);
    }
    else if (input == "b" || input == "bad")
    {
      high = mid;
      if (dolphin_install)
        File::DeleteDirRecursively(*dolphin_install);
    }
    else if (input == "q" || input == "quit")
    {
      fmt::print(std::cerr, "\nAborting bisect.\n");
      return EXIT_FAILURE;
    }
    else if (input == "r" || input == "retry")
    {
    }
    else
    {
      fmt::print(std::cerr, "Unknown command '{}'. Please enter one of: g, b, q\n\n", input);
    }
  }

  fmt::print(std::cout, "\nLast known GOOD version: {}\n", builds_between->at(low));
  fmt::print(std::cout, "First known BAD version:  {}\n", builds_between->at(high));

  return EXIT_SUCCESS;
}
}  // namespace DolphinTool
