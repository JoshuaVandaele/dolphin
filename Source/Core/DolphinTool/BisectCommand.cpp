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

#include "Common/CommonPaths.h"
#include "Common/FileUtil.h"
#include "Common/HttpRequest.h"
#include "Common/ScopeGuard.h"
#include "Common/StringUtil.h"
#include "Common/scmrev.h"
#include "UICommon/AutoUpdate.h"

namespace DolphinTool
{

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

  const optparse::Values& options = parser.parse_args(args);

  const std::string& good_version = options["good"].empty() ? SCM_DESC_STR : options["good"];

  const std::string& bad_version = options["bad"];
  if (bad_version.empty())
  {
    fmt::print(std::cerr, "Error: No bad version set\n");
    return EXIT_FAILURE;
  }

  auto builds_between = GetBuildsBetween(bad_version, good_version);
  if (!builds_between)
    return EXIT_FAILURE;

  fmt::print(std::cerr, "{} versions between {} and {}.\n", builds_between->size(), good_version,
             bad_version);

  size_t low = 0;
  size_t high = builds_between->size() - 1;

  fmt::print(std::cout, "\nBisect started. Commands:\n");
  fmt::print(std::cout, "  g or good   -> mark the shown version as GOOD\n");
  fmt::print(std::cout, "  b or bad    -> mark the shown version as BAD\n");
  fmt::print(std::cout, "  q or quit   -> abort bisect\n\n");

  while (high - low > 1)
  {
    const size_t mid = low + (high - low) / 2;
    const std::string& candidate = builds_between->at(mid);

    const size_t range_size = high - low + 1;
    const size_t steps_left = static_cast<size_t>(std::ceil(std::log2(range_size)));

    fmt::print(std::cout, "Current candidate: {}. ~{} steps left.\n", candidate, steps_left);
    fmt::print(std::cout, "Mark this version as (g/b/q): ");

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
    }
    else if (input == "b" || input == "bad")
    {
      high = mid;
    }
    else if (input == "q" || input == "quit")
    {
      fmt::print(std::cerr, "\nAborting bisect.\n");
      return EXIT_FAILURE;
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
