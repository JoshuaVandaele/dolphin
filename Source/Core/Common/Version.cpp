// Copyright 2008 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#include "Common/Version.h"

#include <string>

#include "Common/scmrev.h"

namespace Common
{
#define EMULATOR_NAME "Dolphin"

#ifdef _DEBUG
#define BUILD_TYPE_STR "Debug "
#elif defined DEBUGFAST
#define BUILD_TYPE_STR "DebugFast "
#else
#define BUILD_TYPE_STR ""
#endif

const std::string& GetScmRevStr()
{
  static const std::string scm_rev_str = EMULATOR_NAME " "
  // Note this macro can be empty if the master branch does not exist.
#if 1 - SCM_COMMITS_AHEAD_MASTER - 1 != 0
                                                       "[" SCM_BRANCH_STR "] "
#endif

#ifdef __INTEL_COMPILER
      BUILD_TYPE_STR SCM_DESC_STR "-ICC";
#else
      BUILD_TYPE_STR SCM_DESC_STR;
#endif
  return scm_rev_str;
}

const std::string& GetScmRevGitStr()
{
  static const std::string SCM_REV_GIT_STR = SCM_REV_STR;
  return SCM_REV_GIT_STR;
}

const std::string& GetScmDescStr()
{
  static const std::string scm_desc_str = SCM_DESC_STR;
  return scm_desc_str;
}

const std::string& GetScmBranchStr()
{
  static const std::string scm_branch_str = SCM_BRANCH_STR;
  return scm_branch_str;
}

const std::string& GetUserAgentStr()
{
  static const std::string USER_AGENT_STR = EMULATOR_NAME "/" SCM_DESC_STR;
  return USER_AGENT_STR;
}

const std::string& GetScmDistributorStr()
{
  static const std::string scm_distributor_str = SCM_DISTRIBUTOR_STR;
  return scm_distributor_str;
}

const std::string& GetScmUpdateTrackStr()
{
  static const std::string scm_update_track_str = SCM_UPDATE_TRACK_STR;
  return scm_update_track_str;
}

const std::string& GetNetplayDolphinVer()
{
#ifdef _WIN32
  static const std::string netplay_dolphin_ver = SCM_DESC_STR " Win";
#elif __APPLE__
  static const std::string netplay_dolphin_ver = SCM_DESC_STR " Mac";
#else
  static const std::string NETPLAY_DOLPHIN_VER = SCM_DESC_STR " Lin";
#endif
  return NETPLAY_DOLPHIN_VER;
}

int GetScmCommitsAheadMaster()
{
  // Note this macro can be empty if the master branch does not exist.
  return SCM_COMMITS_AHEAD_MASTER + 0;
}

}  // namespace Common
