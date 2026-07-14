// Copyright 2009 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#pragma once

#include <format>
#include <string>
#include <string_view>

#include "Common/FormatUtil.h"
#include "Common/Logging/Log.h"

namespace Common
{
// Message alerts
enum class MsgType
{
  Information,
  Question,
  Warning,
  Critical
};

using MsgAlertHandler = bool (*)(const char* caption, const char* text, bool yes_no, MsgType style);
using StringTranslator = std::string (*)(const char* text);

void RegisterMsgAlertHandler(MsgAlertHandler handler);
void RegisterStringTranslator(StringTranslator translator);

[[nodiscard]] std::string GetStringT(const char* string);

bool MsgAlertFmtImpl(bool yes_no, MsgType style, Common::Log::LogType log_type, const char* file,
                     int line, std::string_view format, std::format_args args);

template <typename... Args>
bool MsgAlertFmt(bool yes_no, MsgType style, Common::Log::LogType log_type, const char* file,
                 int line, std::format_string<Args...> format, Args&&... args)
{
  return MsgAlertFmtImpl(yes_no, style, log_type, file, line, format.get(),
                         std::make_format_args(args...));
}

template <bool has_non_positional_args, typename... Args>
bool MsgAlertFmtT(bool yes_no, MsgType style, Common::Log::LogType log_type, const char* file,
                  int line, [[maybe_unused]] std::format_string<Args...> format,
                  std::string_view translated_format, Args&&... args)
{
  static_assert(!has_non_positional_args,
                "Translatable strings must use positional arguments (e.g. {0} instead of {})");
  return MsgAlertFmtImpl(yes_no, style, log_type, file, line, translated_format,
                         std::make_format_args(args...));
}

void SetEnableAlert(bool enable);
void SetAbortOnPanicAlert(bool should_abort);

template <typename... Args>
std::string FmtFormatT(const char* string, Args&&... args)
{
  return std::vformat(Common::GetStringT(string), std::make_format_args(args...));
}
}  // namespace Common

// std::format-capable variants of the macros

#define GenericAlertFmt(yes_no, style, log_type, format, ...)                                      \
  Common::MsgAlertFmt(yes_no, style, Common::Log::LogType::log_type, __FILE__, __LINE__,           \
                      format __VA_OPT__(, ) __VA_ARGS__)

#define GenericAlertFmtT(yes_no, style, log_type, format, ...)                                     \
  Common::MsgAlertFmtT<Common::ContainsNonPositionalArguments(format)>(                            \
      yes_no, style, Common::Log::LogType::log_type, __FILE__, __LINE__, format,                   \
      Common::GetStringT(format) __VA_OPT__(, ) __VA_ARGS__)

#define SuccessAlertFmt(format, ...)                                                               \
  GenericAlertFmt(false, Common::MsgType::Information, MASTER_LOG,                                 \
                  format __VA_OPT__(, ) __VA_ARGS__)

#define PanicAlertFmt(format, ...)                                                                 \
  GenericAlertFmt(false, Common::MsgType::Warning, MASTER_LOG, format __VA_OPT__(, ) __VA_ARGS__)

#define PanicYesNoFmt(format, ...)                                                                 \
  GenericAlertFmt(true, Common::MsgType::Warning, MASTER_LOG, format __VA_OPT__(, ) __VA_ARGS__)

#define AskYesNoFmt(format, ...)                                                                   \
  GenericAlertFmt(true, Common::MsgType::Question, MASTER_LOG, format __VA_OPT__(, ) __VA_ARGS__)

#define CriticalAlertFmt(format, ...)                                                              \
  GenericAlertFmt(false, Common::MsgType::Critical, MASTER_LOG, format __VA_OPT__(, ) __VA_ARGS__)

// Use these macros (that do the same thing) if the message should be translated.
#define SuccessAlertFmtT(format, ...)                                                              \
  GenericAlertFmtT(false, Common::MsgType::Information, MASTER_LOG,                                \
                   format __VA_OPT__(, ) __VA_ARGS__)

#define PanicAlertFmtT(format, ...)                                                                \
  GenericAlertFmtT(false, Common::MsgType::Warning, MASTER_LOG, format __VA_OPT__(, ) __VA_ARGS__)

#define PanicYesNoFmtT(format, ...)                                                                \
  GenericAlertFmtT(true, Common::MsgType::Warning, MASTER_LOG, format __VA_OPT__(, ) __VA_ARGS__)

#define AskYesNoFmtT(format, ...)                                                                  \
  GenericAlertFmtT(true, Common::MsgType::Question, MASTER_LOG, format __VA_OPT__(, ) __VA_ARGS__)

#define CriticalAlertFmtT(format, ...)                                                             \
  GenericAlertFmtT(false, Common::MsgType::Critical, MASTER_LOG, format __VA_OPT__(, ) __VA_ARGS__)

// Variant that takes a log type, used by the assert macros
#define PanicYesNoFmtAssert(log_type, format, ...)                                                 \
  GenericAlertFmt(true, Common::MsgType::Warning, log_type, format __VA_OPT__(, ) __VA_ARGS__)
