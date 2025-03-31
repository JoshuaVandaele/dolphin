// Copyright 2014 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#include <array>
#include <gtest/gtest.h>
#include <thread>

#include "Common/Flag.h"

using Common::Flag;

TEST(Flag, Simple)
{
  Flag f;
  EXPECT_FALSE(f.IsSet());

  f.Set();
  EXPECT_TRUE(f.IsSet());

  f.Clear();
  EXPECT_FALSE(f.IsSet());

  f.Set(false);
  EXPECT_FALSE(f.IsSet());

  EXPECT_TRUE(f.TestAndSet());
  EXPECT_TRUE(f.TestAndClear());

  Flag f2(true);
  EXPECT_TRUE(f2.IsSet());
}

TEST(Flag, MultiThreaded)
{
  Flag f;
  int count = 0;
  const int iterations_count = 100000;

  auto setter = [&]() {
    for (int i = 0; i < iterations_count; ++i)
    {
      while (f.IsSet())
        ;
      f.Set();
    }
  };

  auto clearer = [&]() {
    for (int i = 0; i < iterations_count; ++i)
    {
      while (!f.IsSet())
        ;
      count++;
      f.Clear();
    }
  };

  std::thread setter_thread(setter);
  std::thread clearer_thread(clearer);

  setter_thread.join();
  clearer_thread.join();

  EXPECT_EQ(iterations_count, count);
}

TEST(Flag, SpinLock)
{
  // Uses a flag to implement basic spinlocking using TestAndSet.
  Flag f;
  int count = 0;
  const int iterations_count = 5000;
  const int threads_count = 50;

  auto adder_func = [&]() {
    for (int i = 0; i < iterations_count; ++i)
    {
      // Acquire the spinlock.
      while (!f.TestAndSet())
        ;
      count++;
      f.Clear();
    }
  };

  std::array<std::thread, threads_count> threads;
  for (auto& th : threads)
    th = std::thread(adder_func);
  for (auto& th : threads)
    th.join();

  EXPECT_EQ(iterations_count * threads_count, count);
}
