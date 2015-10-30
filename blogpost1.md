# Gluon VM - Virtual machine for BEAM

Presenting a hobby project that I've been doing for several months now.

## Summary

*  A virtual machine which runs BEAM bytecode -- Erlang, Elixir, LFE, Joxa, you name it.
*  Single core -- simple!
*  Optional features -- small size!
*  Written in standard C++ -- platform independent.
*  Clean source, modern standard (C++11 and C++14 both are in use). Avoiding complex code constructs.

## Usecases

Because the VM is small (think 10-20% of regular OTP binary), it opens new and different possibilities which were not reachable before. Main idea would be to use it on IoT platforms which are too small for regular BEAM VM to run. Also those platforms that don't run Linux (again for size reasons), Gluon VM is written in such a portable way, that replacing OS layer or even rewriting it for OS-less chip, would become a fairly short project.

Further work would have to focus on reducing BEAM file sizes and rewriting libraries to a very compact and limited form. In current state libraries take a significant amount of memory, many of them are not used in one specific system, and many of those which are used - are ran only once, at init time.

Another usecase could be embedding into another system in form of a scripting platform. Transpile into Javascript or Asm.js and run in browser maybe?
