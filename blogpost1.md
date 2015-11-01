# Gluon VM - Virtual machine for BEAM

Presenting a hobby project that I've been doing for several months now.

## Summary

*  A virtual machine which runs BEAM bytecode -- Erlang, Elixir, LFE, Joxa, you name it.
*  Single core -- simple!
*  Optional features -- small size!
*  Written in standard C++ -- platform independent.
*  Clean source, modern standard (C++11 and C++14 both are in use). Avoiding complex code constructs.

## Why it exists?

There definitely is a vacant niche in Erlang ecosystem -- a VM which has ability to run in constrained environments. Existing projects try to shrink and reach there, but they were never designed to be small in the first place. Even having cut the VM code, the next step would be to minimize memory usage at startup, rewrite parts of startup code and cut library. I've been able to find references to such projects but they still require quite powerful hardware to even start a shell in an empty VM.

This project attempts to occupy this niche and deliver small and simple VM which is memory-savvy and covers most of BEAM features enough to run complex projects (such as a web server). As a side effect and a result of programming style it is also possible to embed virtual machine into one's application.

I started with single core for simplicity sake, because hours I am able to put into it are limited, so project will benefit from being delivered as soon as possible, but will not benefit from rich features that take too many hours, rarely wanted and are never done.

## Usecases

Because the VM is small (think 10-20% of regular OTP binary), it opens new and different possibilities which were not reachable before. Main idea would be to use it on IoT platforms which are too small for regular BEAM VM to run. Also those platforms that don't run Linux (again for size reasons), Gluon VM is written in such a portable way, that replacing OS layer or even rewriting it for OS-less chip, would become a fairly short project.

Another usecase could be embedding into another system in form of a scripting platform. Transpile into Javascript or Asm.js and run in browser maybe?

## What is done?

Status on 15th October: the project was able to load multiple BEAM files at once (automatic import and load) and run most of code in `lists` module, some simple demos, was able to run and return result from famous ring example (spawn multiple processes and chain send/receive a number).

## When is it ready?

The work is in progress. Several months ahead, to say least. This is a hobby project, so basically it is done when it is done (or I start working full time on it and then it's ready much faster).

## Ideas and improvements

A standard C library has to be replaced to an embedded-oriented micro-library. Such as `dietlibc` or similar. It is possible that it may have to be replaced more than once for certain platforms where `dietlibc` won't run.

Further work would have to focus on reducing BEAM file sizes and rewriting libraries to a very compact and limited form. In current state libraries take a significant amount of memory, many of them are not used in one specific system, and many of those which are used - are ran only once, at init time.
