# Gluon VM - Virtual machine for BEAM

Presenting a hobby project that I've been doing for several months now.

## Summary

*  A virtual machine which runs BEAM bytecode -- Erlang, Elixir, LFE, Joxa, you name it.
*  Single core -- simple!
*  Optional features -- small size!
*  Written in standard C++ -- platform independent.
*  Clean source, modern standard (C++14). Keeping code simple and typesafe.

## Why it exists?

There definitely is a vacant niche in Erlang ecosystem -- we miss a VM which has ability to run in constrained environments. Existing projects try to shrink and reach there, but they were never designed to be small in the first place. Even having cut the VM code, the next step would be to minimize memory usage at startup, rewrite parts of startup code and cut library. I've been able to find references to such projects but they still require quite powerful hardware to even start a shell in an empty VM.

This project attempts to occupy this niche and deliver small and simple VM which is memory-savvy and covers most of BEAM features enough to run complex projects (such as a web server). As a side effect and a result of programming style it is also possible to embed virtual machine into one's application.

I started with single core for simplicity sake, because hours I am able to put into it are limited, so project will benefit from being delivered as soon as possible, but will not benefit from rich features that take too many hours, rarely wanted and are never done.

## Usecases

Because the VM is small (think 10-20% of regular OTP binary), it opens new possibilities which were not reachable before. Main idea would be to use it on IoT platforms which are too constrained for regular BEAM VM to run. Also those platforms that are not able to run Linux should become within reach. Replacing OS layer or even rewriting it for OS-less chip is a fairly short project, only several hooks to manage VM process, reach files, sockets or memory are required.

A first reachable goal would be to run on a ≤1 Megabyte system. I mean, would you imagine Erlang running on a 1Mb DOS system? I could, it is possible, and we're getting there eventually.

Another usecase could be embedding into another application in form of a scripting platform. A game scripting engine in Erlang? Okay. Or transpile into Asm.js and run in browser? Oh god, no... *shivers*

## What is done?

Status on 1st of November: the project was able to read multiple BEAM files and run most of code in `lists` module, some simple demos, was able to run and return result from famous ring example (spawn multiple processes and chain send/receive a number).

Currently slowly making my way to be able to enter and run otp_ring0 (init entry point for OTP and to get to the shell).

## When is it ready?

The work is in progress. Several months ahead, to say least. This is a hobby project, so basically it is done when it is done (or I start working full time on it and then it's ready much faster). 

### TODO

* GC, this is coming soon. I am using "The garbage collection handbook" from 2012.
* Ports, sockets, file access for Erlang code, io in general
* Timers
* Binary opcodes
* ETS

### Optional improvements

* Bignums and floats
* More compact BEAM format

## Ideas and improvements

To go the path of further shrinking:
* Standard C library has to be replaced with an embedded-oriented library. Such as `dietlibc` or a similar. 
* Try how 32-bit version affects the binary size and memory footprint.
* Try how other than AMD64 instruction set affects size and memory footprint.
* Shrink the BEAM file size, possibly develop a new smaller format. Possibly optimize out unused functions in standard library and produce 'release-like' output directory with shrinked Erlang library.
* Possibly optimize out unused BIF functions from the VM source to produce smaller VM.
* Optimize Erlang startup, load start modules temporarily then remove them from memory.
* Tweak startup memory usage.
* JIT? Not there yet.

Further work would have to focus on reducing BEAM file sizes and rewriting libraries to a very compact and limited form. In current state libraries take a significant amount of memory, many of them are not used in one specific system, and many of those which are used - are ran only once, at init time.

## Contributions

Contributions are not accepted until the code is fully shaped and first release is coming.
