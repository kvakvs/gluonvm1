#pragma once

// Allows loading big integer constants from BEAM files and using big arithmetics
// If this is disabled, only machine word size (32 or 64bit) integers are allowed
// TODO: bignums have to be implemented throughout loader and vm
#define FEATURE_BIGNUM        0

// Allows loading float values from BEAM and enables float BIFs and arithmetics
// 32-bit floats will be used as default (see float_t in g_defs.h)
// TODO: floats have to be implemented throughout loader and vm
#define FEATURE_FLOAT         0

// Well, maps, map value support, map bifs, and map opcodes in VM loop
// TODO: maps have to be implemented throughout loader and vm
#define FEATURE_MAPS          0

// Binary constants, binary opcodes opcodes in VM loop, binary construction and
// matching features also BIFs that manipulate binaries
// TODO: binaries have to be implemented throughout loader and vm
#define FEATURE_BINARIES      0

// Distribution features such as long pids
// TODO: distribution features have to be implemented throughout loader and vm
#define FEATURE_ERL_DIST      0

// Loads line section from BEAM, enables 'line' opcodes in VM loop, resolves
// line numbers for code locations. REQUIRES: FEATURE_CODE_RANGES=1 too
#define FEATURE_LINE_NUMBERS  0

// Can be used separately without FEATURE_LINE_NUMBERS. Stores ranges of code
// addresses to quickly identify module and function (and line number if feature
// is enabled) by code location.
#define FEATURE_CODE_RANGES   1

#if FEATURE_LINE_NUMBERS && !FEATURE_CODE_RANGES
#   error "LINE_NUMBERS feature requires also CODE_RANGES"
#endif