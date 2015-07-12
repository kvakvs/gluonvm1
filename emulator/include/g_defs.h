#pragma once

#include <string>
#include <stdint.h>
#include <stdlib.h>
#include <map>

namespace gluon {

  using Str = std::basic_string<char>;
  using UStr = std::basic_string<uint8_t>;

  template <typename K, typename V>
  using Map = std::map<K, V>;

  // Hardware abstractions
  namespace hw {
    using Word = size_t; // size of machine word
  } // ns hw

} // ns gluon
