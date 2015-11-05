#pragma once

#include "defs.h"
#include <map>

namespace gluon {

namespace containers {

template <typename Key, typename Value>
class DictRangeMapping {
 public:
  using Storage = std::map<Key, Value>;
  using Iterator = typename Storage::iterator;

  Iterator pos;
  Iterator end;

  DictRangeMapping(const Iterator& b, const Iterator& e) : pos(b), end(e) {}

  bool have() const { return pos != end; }
  Iterator current() { return pos; }
  void advance() { pos++; }
};

// TODO: std::enable_if magic with const/nonconst members?
template <typename Key, typename Value>
class ConstDictRangeMapping {
 public:
  using Storage = std::map<Key, Value>;
  using Iterator = typename Storage::const_iterator;

  Iterator pos;
  Iterator end;
  ConstDictRangeMapping(const Iterator& b, const Iterator& e)
      : pos(b), end(e) {}

  bool have() const { return pos != end; }
  Iterator current() { return pos; }
  void advance() { pos++; }
};

// A map wrapper with mapping helper classes (to iterate)
// TODO: Replace this with self-made structure
template <typename Key, typename Value>
class STLDict {
 private:
  using Self = STLDict<Key, Value>;
  using Storage = std::map<Key, Value>;
  Storage map_;

 public:
  using Iterator = typename Storage::iterator;

  STLDict() = default;
  STLDict(const STLDict&) = delete;
  STLDict(STLDict&&) = default;
  STLDict& operator=(STLDict&&) = default;

  // Removed operator[] to separate insert and find semantic
  // const Value& operator[](const Key& k) const { return map_[k]; }
  // Value& operator[](const Key& k) { return map_[k]; }
  void insert(const Key& k, const Value& v) { map_[k] = v; }

  bool contains(const Key& k) const {
    auto i = map_.find(k);
    return i != map_.end();
  }

  // Returns pointer to result or nullptr
  Value* find_ptr(const Key& k) {
    auto i = map_.find(k);
    if (i == map_.end()) {
      return nullptr;
    }
    return &i->second;
  }
  // Returns const pointer to result or nullptr
  const Value* find_ptr(const Key& k) const {
    auto i = map_.find(k);
    if (i == map_.end()) {
      return nullptr;
    }
    return &i->second;
  }
  /*
      // Returns reference to result or a default value you provide
      Value &find_ref(const Key &k, Value &defa) {
        auto i = map_.find(k);
        if (i == map_.end()) { return defa; }
        return i->second;
      }
      // Returns const reference to result or a default value you provide
      const Value &find_ref(const Key &k, const Value &defa) const {
        auto i = map_.find(k);
        if (i == map_.end()) { return defa; }
        return i->second;
      }
  */

  void erase(const Key& k) { map_.erase(k); }
  size_t size() const { return map_.size(); }

  using Mapping = DictRangeMapping<Key, Value>;
  using ConstMapping = ConstDictRangeMapping<Key, Value>;

  Mapping all() { return Mapping(map_.begin(), map_.end()); }
  ConstMapping all() const { return ConstMapping(map_.begin(), map_.end()); }
};

}  // ns containers

// A dictionary
// Keys should have 'operator <'
template <typename K, typename V>
using Dict = containers::STLDict<K, V>;

}  // ns gluon
