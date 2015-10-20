#pragma once

#include "g_defs.h"

namespace gluon {

namespace containers {

  template <typename K, typename V>
  class dict_range_mapping {
  public:
    using iter_t = typename std::map<K, V>::iterator;
    iter_t pos;
    iter_t end;
    dict_range_mapping(const iter_t &b, const iter_t &e): pos(b), end(e) {}

    bool have() const { return pos != end; }

    K key() { return pos->first; }
    const K &key() const { return pos->first; }
    V &value() { return pos->second; }
    const V &value() const { return pos->second; }

    void advance() { pos++; }
  };


  // TODO: std::enable_if magic with const/nonconst members?
  template <typename K, typename V>
  class c_dict_range_mapping {
  public:
    using iter_t = typename std::map<K, V>::const_iterator;
    iter_t pos;
    iter_t end;
    c_dict_range_mapping(const iter_t &b, const iter_t &e): pos(b), end(e) {}

    bool have() const { return pos != end; }

    const K &key() const { return pos->first; }
    const V &value() const { return pos->second; }
    void advance() { pos++; }
  };


  // A map wrapper with mapping helper classes (to iterate)
  // TODO: Replace this with self-made structure
  template <typename Key, typename Value>
  class stl_map {
  private:
    using Self = stl_map<Key, Value>;
    std::map<Key, Value> map_;
  public:
    stl_map() = default;
    stl_map(stl_map &&) = default;
    stl_map &operator = (stl_map &&) = default;

    const Value &operator[] (const Key &k) const { return map_[k]; }
    Value &operator[] (const Key &k) { return map_[k]; }

    bool contains(const Key &k) const {
      auto i = map_.find(k);
      return i != map_.end();
    }

    Value *find_ptr(const Key &k) {
      auto i = map_.find(k);
      if (i == map_.end()) { return nullptr; }
      return &i->second;
    }
    const Value *find_ptr(const Key &k) const {
      auto i = map_.find(k);
      if (i == map_.end()) { return nullptr; }
      return &i->second;
    }

    Value &find_ref(const Key &k, Value &defa) {
      auto i = map_.find(k);
      if (i == map_.end()) { return defa; }
      return i->second;
    }
    const Value &find_ref(const Key &k, const Value &defa) const {
      auto i = map_.find(k);
      if (i == map_.end()) { return defa; }
      return i->second;
    }

    void erase(const Key &k) { map_.erase(k); }
    size_t size() const { return map_.size(); }

    using Mapping = dict_range_mapping<Key, Value>;
    using ConstMapping = c_dict_range_mapping<Key, Value>;

    Mapping all() {
      return Mapping(map_.begin(), map_.end());
    }
    ConstMapping all() const {
      return ConstMapping(map_.begin(), map_.end());
    }
  };

} // ns containers

// A dictionary
// Keys should have 'operator <'
template <typename K, typename V>
using Dict = containers::stl_map<K, V>;

template <class Mapping, typename Callable>
void for_each_keyvalue(Mapping &m, Callable fn) {
  while (m.have()) {
    fn(m.key(), m.value());
    m.advance();
  }
}

} // ns gluon
