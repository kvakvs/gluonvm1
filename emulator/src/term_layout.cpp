#include "term.h"
#include "heap.h"

namespace gluon {
namespace layout {

Word ProcBin::box_size(Word bytes) {
    return calculate_word_size(bytes) + box_extra_words;
}

Word HeapBin::box_size(Word bytes) {
    return calculate_word_size(bytes) + farheap_extra_words;
}

}  // ns layout
}  // ns gluon
