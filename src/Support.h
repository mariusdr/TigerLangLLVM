#ifndef TIGER_LANG_SUPPORT_H
#define TIGER_LANG_SUPPORT_H

#include "string"
#include "vector"
#include "stack"
#include "llvm/Support/Allocator.h"
#include "llvm/Support/AllocatorBase.h"
#include "llvm/Support/FormatVariadic.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/raw_os_ostream.h"
#include <iostream>

namespace tig {

template <typename... Ts> 
void debug(const char *fmt, Ts &&...vals) {
  llvm::raw_os_ostream os(std::cout);
  os << llvm::formatv(fmt, std::forward<Ts>(vals)...);
}

template <typename... Ts> 
void err(const char *fmt, Ts &&...vals) {
  llvm::raw_os_ostream os(std::cerr);
  os << llvm::formatv(fmt, std::forward<Ts>(vals)...);
}

namespace detail {

template <class LlvmAlloc, typename T> 
class LlvmStlAlloc {
  using AllocTy = llvm::AllocatorBase<LlvmAlloc>;
public:
  using value_type = T;
  using size_type = size_t;
  using difference_type = ptrdiff_t;

  using pointer = T *;
  using const_pointer = const T *;
  using void_pointer = void *;
  using const_void_pointer = const void *;
  using reference = T &;
  using const_reference = const T &;

  using propagate_on_container_copy_assignment = std::false_type;
  using propagate_on_container_move_assignment = std::true_type;
  using propagate_on_container_swap = std::true_type;
  using is_always_equal = std::false_type;

  LlvmStlAlloc(AllocTy &alloc) noexcept:
    impl(&alloc) 
  {}
  
  LlvmStlAlloc(AllocTy *alloc) noexcept:
    impl(alloc)
  {}

  ~LlvmStlAlloc() noexcept {}

  template <typename U> 
  LlvmStlAlloc(const LlvmStlAlloc<LlvmAlloc, U> &other) noexcept:
    impl(other.impl)
  {}

  template <typename U> 
  LlvmStlAlloc(LlvmStlAlloc<LlvmAlloc, U> &&other) noexcept:
    impl(std::move(other.impl))
  {}

  template <typename U> 
  LlvmStlAlloc<LlvmAlloc, T> &operator=(const LlvmStlAlloc<LlvmAlloc, U> &other) noexcept {
    impl = other.impl;
    return *this;
  }

  template <typename U> 
  LlvmStlAlloc<LlvmAlloc, T> &operator=(LlvmStlAlloc<LlvmAlloc, U> &&other) noexcept {
    impl = std::move(other.impl);
    return *this;
  }

  pointer address(reference r) noexcept {
    return &r;
  }

  const_pointer address(const_reference r) noexcept {
    return &r;
  }

  [[nodiscard]] pointer allocate(size_type cnt) {
    const size_t bytes = cnt * sizeof(value_type);
    constexpr size_t align = alignof(value_type);
    return static_cast<pointer>(impl->Allocate(bytes, align));
  }

  void deallocate(pointer ptr, size_type cnt) {
    const size_t bytes = cnt * sizeof(value_type);
    constexpr size_t align = alignof(value_type);
    impl->Deallocate(static_cast<void *>(ptr), bytes, align);
  }

  LlvmStlAlloc *select_on_container_copy_construction() const noexcept {
    return *this;
  }

  bool operator==(const LlvmStlAlloc &) const noexcept {
    return false;
  }

  bool operator!=(const LlvmStlAlloc &) const noexcept {
    return true;
  }

private:
  AllocTy *impl;
};

} // namespace

template <typename T, class AllocImpl>
using AllocVector = std::vector<
  T, 
  detail::LlvmStlAlloc<AllocImpl, T>>;

template <typename T, class AllocImpl>
using AllocStack = std::stack<
  T, 
  AllocVector<T, AllocImpl>>;

template <typename T, class AllocImpl, size_t Reserve = 0> 
AllocStack<T, AllocImpl> emptyStack(llvm::AllocatorBase<AllocImpl> &alloc) {
  AllocVector<T, AllocImpl> vec(alloc);
  if constexpr (Reserve) {
    vec.reserve(Reserve);
  }
  return AllocStack<T, AllocImpl>(std::move(vec));
}

template <class AllocImpl>
using AllocString = std::basic_string<
  char,
  std::char_traits<char>,
  detail::LlvmStlAlloc<AllocImpl, char>>;

namespace detail {

template <typename T, class AllocImpl>
class LlvmStlDeleter {
  using AllocTy = llvm::AllocatorBase<AllocImpl>;
public:
  LlvmStlDeleter() noexcept : alloc(nullptr) {}
  LlvmStlDeleter(AllocTy *alloc) noexcept : alloc(alloc) {}
  LlvmStlDeleter(AllocTy &alloc) noexcept : alloc(&alloc) {}
  LlvmStlDeleter(const LlvmStlDeleter &rhs) noexcept : alloc(rhs.alloc) {}
  LlvmStlDeleter(LlvmStlDeleter &&rhs) noexcept : alloc(std::move(rhs.alloc)) {}

  LlvmStlDeleter &operator=(const LlvmStlDeleter &rhs) noexcept {
    alloc = rhs.alloc;
    return *this;
  }
  
  LlvmStlDeleter &operator=(LlvmStlDeleter &&rhs) noexcept {
    alloc = std::move(rhs.alloc);
    return *this;
  }

  void operator()(T *ptr) const {
    if (ptr) {
      ptr->~T();
    } 
    if (alloc) {
      alloc->Deallocate(ptr, 1);
    }
  }

private:
  llvm::AllocatorBase<AllocImpl> *alloc;
};

} // namespace

template <typename T, class AllocImpl>
using AllocPtr = std::unique_ptr<T, detail::LlvmStlDeleter<T, AllocImpl>>;

template <typename T, class AllocImpl, typename... Args>
[[nodiscard]] AllocPtr<T, AllocImpl> allocateUnique(llvm::AllocatorBase<AllocImpl> &alloc, Args &&...args) {
  void *addr = alloc.Allocate(sizeof(T), alignof(T));
  if (LLVM_UNLIKELY(!addr)) {
    return nullptr;
  }
  T *obj = new (addr) T(std::forward<Args>(args)...);
  return {obj, detail::LlvmStlDeleter<T, AllocImpl>(alloc)};
}

template <typename T>
using AllocSharedPtr = std::shared_ptr<T>;

template <typename T, class AllocImpl, typename... Args>
[[nodiscard]] AllocSharedPtr<T> allocateShared(llvm::AllocatorBase<AllocImpl> &alloc, Args &&...args) {
  void *addr = alloc.Allocate(sizeof(T), alignof(T));
  if (LLVM_UNLIKELY(!addr)) {
    return nullptr;
  }
  T *obj = new (addr) T(std::forward<Args>(args)...);
  return {obj, detail::LlvmStlDeleter<T, AllocImpl>(alloc)};
}

template <typename T>
using AllocWeakPtr = std::shared_ptr<T>;

template <typename T, class AllocImpl, typename... Args>
[[nodiscard]] AllocWeakPtr<T> allocateWeak(llvm::AllocatorBase<AllocImpl> &alloc, Args &&...args) {
  void *addr = alloc.Allocate(sizeof(T), alignof(T));
  if (LLVM_UNLIKELY(!addr)) {
    return nullptr;
  }
  T *obj = new (addr) T(std::forward<Args>(args)...);
  return {obj, detail::LlvmStlDeleter<T, AllocImpl>(alloc)};
}

} // namespace
#endif
