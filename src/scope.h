#pragma once

#include <memory>

namespace scope::detail {

template <typename F> class DeferImpl {
  F func_;

public:
  DeferImpl(F &&func) : func_{std::forward<F>(func)} {}
  ~DeferImpl() { func_(); }
};

}; // namespace scope::detail

template <typename F> scope::detail::DeferImpl<F> defer(F &&func) {
  return scope::detail::DeferImpl<F>{std::forward<F>(func)};
}
