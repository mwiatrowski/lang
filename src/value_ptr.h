#pragma once

#include <cassert>
#include <type_traits>
#include <utility>

template <typename T> class ValuePtr {
    T *rawPtr_ = nullptr;

  public:
    template <typename... Args> ValuePtr(Args &&...args) : rawPtr_(new T(std::forward<Args>(args)...)) {}

    ~ValuePtr() { delete rawPtr_; }
    ValuePtr(ValuePtr const &other) {
        if (other.hasValue()) {
            rawPtr_ = new T(*other.rawPtr_);
        }
    }
    ValuePtr(ValuePtr &&other) noexcept : rawPtr_(std::exchange(other.rawPtr_, nullptr)) {}
    ValuePtr &operator=(ValuePtr other) noexcept {
        std::swap(rawPtr_, other.rawPtr_);
        return *this;
    }

    T &operator*() const noexcept {
        assert(hasValue());
        return *rawPtr_;
    }
    T *operator->() const noexcept {
        assert(hasValue());
        return rawPtr_;
    }

    bool hasValue() const { return rawPtr_ != nullptr; }
};
