#pragma once

#include <cstdio>
#include <cstdlib>
#include <cassert>

#define MO_DEBUG(fmt, ...)                              \
    do                                                  \
    {                                                   \
        std::printf("[D] %s:%d: ", __FILE__, __LINE__); \
        std::printf(fmt, ##__VA_ARGS__);                \
        std::putchar('\n');                             \
    } while (0)

#define MO_WARN(fmt, ...)                               \
    do                                                  \
    {                                                   \
        std::printf("[W] %s:%d: ", __FILE__, __LINE__); \
        std::printf(fmt, ##__VA_ARGS__);                \
        std::putchar('\n');                             \
    } while (0)

#define MO_ERROR(fmt, ...)                                       \
    do                                                           \
    {                                                            \
        std::fprintf(stderr, "[E] %s:%d: ", __FILE__, __LINE__); \
        std::fprintf(stderr, fmt, ##__VA_ARGS__);                \
        std::fputc('\n', stderr);                                \
    } while (0)

#ifdef NDEBUG
#define MO_ASSERT(cond, fmt, ...) ((void)0)
#else
#define MO_ASSERT(cond, fmt, ...)                                          \
    do                                                                     \
    {                                                                      \
        if (!(cond))                                                       \
        {                                                                  \
            MO_ERROR("Assertion '%s' failed. " fmt, #cond, ##__VA_ARGS__); \
            std::abort();                                                  \
        }                                                                  \
    } while (0)
#endif

#define MO_NOT_IMPLEMENTED()                                            \
    do                                                                  \
    {                                                                   \
        MO_ERROR("Not implemented code at %s:%d.", __FILE__, __LINE__); \
        std::abort();                                                   \
    } while (0)

#define MO_UNREACHABLE()                                            \
    do                                                              \
    {                                                               \
        MO_ERROR("Unreachable code at %s:%d.", __FILE__, __LINE__); \
        std::abort();                                               \
    } while (0)

#define MO_NOP(x) ((void)(x))
