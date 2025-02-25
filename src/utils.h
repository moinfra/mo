#include <cstdarg>
#include <cstdio>
void debug(const char *fmt, ...)
{
    std::printf("[DEBUG] ");
    va_list args;
    va_start(args, fmt);
    std::vprintf(fmt, args);
    va_end(args);
    std::printf("\n");
}
