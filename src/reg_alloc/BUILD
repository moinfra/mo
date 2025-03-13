cc_library(
    name = "spillgen",
    srcs = ["spillgen.cc"],
    hdrs = ["spillgen.h"],
    deps = ["//src:machine"],
    visibility = ["//visibility:public"],
)


cc_library(
    name = "lsra",
    srcs = ["lsra.cc"],
    hdrs = ["lsra.h"],
    deps = [":spillgen"],
    visibility = ["//visibility:public"],
)
