cc_library(
    name = "machine_module",
    srcs = ["machine_module.cc"],
    hdrs = ["machine_module.h"],
    deps = [
        "//src:ir",
        "//src:machine",
    ],
    visibility = ["//visibility:public"],
)

cc_library(
    name = "instruction_selector",
    srcs = ["instruction_selector.cc"],
    hdrs = ["instruction_selector.h"],
    deps = [
        "//src:ir",
        "//src:machine",
    ],
    visibility = ["//visibility:public"],
)

cc_library(
    name = "assembly_emitter",
    srcs = ["assembly_emitter.cc"],
    hdrs = ["assembly_emitter.h"],
    deps = [
        "//src:machine",
    ],
    visibility = ["//visibility:public"],
)