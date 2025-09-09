cc_library(
    name = "target_base",
    srcs = ["target_base.cc"],
    hdrs = ["target_base.h"],
    visibility = ["//visibility:public"],
)

cc_library(
    name = "riscv_target",
    srcs = ["riscv_target.cc"],
    hdrs = ["riscv_target.h"],
    deps = [
        ":target_base",
        "//src:machine"
    ],
    visibility = ["//visibility:public"],
)

cc_library(
    name = "asimov_target",
    srcs = ["asimov_target.cc"],
    hdrs = ["asimov_target.h"],
    deps = ["//src:machine"],
    visibility = ["//visibility:public"],
)
