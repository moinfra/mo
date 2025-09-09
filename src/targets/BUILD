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
    name = "riscv_instruction_selector",
    srcs = ["riscv_instruction_selector.cc"],
    hdrs = ["riscv_instruction_selector.h"],
    deps = [
        ":riscv_target",
        "//src/codegen:instruction_selector",
    ],
    visibility = ["//visibility:public"],
)

cc_library(
    name = "riscv_assembly_emitter",
    srcs = ["riscv_assembly_emitter.cc"],
    hdrs = ["riscv_assembly_emitter.h"],
    deps = [
        ":riscv_target",
        "//src/codegen:assembly_emitter",
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
