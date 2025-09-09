cc_test(
    name = "lexer_test",
    srcs = ["lexer_test.cc"],
    copts = ["-DNDEBUG"],
    deps = [
        "//src:lexer",
        "@googletest//:gtest_main",
    ],
)

cc_test(
    name = "parser_test",
    srcs = ["parser_test.cc"],
    copts = ["-DNDEBUG"],
    deps = [
        "//src:parser",
        "//src:ast_printer",
        "@googletest//:gtest_main",
    ],
)
cc_test(
    name = "type_checker_test",
    srcs = ["type_checker_test.cc"],
    copts = ["-DNDEBUG"],
    deps = [
        "//src:type_checker",
        "@googletest//:gtest_main",
    ],
)

cc_test(
    name = "ir_test",
    srcs = ["ir_test.cc"],
    copts = ["-DNDEBUG"],
    deps = [
        "//src:ir",
        "@googletest//:gtest_main",
    ],
)

cc_test(
    name = "ir_builder_test",
    srcs = ["ir_builder_test.cc"],
    copts = ["-DNDEBUG"],
    deps = [
        "//src:ir_builder",
        "@googletest//:gtest_main",
    ],
)

cc_test(
    name = "ir_generator_test",
    srcs = ["ir_generator_test.cc"],
    copts = ["-DNDEBUG"],
    deps = [
        "//src:ir_generator",
        "//src:ir_printer",
        "//src:type_checker",
        "@googletest//:gtest_main",
    ],
)

cc_test(
    name = "ir_printer_test",
    srcs = ["ir_printer_test.cc"],
    copts = ["-DNDEBUG"],
    deps = [
        "//src:ir_builder",
        "//src:ir_printer",
        "@googletest//:gtest_main",
    ],
)

cc_test(
    name = "machine_test",
    srcs = ["machine_test.cc"],
    copts = ["-DNDEBUG"],
    deps = [
        "//src:machine",
        "@googletest//:gtest_main",
    ],
)

cc_test(
    name = "lra_test",
    srcs = ["lra_test.cc"],
    copts = ["-DNDEBUG"],
    deps = [
        "//src:machine",
        "//src/targets:riscv_target",
        "@googletest//:gtest_main",
    ],
)

cc_test(
    name = "asimov_vm_test",
    srcs = ["asimov_vm_test.cc"],
    copts = ["-DNDEBUG"],
    deps = [
        "//src:machine",
        "//src/targets:asimov_target",
        "//src/vm:asimov_vm",
        "@googletest//:gtest_main",
    ],
)

cc_test(
    name = "lsra_test",
    srcs = ["lsra_test.cc"],
    copts = ["-DNDEBUG"],
    deps = [
        "//src:machine",
        "//src/targets:asimov_target",
        "//src/vm:asimov_vm",
        "//src/reg_alloc:lsra",
        "@googletest//:gtest_main",
    ],
)

cc_test(
    name = "target_base_test",
    srcs = ["target_base_test.cc"],
    copts = ["-DNDEBUG"],
    deps = [
        "//src:ir",
        "//src:machine",
        "//src/targets:target_base",
        "@googletest//:gtest_main",
    ],
)

cc_test(
    name = "riscv_target_test",
    srcs = ["riscv_target_test.cc"],
    copts = ["-DNDEBUG"],
    deps = [
        "//src:ir",
        "//src:machine", 
        "//src/targets:riscv_target",
        "@googletest//:gtest_main",
    ],
)

cc_test(
    name = "machine_module_test",
    srcs = ["machine_module_test.cc"],
    copts = ["-DNDEBUG"],
    deps = [
        "//src:ir",
        "//src:machine",
        "//src/codegen:machine_module", 
        "//src/targets:riscv_target",
        "@googletest//:gtest_main",
    ],
)

cc_test(
    name = "instruction_selector_test",
    srcs = ["instruction_selector_test.cc"],
    copts = ["-DNDEBUG"],
    deps = [
        "//src:ir",
        "//src:machine",
        "//src/codegen:instruction_selector",
        "//src/targets:riscv_instruction_selector",
        "@googletest//:gtest_main",
    ],
)

cc_test(
    name = "assembly_emitter_test",
    srcs = ["assembly_emitter_test.cc"],
    copts = ["-DNDEBUG"],
    deps = [
        "//src:ir",
        "//src:machine",
        "//src/codegen:assembly_emitter",
        "//src/targets:riscv_assembly_emitter",
        "@googletest//:gtest_main",
    ],
)

cc_test(
    name = "end_to_end_basic_test",
    srcs = ["end_to_end_basic_test.cc"],
    copts = ["-DNDEBUG"],
    deps = [
        "//src:ir",
        "//src:ir_builder",
        "//src:machine",
        "//src/codegen:machine_module",
        "//src/targets:riscv_target",
        "//src/targets:riscv_instruction_selector",
        "//src/targets:riscv_assembly_emitter",
        "@googletest//:gtest_main",
    ],
)

cc_test(
    name = "simple_instruction_selection_test",
    srcs = ["simple_instruction_selection_test.cc"],
    copts = ["-DNDEBUG"],
    deps = [
        "//src:ir",
        "//src:ir_builder",
        "//src:machine",
        "//src/codegen:machine_module",
        "//src/targets:riscv_target",
        "//src/targets:riscv_instruction_selector",
        "//src/targets:riscv_assembly_emitter",
        "@googletest//:gtest_main",
    ],
)

cc_test(
    name = "function_lowering_test",
    srcs = ["function_lowering_test.cc"],
    copts = ["-DNDEBUG"],
    deps = [
        "//src:ir",
        "//src:ir_builder",
        "//src:machine",
        "//src/codegen:machine_module",
        "//src/targets:riscv_target",
        "//src/targets:riscv_instruction_selector",
        "//src/targets:riscv_assembly_emitter",
        "@googletest//:gtest_main",
    ],
)

cc_test(
    name = "control_flow_test",
    srcs = ["control_flow_test.cc"],
    copts = ["-DNDEBUG"],
    deps = [
        "//src:ir",
        "//src:ir_builder",
        "//src:machine",
        "//src/codegen:machine_module",
        "//src/targets:riscv_target",
        "//src/targets:riscv_instruction_selector",
        "//src/targets:riscv_assembly_emitter",
        "@googletest//:gtest_main",
    ],
)
