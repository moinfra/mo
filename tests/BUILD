cc_test(
    name = "lexer_test",
    srcs = ["lexer_test.cc"],
    deps = [
        "//src:lexer",
        "@googletest//:gtest_main",
    ],
)

cc_test(
    name = "parser_test",
    srcs = ["parser_test.cc"],
    deps = [
        "//src:parser",
        "//src:ast_printer",
        "@googletest//:gtest_main",
    ],
)
cc_test(
    name = "type_checker_test",
    srcs = ["type_checker_test.cc"],
    deps = [
        "//src:type_checker",
        "@googletest//:gtest_main",
    ],
)

cc_test(
    name = "ir_test",
    srcs = ["ir_test.cc"],
    deps = [
        "//src:ir",
        "@googletest//:gtest_main",
    ],
)

cc_test(
    name = "ir_builder_test",
    srcs = ["ir_builder_test.cc"],
    deps = [
        "//src:ir_builder",
        "@googletest//:gtest_main",
    ],
)

cc_test(
    name = "ir_printer_test",
    srcs = ["ir_printer_test.cc"],
    deps = [
        "//src:ir_builder",
        "//src:ir_printer",
        "@googletest//:gtest_main",
    ],
)
