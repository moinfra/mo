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
    features = ["cpp_standard"],
)
