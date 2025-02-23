cc_library(
    name = "lexer",
    srcs = ["lexer.cc"],
    hdrs = ["lexer.h"],
    visibility = ["//visibility:public"],
)

cc_library(
    name = "parser",
    srcs = ["parser.cc"],
    hdrs = ["parser.h", "ast.h"],
    deps = [":lexer"],
    visibility = ["//visibility:public"],
)
