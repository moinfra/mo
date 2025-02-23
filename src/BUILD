cc_library(
    name = "lexer",
    srcs = ["lexer.cc"],
    hdrs = ["lexer.h"],
    visibility = ["//visibility:public"],
)

cc_library(
    name = "parser",
    srcs = ["parser.cc", "ast.cc"],
    hdrs = ["parser.h", "ast.h"],
    deps = [":lexer"],
    visibility = ["//visibility:public"],
)

cc_library(
    name = "ast_printer",
    srcs = ["ast_printer.cc"],
    hdrs = ["ast_printer.h"],
    deps = [":parser"],
    visibility = ["//visibility:public"],
)
