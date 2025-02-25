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
    name = "ast_printer_yaml",
    srcs = ["ast_printer_yaml.cc"],
    hdrs = ["ast_printer_yaml.h"],
    deps = [":parser"],
    visibility = ["//visibility:public"],
)

cc_library(
    name = "ast_printer",
    srcs = ["ast_printer.cc"],
    hdrs = ["ast_printer.h"],
    deps = [":parser"],
    visibility = ["//visibility:public"],
)

cc_binary(
    name = "print_ast",
    srcs = ["ast_printer_bin.cc"],
    deps = [":ast_printer_yaml"],
)

cc_library(
    name = "ir",
    srcs = ["ir.cc"],
    hdrs = ["ir.h"],
    visibility = ["//visibility:public"],
)

cc_library(
    name = "ir_builder",
    srcs = ["ir_builder.cc"],
    hdrs = ["ir_builder.h"],
    deps = [":ir"],
    visibility = ["//visibility:public"],
)

cc_library(
    name = "ir_printer",
    # srcs = ["ir_printer.cc"],
    hdrs = ["ir_printer.h"],
    deps = [":ir"],
    visibility = ["//visibility:public"],
)
