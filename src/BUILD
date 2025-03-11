cc_library(
    name = "utils",
    srcs = ["mo_debug.cc"],
    hdrs = ["mo_debug.h"],
    visibility = ["//visibility:public"],
)

cc_library(
    name = "lexer",
    srcs = ["lexer.cc"],
    hdrs = ["lexer.h"],
    deps = [":utils",
        # "@icu//:icuuc", # unicode identifier support
    ],
    visibility = ["//visibility:public"],
)

cc_library(
    name = "parser",
    srcs = ["parser.cc", "ast.cc", "ast_type.cc"],
    hdrs = ["parser.h", "ast.h", "ast_type.h"],
    deps = [":lexer"],
    visibility = ["//visibility:public"],
)

cc_library(
    name = "type_checker",
    srcs = ["type_checker.cc", "ast.cc", "ast_type.cc", "ast_scope.cc"],
    hdrs = ["type_checker.h", "ast.h", "ast_type.h", "ast_scope.h"],
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
    deps = [":utils"],
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
    name = "ir_generator",
    srcs = ["ir_generator.cc", "ir_scope.cc"],
    hdrs = ["ir_generator.h", "ir_scope.h"],
    deps = [":parser", ":ir_builder"],
    visibility = ["//visibility:public"],
)

cc_library(
    name = "ir_printer",
    # srcs = ["ir_printer.cc"],
    hdrs = ["ir_printer.h"],
    deps = [":ir"],
    visibility = ["//visibility:public"],
)

cc_library(
    name = "machine_frame",
    srcs = ["machine_frame.cc"],
    hdrs = ["machine_frame.h"],
    deps = [":ir"],
    visibility = ["//visibility:public"],
)

cc_library(
    name = "machine",
    srcs = ["machine.cc", "lra.cc", "reg_alloc.cc"],
    hdrs = ["machine.h", "lra.h", "reg_alloc.h"],
    deps = [":utils", ":ir", ":machine_frame"],
    visibility = ["//visibility:public"],
)
