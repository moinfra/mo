#pragma once
#include <sstream>
#include <algorithm>
#include <string>
#include <vector>

#include "ast.h"

class ASTPrinter {
public:
    std::string print(const Program& program);
    std::string print(const Expr& expr);
    std::string print(const Statement& stmt);
    std::string print(const Type& type);
    std::string print(const StructDecl& struct_decl);
    std::string print(const FunctionDecl& func_decl);
    std::string print(const ImplBlock& impl_block);
    std::string print(const VarDeclStmt& var_decl);
    std::string print(const GlobalDecl& global_decl);

    // Expressions
    std::string visit(const VariableExpr& expr);
    std::string visit(const IntegerLiteralExpr& expr);
    std::string visit(const FloatLiteralExpr& expr);
    std::string visit(const StringLiteralExpr& expr);
    std::string visit(const BinaryExpr& expr);
    std::string visit(const UnaryExpr& expr);
    std::string visit(const CallExpr& expr);
    std::string visit(const MemberAccessExpr& expr);
    std::string visit(const CastExpr& expr);
    std::string visit(const SizeofExpr& expr);
    std::string visit(const InitListExpr& expr);
    std::string visit(const FunctionPointerExpr& expr);
    std::string visit(const StructLiteralExpr& expr);

    // Statements
    std::string visit(const BlockStmt& stmt);
    std::string visit(const ReturnStmt& stmt);
    std::string visit(const IfStmt& stmt);
    std::string visit(const WhileStmt& stmt);
    std::string visit(const BreakStmt& stmt);
    std::string visit(const ContinueStmt& stmt);
    std::string visit(const ExprStmt& stmt);

private:
    std::string indent(int level) const;
    std::string indent_current() const { return indent(current_indent); }
    void enter_scope();
    void leave_scope();

    int current_indent = 0;

    // Helper function to escape strings for JSON
    std::string escape_json_string(const std::string& str) {
        std::string result;
        for (char c : str) {
            switch (c) {
            case '\\': result += "\\\\"; break;
            case '"': result += "\\\""; break;
            case '\b': result += "\\b"; break;
            case '\f': result += "\\f"; break;
            case '\n': result += "\\n"; break;
            case '\r': result += "\\r"; break;
            case '\t': result += "\\t"; break;
            default: result += c;
            }
        }
        return result;
    }

    // Helper function to create a JSON key-value pair
    std::string json_pair(const std::string& key, const std::string& value, int level) {
        return "\"" + escape_json_string(key) + "\": " + value;
    }

    // Helper function to create a JSON array from a vector of strings
    std::string json_array(const std::vector<std::string>& values, int level) {
        std::string result = "[\n";
        for (size_t i = 0; i < values.size(); ++i) {
            result += indent(level) + values[i];
            if (i != values.size() - 1) {
                result += ",";
            }
            result += "\n";
        }
        result += indent(level) + "]";
        return result;
    }

    // Overload json_pair and json_array to use current_indent
    std::string json_pair(const std::string& key, const std::string& value) {
        return json_pair(key, value, current_indent);
    }
    std::string json_array(const std::vector<std::string>& values) {
        return json_array(values, current_indent);
    }
};
