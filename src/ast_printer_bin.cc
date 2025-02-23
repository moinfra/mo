#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include "ast_printer.h"
#include "parser.h"  // 假设这是解析文件的类

int main(int argc, char* argv[]) {
    if (argc != 2) {
        std::cerr << "Usage: " << argv[0] << " <filename>" << std::endl;
        return 1;
    }

    std::ifstream file(argv[1]);
    if (!file.is_open()) {
        std::cerr << "Error: Could not open file " << argv[1] << std::endl;
        return 1;
    }

    std::stringstream buffer;
    buffer << file.rdbuf();
    std::string file_content = buffer.str();

    Parser parser(file_content);
    Program program = parser.parse();

    // 打印 AST
    ASTPrinter printer;
    std::string ast_str = printer.print(program);
    std::cout << ast_str << std::endl;

    return 0;
}
