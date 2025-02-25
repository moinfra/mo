#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <memory>

#include "ast_printer_yaml.h"
#include "parser.h"

int main(int argc, char *argv[])
{
    if (argc < 3)
    {
        std::cerr << "Usage: " << argv[0] << " --program|--expr <filename|source_code>" << std::endl;
        return 1;
    }

    std::string mode = argv[1];
    std::string source_code;

    if (mode == "--program")
    {
        if (argc == 4 && std::string(argv[2]) == "--code")
        {

            source_code = argv[3];
        }
        else if (argc == 3)
        {

            std::ifstream file(argv[2]);
            if (!file.is_open())
            {
                std::cerr << "Error: Could not open file " << argv[2] << std::endl;
                return 1;
            }
            std::stringstream buffer;
            buffer << file.rdbuf();
            source_code = buffer.str();
        }
        else
        {
            std::cerr << "Error: Invalid arguments for --program mode." << std::endl;
            return 1;
        }
    }
    else if (mode == "--expr")
    {

        if (argc == 3)
        {
            source_code = argv[2];
        }
        else
        {
            std::cerr << "Error: Invalid arguments for --expr mode." << std::endl;
            return 1;
        }
    }
    else
    {
        std::cerr << "Error: Unknown mode. Use --program or --expr." << std::endl;
        return 1;
    }

    Parser parser(source_code);
    std::unique_ptr<Program> program;
    ExprPtr expr;

    try
    {
        if (mode == "--program")
        {
            program = std::make_unique<Program>(parser.parse());
        }
        else if (mode == "--expr")
        {
            expr = parser.parse_expr();
        }
    }
    catch (const std::exception &e)
    {
        std::cerr << "Error: Parsing failed - " << e.what() << std::endl;
        return 1;
    }

    if (parser.errors().size() > 0) {
        std::cerr << "Errors:" << std::endl;
        for (const auto &error : parser.errors()) {
            std::cerr << error << std::endl;
        }
        return 1;
    }

    ASTPrinter printer;
    if (program)
    {
        std::cout << printer.print(*program) << std::endl;
    }
    else if (expr)
    {
        std::cout << printer.print(*expr) << std::endl;
    }

    return 0;
}
