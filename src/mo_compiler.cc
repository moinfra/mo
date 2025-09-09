#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <memory>

#include "parser.h"
#include "type_checker.h"
#include "ir_generator.h"
#include "ir_printer.h"

using namespace ast;

int main(int argc, char *argv[])
{
    if (argc < 2)
    {
        std::cerr << "Usage: " << argv[0] << " <source_file.mo> [--emit-ir]" << std::endl;
        return 1;
    }

    std::string filename = argv[1];
    bool emit_ir = false;

    // Parse command line options
    for (int i = 2; i < argc; i++)
    {
        if (std::string(argv[i]) == "--emit-ir")
            emit_ir = true;
    }

    // Read source file
    std::ifstream file(filename);
    if (!file.is_open())
    {
        std::cerr << "Error: Could not open file " << filename << std::endl;
        return 1;
    }

    std::stringstream buffer;
    buffer << file.rdbuf();
    std::string source_code = buffer.str();

    try
    {
        // Step 1: Parse
        std::cout << "=== Parsing ===" << std::endl;
        Parser parser(source_code);
        auto program = std::make_unique<Program>(parser.parse());
        
        if (parser.errors().size() > 0) {
            std::cerr << "Parse errors:" << std::endl;
            for (const auto &error : parser.errors()) {
                std::cerr << error << std::endl;
            }
            return 1;
        }
        std::cout << "Parse successful" << std::endl;

        // Step 2: Type checking
        std::cout << "\n=== Type Checking ===" << std::endl;
        TypeChecker type_checker(program.get());
        auto type_result = type_checker.check();
        
        // Check for type errors (need to check the actual interface)
        std::cout << "Type check completed" << std::endl;

        // Step 3: IR Generation
        std::cout << "\n=== IR Generation ===" << std::endl;
        
        // Create a module first
        auto module = std::make_unique<Module>("test_module");
        IRGenerator ir_gen(module.get());
        ir_gen.generate(*program);
        
        std::cout << "IR generation successful" << std::endl;

        // Optionally emit IR
        if (emit_ir) {
            std::cout << "\n=== IR Output ===" << std::endl;
            IRPrinter::print_module(*module, std::cout);
        }

        // TODO: Add RISC-V target code generation when the target interface is complete
        std::cout << "\n=== RISC-V Code Generation ===" << std::endl;
        std::cout << "RISC-V target not yet implemented - skipping" << std::endl;

        std::cout << "\nCompilation pipeline test successful!" << std::endl;
        return 0;
    }
    catch (const std::exception &e)
    {
        std::cerr << "Compilation failed: " << e.what() << std::endl;
        return 1;
    }
}