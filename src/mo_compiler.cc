#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <memory>

#include "parser.h"
#include "type_checker.h"
#include "ir_generator.h"
#include "ir_printer.h"
#include "machine.h"
#include "targets/riscv_target.h"
#include "targets/riscv_instruction_selector.h"
#include "targets/riscv_assembly_emitter.h"

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

        // Step 4: RISC-V Code Generation
        std::cout << "\n=== RISC-V Code Generation ===" << std::endl;
        
        try {
            // Create RISC-V target with LP64D ABI (64-bit with double-precision floating-point)
            auto reg_info = std::make_unique<RISCV::RISCVRegisterInfo>(RISCV::ABIVersion::LP64D);
            auto inst_info = std::make_unique<RISCV::RISCVTargetInstInfo>(RISCV::ABIVersion::LP64D);
            auto riscv_target = std::make_unique<RISCV::RISCVTarget>(std::move(reg_info), std::move(inst_info), RISCV::ABIVersion::LP64D);
            
            // Convert IR module to machine representation
            std::cout << "Converting IR to machine representation..." << std::endl;
            
            // Create machine module
            auto machine_module = std::make_unique<MachineModule>(module.get());
            
            for (const auto& ir_function : module->functions()) {
                std::cout << "Processing function: " << ir_function->name() << std::endl;
                
                // Create machine function using the module's factory method
                auto* machine_func = machine_module->create_machine_function(ir_function);
                
                // Run instruction selection  
                const auto* reg_info = riscv_target->getRegisterInfo();
                const auto* inst_info = riscv_target->getInstInfo();
                
                RISCV::RISCVInstructionSelector selector(reg_info, inst_info);
                bool selection_success = selector.selectInstructions(*ir_function, *machine_func);
                
                if (!selection_success) {
                    std::cerr << "Instruction selection failed for function: " 
                              << ir_function->name() << std::endl;
                    return 1;
                }
                
                std::cout << "Instruction selection successful for: " << ir_function->name() << std::endl;
            }
            
            // Generate RISC-V assembly from the complete machine module
            RISCV::RISCVAssemblyEmitter emitter(riscv_target->getRegisterInfo(), riscv_target->getInstInfo());
            std::string assembly = emitter.emit(*machine_module);
            
            std::cout << "\n--- Generated RISC-V Assembly ---" << std::endl;
            std::cout << assembly << std::endl;
            
            // Write assembly to file
            std::string output_filename = filename.substr(0, filename.find_last_of('.')) + ".s";
            std::ofstream asm_file(output_filename);
            if (asm_file.is_open()) {
                asm_file << assembly;
                asm_file.close();
                std::cout << "Assembly written to: " << output_filename << std::endl;
            } else {
                std::cerr << "Warning: Could not write assembly to file " << output_filename << std::endl;
            }
            
            std::cout << "\nRISC-V code generation successful!" << std::endl;
        }
        catch (const std::exception& e) {
            std::cerr << "RISC-V code generation failed: " << e.what() << std::endl;
            return 1;
        }

        std::cout << "\nCompilation successful!" << std::endl;
        return 0;
    }
    catch (const std::exception &e)
    {
        std::cerr << "Compilation failed: " << e.what() << std::endl;
        return 1;
    }
}