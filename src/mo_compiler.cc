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
#include "targets/arm64_target.h"
#include "targets/arm64_instruction_selector.h"
#include "targets/arm64_assembly_emitter.h"
#include "reg_alloc/lsra.h"

using namespace ast;

// Enum for target architecture selection
enum class TargetArch {
    RISCV64,
    ARM64
};

int main(int argc, char *argv[])
{
    if (argc < 2)
    {
        std::cerr << "Usage: " << argv[0] << " <source_file.mo> [--emit-ir] [--target=<arch>]" << std::endl;
        std::cerr << "  Supported targets: riscv64, arm64" << std::endl;
        return 1;
    }

    std::string filename = argv[1];
    bool emit_ir = false;
    TargetArch target_arch = TargetArch::RISCV64;  // Default to RISC-V

    // Parse command line options
    for (int i = 2; i < argc; i++)
    {
        std::string arg = argv[i];
        if (arg == "--emit-ir") {
            emit_ir = true;
        } else if (arg.substr(0, 9) == "--target=") {
            std::string target = arg.substr(9);
            if (target == "riscv64" || target == "riscv") {
                target_arch = TargetArch::RISCV64;
            } else if (target == "arm64" || target == "aarch64") {
                target_arch = TargetArch::ARM64;
            } else {
                std::cerr << "Error: Unknown target architecture: " << target << std::endl;
                std::cerr << "Supported targets: riscv64, arm64" << std::endl;
                return 1;
            }
        } else {
            std::cerr << "Warning: Unknown option: " << arg << std::endl;
        }
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

        // Step 4: Target-Specific Code Generation
        const char* target_name = (target_arch == TargetArch::ARM64) ? "ARM64" : "RISC-V";
        std::cout << "\n=== " << target_name << " Code Generation ===" << std::endl;
        
        try {
            // Create machine module first
            auto machine_module = std::make_unique<MachineModule>(module.get());
            
            // Target-specific setup
            if (target_arch == TargetArch::ARM64) {
                // Create ARM64 target with Darwin ABI (for macOS M2)
                auto reg_info = std::make_unique<ARM64::ARM64RegisterInfo>(ARM64::ABIVersion::DARWIN);
                auto inst_info = std::make_unique<ARM64::ARM64TargetInstInfo>(ARM64::ABIVersion::DARWIN);
                auto arm64_target = std::make_unique<ARM64::ARM64Target>(std::move(reg_info), std::move(inst_info), ARM64::ABIVersion::DARWIN);
                
                const auto* target_reg_info = arm64_target->getRegisterInfo();
                const auto* target_inst_info = arm64_target->getInstInfo();
                machine_module->set_target_info(target_reg_info, target_inst_info);
                
                std::cout << "Converting IR to machine representation..." << std::endl;
                
                for (const auto& ir_function : module->functions()) {
                    std::cout << "Processing function: " << ir_function->name() << std::endl;
                    
                    auto* machine_func = machine_module->create_machine_function(ir_function);
                    
                    // ARM64 instruction selection
                    ARM64::ARM64InstructionSelector selector(target_reg_info, target_inst_info);
                    bool selection_success = selector.selectInstructions(*ir_function, *machine_func);
                    
                    if (!selection_success) {
                        std::cerr << "ARM64 instruction selection failed for function: " 
                                  << ir_function->name() << std::endl;
                        return 1;
                    }
                    
                    std::cout << "ARM64 instruction selection successful for: " << ir_function->name() << std::endl;
                    
                    // Register allocation
                    std::cout << "Running register allocation for: " << ir_function->name() << std::endl;
                    machine_func->live_range_analyzer()->compute();
                    
                    auto allocator = std::make_unique<LinearScanRegisterAllocator>(*machine_func);
                    RegAllocResult alloc_result = allocator->allocate_registers();
                    
                    if (!alloc_result.successful) {
                        std::cerr << "ARM64 LSRA failed: " << alloc_result.error_message << std::endl;
                        return 1;
                    }
                    
                    std::cout << "ARM64 LSRA successful: " << alloc_result.num_spills << " spills, " 
                             << alloc_result.num_copies << " copies" << std::endl;
                    
                    machine_func->set_register_allocation_result(alloc_result);
                    allocator->apply();
                    machine_func->set_register_allocator(std::move(allocator));
                }
                
                // Generate ARM64 assembly
                ARM64::ARM64AssemblyEmitter emitter(arm64_target->getRegisterInfo(), arm64_target->getInstInfo());
                std::string assembly = emitter.emit(*machine_module);
                
                std::cout << "\n--- Generated ARM64 Assembly ---" << std::endl;
                std::cout << assembly << std::endl;
                
                // Write assembly to file with .s extension
                std::string output_filename = filename.substr(0, filename.find_last_of('.')) + ".s";
                std::ofstream asm_file(output_filename);
                if (asm_file.is_open()) {
                    asm_file << assembly;
                    asm_file.close();
                    std::cout << "ARM64 assembly written to: " << output_filename << std::endl;
                } else {
                    std::cerr << "Warning: Could not write ARM64 assembly to file " << output_filename << std::endl;
                }
                
                std::cout << "\nARM64 code generation successful!" << std::endl;
                
            } else {
                // Original RISC-V path
                // Create RISC-V target with LP64D ABI (64-bit with double-precision floating-point)
                auto reg_info = std::make_unique<RISCV::RISCVRegisterInfo>(RISCV::ABIVersion::LP64D);
                auto inst_info = std::make_unique<RISCV::RISCVTargetInstInfo>(RISCV::ABIVersion::LP64D);
                auto riscv_target = std::make_unique<RISCV::RISCVTarget>(std::move(reg_info), std::move(inst_info), RISCV::ABIVersion::LP64D);
                
                const auto* target_reg_info = riscv_target->getRegisterInfo();
                const auto* target_inst_info = riscv_target->getInstInfo();
                machine_module->set_target_info(target_reg_info, target_inst_info);
                
                
                for (const auto& ir_function : module->functions()) {
                std::cout << "Processing function: " << ir_function->name() << std::endl;
                
                // Create machine function using the module's factory method
                auto* machine_func = machine_module->create_machine_function(ir_function);
                
                // Run instruction selection  
                
                RISCV::RISCVInstructionSelector selector(target_reg_info, target_inst_info);
                bool selection_success = selector.selectInstructions(*ir_function, *machine_func);
                
                if (!selection_success) {
                    std::cerr << "Instruction selection failed for function: " 
                              << ir_function->name() << std::endl;
                    return 1;
                }
                
                std::cout << "Instruction selection successful for: " << ir_function->name() << std::endl;
                
                // Run register allocation on the machine function
                std::cout << "Running register allocation for: " << ir_function->name() << std::endl;
                
                // Compute live ranges first
                std::cout << "Computing live ranges..." << std::endl;
                machine_func->live_range_analyzer()->compute();
                
                // Create linear scan register allocator
                auto allocator = std::make_unique<LinearScanRegisterAllocator>(*machine_func);
                
                // Perform register allocation using LSRA
                std::cout << "Running LSRA algorithm..." << std::endl;
                RegAllocResult alloc_result = allocator->allocate_registers();
                
                if (!alloc_result.successful) {
                    std::cerr << "LSRA failed: " << alloc_result.error_message << std::endl;
                    return 1;
                }
                
                std::cout << "LSRA successful: " << alloc_result.num_spills << " spills, " 
                         << alloc_result.num_copies << " copies" << std::endl;
                
                // Store the allocation result and allocator in machine function for assembly generation
                machine_func->set_register_allocation_result(alloc_result);
                
                // Apply register allocation results to machine instructions
                allocator->apply();
                std::cout << "Applied register allocation to machine instructions" << std::endl;
                
                // Store the allocator in the machine function for later access during assembly
                machine_func->set_register_allocator(std::move(allocator));
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
                    std::cout << "RISC-V assembly written to: " << output_filename << std::endl;
                } else {
                    std::cerr << "Warning: Could not write RISC-V assembly to file " << output_filename << std::endl;
                }
                
                std::cout << "\nRISC-V code generation successful!" << std::endl;
            }
        }
        catch (const std::exception& e) {
            std::cerr << target_name << " code generation failed: " << e.what() << std::endl;
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