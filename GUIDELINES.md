# Code Style Guide

This guide outlines the coding style conventions for this project. Adhering to these guidelines will help ensure code consistency and readability.

**1. General**

*   **Language:** C++17 or later.
*   **Coding Standard:** Follow modern C++ practices. Prefer clear, concise code.
*   **Naming Conventions:**
    *   Classes: `PascalCase` (e.g., `IntegerType`, `BasicBlock`).
    *   Variables: `snake_case` (e.g., `bit_width_`, `num_elements`). Use descriptive names.
    *   Functions/Methods: `snake_case` (e.g., `get_void_type`, `add_successor`).
    *   Constants: `ALL_CAPS` (e.g., `EQ`, `NE`). Use `constexpr` where appropriate.
    *   Enums/Enum Classes: `PascalCase` (e.g., `TypeID`, `Opcode`).
    *   Typedefs/Type Aliases: `PascalCase` (e.g., `ParamList`).
*   **File Naming:** `.h` for headers, `.cc` for source files.
*   **Indentation:** 4 spaces. No tabs.
*   **Line Length:**  Aim for a maximum of 120 characters per line.
*   **Comments:**  Use comments in **ENGLISH** to explain complex logic or non-obvious behavior. Keep comments concise and up-to-date.
*   **Error Handling:** Use assertions (`MO_ASSERT`) for internal checks and preconditions. Consider exceptions or error codes for external API error reporting (depending on project needs - *clarify this choice in a more complete guide*).
*   **Include Guards:** Use `#pragma once`.
*   **Header Organization:** Follow a consistent order for includes (e.g., standard library, project headers).

**2. Types and Classes**

*   **Immutability:**  Prefer immutable data structures where possible. Use `const` liberally.
*   **Smart Pointers:** Use `std::unique_ptr` for ownership. Consider `std::shared_ptr` only when shared ownership is genuinely needed.
*   **Virtual Functions:**  Declare virtual functions as `override` when overriding a base class method.
*   **Final Classes:** Use `final` to prevent inheritance when a class is not designed to be a base class.
