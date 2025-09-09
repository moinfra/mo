# Test Coverage for Mo Compiler

This document explains how to use the test coverage measurement functionality for the Mo compiler project.

## Quick Start

### Using the Coverage Script
```bash
# Run coverage for all tests
./scripts/coverage.sh

# Run coverage for specific test target
./scripts/coverage.sh //tests:control_flow_test

# Run coverage for RISC-V tests only
./scripts/coverage.sh //tests:riscv_target_test
```

### Using Make
```bash
# Run coverage for all tests
make coverage

# Run coverage for specific target
make coverage TARGET=//tests:control_flow_test

# Install coverage dependencies (macOS)
make install-deps

# Show coverage summary (requires lcov)
make coverage-summary
```

### Using Bazel Directly
```bash
# Run coverage with Bazel directly
bazel coverage //tests:all

# Run with specific instrumentation filter
bazel coverage //tests:all --instrumentation_filter="//src/..."
```

## Configuration

### Bazel Configuration (.bazelrc)
The coverage functionality is configured in `.bazelrc` with these settings:

```bash
# Coverage configuration
coverage --combined_report=lcov
coverage --instrumentation_filter="//src/..."
coverage --coverage_report_generator=@bazel_tools//tools/test:coverage_report_generator
coverage --collect_code_coverage

# Coverage build flags
coverage --copt=-g
coverage --copt=-O0
coverage --copt=--coverage
coverage --linkopt=--coverage
```

### Instrumentation Filter
The `--instrumentation_filter="//src/..."` setting ensures that only source files in the `src/` directory are instrumented for coverage measurement, excluding test files and external dependencies.

## Output Files

### Coverage Data Location
- **LCOV File**: `coverage_reports/coverage.lcov` - Raw coverage data in LCOV format
- **HTML Report**: `coverage_reports/html/index.html` - Interactive HTML coverage report (if `genhtml` is installed)

### Bazel Output
Bazel generates coverage data at:
- `bazel-out/_coverage/_coverage_report.dat` - Primary coverage report
- Test logs with coverage information in `bazel-out/*/testlogs/`

## HTML Reports

### Installing Dependencies
To generate HTML coverage reports, install `lcov`:

```bash
# macOS with Homebrew
brew install lcov

# Ubuntu/Debian
sudo apt-get install lcov

# CentOS/RHEL/Fedora
sudo yum install lcov
```

### Viewing HTML Reports
After running coverage with `lcov` installed:
1. Open `coverage_reports/html/index.html` in your browser
2. On macOS, the script will automatically open the report
3. Navigate through files to see line-by-line coverage details

## Understanding Coverage Metrics

### Line Coverage
- **Lines Hit (LH)**: Number of executable lines that were executed
- **Lines Found (LF)**: Total number of executable lines
- **Line Coverage %**: (LH / LF) * 100

### Function Coverage
- **Functions Hit (FNH)**: Number of functions that were called
- **Functions Found (FNF)**: Total number of functions
- **Function Coverage %**: (FNH / FNF) * 100

### Branch Coverage
- **Branches Hit (BRH)**: Number of conditional branches taken
- **Branches Found (BRF)**: Total number of conditional branches
- **Branch Coverage %**: (BRH / BRF) * 100

## Coverage Targets by Component

### RISC-V Target (`src/targets/`)
```bash
# Test RISC-V instruction selection coverage
./scripts/coverage.sh //tests:riscv_target_test
./scripts/coverage.sh //tests:simple_instruction_selection_test
./scripts/coverage.sh //tests:control_flow_test
```

### IR Builder (`src/ir_builder.cc`)
```bash
# Test IR generation coverage  
./scripts/coverage.sh //tests:ir_builder_test
./scripts/coverage.sh //tests:ir_test
```

### Machine Code Generation (`src/machine.cc`, `src/codegen/`)
```bash
# Test machine code generation coverage
./scripts/coverage.sh //tests:machine_test
./scripts/coverage.sh //tests:assembly_emitter_test
```

### Register Allocation (`src/reg_alloc/`)
```bash
# Test register allocation coverage
./scripts/coverage.sh //tests:lra_test
./scripts/coverage.sh //tests:lsra_test
```

## Continuous Integration

### Adding Coverage to CI
To add coverage measurement to CI pipelines:

```yaml
# GitHub Actions example
- name: Run Tests with Coverage
  run: |
    bazel coverage //tests:all
    ./scripts/coverage.sh //tests:all

- name: Upload Coverage Reports
  uses: codecov/codecov-action@v3
  with:
    files: ./coverage_reports/coverage.lcov
```

### Coverage Thresholds
Consider setting minimum coverage thresholds:
- **Line Coverage**: 80% minimum
- **Function Coverage**: 90% minimum
- **Critical Components**: 95% minimum (RISC-V instruction selection, IR generation)

## Known Issues and Limitations

### Current Status: Partial Functionality ⚠️

The coverage measurement system is configured but has compatibility issues:

- ✅ **Infrastructure Setup**: All scripts and configuration are in place
- ✅ **Test Execution**: All tests pass successfully  
- ⚠️ **Coverage Data**: Limited coverage data due to gcov compatibility issues
- ❌ **Full Coverage Reports**: Cannot generate comprehensive HTML reports yet

### Core Issue: gcov Tool Compatibility

The main problem is compatibility between Bazel's coverage system and modern gcov implementations:

1. **Apple LLVM gcov** (`/usr/bin/gcov`): Doesn't support Bazel's `-output` option
2. **Homebrew llvm-cov**: Different argument format, incompatible with Bazel expectations  
3. **GNU gcov**: Requires GCC compiler, but conflicts with Clang coverage flags

**Error Message Pattern:**

```bash
gcov: Unknown command line argument '-output'
com.google.devtools.coverageoutputgenerator.Main: There was no coverage found
```

### Attempted Solutions

We tested several approaches:

1. **llvm-cov wrapper**: Created `/scripts/llvm-gcov` wrapper script
2. **Multiple gcov versions**: Tried Apple LLVM, Homebrew LLVM, and GNU gcov
3. **Compiler combinations**: Mixed Clang with different gcov versions

All approaches faced the same fundamental issue: Bazel's coverage tooling expects specific gcov argument patterns that modern implementations don't support.

## Troubleshooting

### No Coverage Data Generated

If you see "There was no coverage found":

1. **Known Issue**: This is the current limitation described above
2. Ensure the instrumentation filter matches your source files
3. Check that tests actually execute the source code  
4. Verify build flags include `--coverage`

### Empty Coverage Reports

This can happen when:

- Tests don't exercise the instrumented code (check test dependencies)
- Instrumentation filter is too restrictive
- gcov compatibility issues (current main problem)
- Build optimization removes coverage instrumentation

### macOS Specific Issues

- Ensure Xcode command line tools are installed: `xcode-select --install`
- Use `brew install lcov` for HTML report generation
- **Main Issue**: macOS LLVM tools use different argument formats than expected
- Coverage may require different toolchain configuration

## Examples

### Basic Coverage Workflow

# Generate coverage report for all tests
make coverage

# Generate coverage for specific test
./scripts/coverage.sh //tests:control_flow_test

# View the report for specific files
genhtml coverage_reports/coverage.lcov \
  --output-directory coverage_reports/html \
  --include "*riscv*" \
  --title "RISC-V Component Coverage"
```

### Focused Component Testing

# Test only RISC-V instruction selector coverage
./scripts/coverage.sh //tests:simple_instruction_selection_test

# View the report for specific files
genhtml coverage_reports/coverage.lcov \
  --output-directory coverage_reports/html \
  --include "*riscv*" \
  --title "RISC-V Component Coverage"
```

### Coverage-Driven Development

1. Write a failing test
2. Run coverage to see uncovered lines: `./scripts/coverage.sh //tests:your_test`
3. Implement code to make test pass
4. Verify coverage improvement
5. Repeat until desired coverage is achieved

## Integration with IDEs

### VS Code

Install the Coverage Gutters extension and point it to `coverage_reports/coverage.lcov` for inline coverage visualization.

### CLion

CLion can import LCOV files for coverage visualization. Import `coverage_reports/coverage.lcov` after running tests.

## Performance Considerations

Coverage builds are slower due to instrumentation overhead:

- **Build Time**: ~2-3x slower than normal builds
- **Test Runtime**: ~10-20% slower than normal test runs
- **Binary Size**: Larger due to instrumentation code

For development, consider:

- Running coverage on specific components rather than all tests
- Using coverage in CI/nightly builds rather than every commit
- Focusing coverage on critical code paths first
