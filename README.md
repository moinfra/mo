## The Mo programming language

A opensource easy to learn programming languge

Working in progress...

To build and test:

```bash
# Build all targets
bazel build //...

# Run specific test
bazel test //tests:lexer_test

# Run all tests
bazel test //tests:all

# Run tests with coverage
make coverage
# or
./scripts/coverage.sh
```

## Test Coverage

To generate test coverage reports:

```bash
# Generate coverage report for all tests
make coverage

# Generate coverage for specific test
./scripts/coverage.sh //tests:control_flow_test

# Install lcov for HTML reports (macOS)
brew install lcov
```

Coverage reports are saved to `coverage_reports/` directory. See [docs/coverage.md](docs/coverage.md) for detailed usage.

## Development Tools

Ast printer binary:

```bash
bazel build //src:print_ast
