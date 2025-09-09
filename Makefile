# Mo Compiler Makefile
# Convenient targets for common development tasks

.PHONY: help test coverage clean build lint format

# Default target
all: build test

# Help target - shows available commands
help:
	@echo "Mo Compiler Build Targets:"
	@echo ""
	@echo "  build     - Build all targets"
	@echo "  test      - Run all tests"
	@echo "  coverage  - Run tests with coverage reporting"
	@echo "  clean     - Clean build artifacts"
	@echo "  lint      - Run linting (if configured)"
	@echo "  format    - Format code (if configured)"
	@echo ""
	@echo "Examples:"
	@echo "  make test"
	@echo "  make coverage"
	@echo "  make coverage TARGET=//tests:control_flow_test"

# Build all targets
build:
	bazel build //...

# Run all tests
test:
	bazel test //tests:all --test_output=errors

# Run tests with coverage reporting
coverage:
	@echo "Running coverage analysis..."
	@./scripts/coverage.sh $(TARGET)

# Clean build artifacts
clean:
	bazel clean
	rm -rf coverage_reports/

# Quick coverage for specific target
coverage-quick:
	bazel coverage //tests:all --combined_report=lcov --instrumentation_filter="//src/..."

# Build and test in one command
check: build test

# Install dependencies (macOS)
install-deps:
	@echo "Installing dependencies on macOS..."
	@if ! command -v brew >/dev/null 2>&1; then \
		echo "Please install Homebrew first: https://brew.sh/"; \
		exit 1; \
	fi
	@echo "Installing lcov for coverage reporting..."
	@brew install lcov || true
	@echo "Dependencies installed!"

# Show coverage summary (requires lcov)
coverage-summary:
	@if [ -f coverage_reports/coverage.lcov ]; then \
		echo "=== Coverage Summary ==="; \
		lcov --summary coverage_reports/coverage.lcov 2>/dev/null | grep -E "(lines|functions|branches)" || true; \
	else \
		echo "No coverage data found. Run 'make coverage' first."; \
	fi