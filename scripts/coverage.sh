#!/bin/bash
#
# Coverage reporting script for Mo compiler
#
# Usage:
#   ./scripts/coverage.sh [target]
#
# Examples:
#   ./scripts/coverage.sh                    # Run coverage for all tests
#   ./scripts/coverage.sh //tests:all        # Run coverage for all tests (explicit)
#   ./scripts/coverage.sh //tests:control_flow_test  # Run coverage for specific test
#

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Default target
TARGET="${1:-//tests:all}"

echo -e "${BLUE}=== Mo Compiler Coverage Report ===${NC}"
echo -e "${YELLOW}Target: ${TARGET}${NC}"
echo

# Create output directory
mkdir -p coverage_reports

echo -e "${BLUE}Running coverage tests...${NC}"
# Run tests with coverage
bazel coverage "${TARGET}" --combined_report=lcov

echo
echo -e "${BLUE}Generating coverage reports...${NC}"

# First, try to generate proper LCOV data from LLVM profdata files
echo -e "${YELLOW}Looking for LLVM profdata files...${NC}"
PROFDATA_FILES=$(find /private/var/tmp/_bazel_* -name "*_cc_coverage.profdata" 2>/dev/null)
if [ -n "$PROFDATA_FILES" ]; then
    echo "Found profdata files, generating LCOV with llvm-cov export..."
    
    # Create combined LCOV file
    > coverage_reports/coverage.lcov
    
    for profdata in $PROFDATA_FILES; do
        # Extract test name from path
        if [[ "$profdata" =~ /([^/]+)_test/test/_cc_coverage\.profdata ]]; then
            TEST_NAME="${BASH_REMATCH[1]}_test"
            BINARY_PATH="bazel-bin/tests/$TEST_NAME"
            
            if [ -x "$BINARY_PATH" ]; then
                echo "  Processing $TEST_NAME..."
                llvm-cov export -format=lcov "$BINARY_PATH" -instr-profile="$profdata" >> coverage_reports/coverage.lcov 2>/dev/null || {
                    echo -e "${YELLOW}    Warning: Failed to process $TEST_NAME${NC}"
                }
            else
                echo -e "${YELLOW}    Warning: Binary not found for $TEST_NAME at $BINARY_PATH${NC}"
            fi
        fi
    done
    
    if [ -s coverage_reports/coverage.lcov ]; then
        echo -e "${GREEN}Successfully generated LCOV data from profdata files${NC}"
        
        # Fix the paths in LCOV file - convert absolute sandbox paths to relative paths
        echo -e "${YELLOW}Fixing file paths in LCOV data...${NC}"
        sed -i '' 's|SF:/private/var/tmp/_bazel_[^/]*/[^/]*/sandbox/[^/]*/[^/]*/execroot/_main/|SF:|g' coverage_reports/coverage.lcov
        
        LCOV_FILE="coverage_reports/coverage.lcov"
    fi
fi

# If we couldn't generate from profdata, fall back to Bazel's output
if [ -z "$LCOV_FILE" ]; then
    # Try multiple approaches to find coverage data
    
    # Method 1: Check bazel-out/_coverage/ directory
    if [ -f "bazel-out/_coverage/_coverage_report.dat" ]; then
        LCOV_FILE="bazel-out/_coverage/_coverage_report.dat"
        echo "Found coverage data at: $LCOV_FILE"
    fi

    # Method 2: Search for coverage files in bazel-out
    if [ -z "$LCOV_FILE" ]; then
        LCOV_FILE=$(find bazel-out -name "*coverage*.dat" -o -name "*.lcov" 2>/dev/null | head -1)
        if [ -n "$LCOV_FILE" ]; then
            echo "Found coverage data at: $LCOV_FILE"
        fi
    fi

    # Method 3: Look in test logs for coverage info
    if [ -z "$LCOV_FILE" ]; then
        echo -e "${YELLOW}Searching for coverage files in test outputs...${NC}"
        COVERAGE_INFO=$(find bazel-out -name "*coverage*" 2>/dev/null | head -5)
        if [ -n "$COVERAGE_INFO" ]; then
            echo "Found coverage-related files:"
            echo "$COVERAGE_INFO"
        fi
    fi
fi

if [ -z "$LCOV_FILE" ]; then
    echo -e "${RED}Error: Could not find LCOV coverage data file${NC}"
    echo -e "${YELLOW}This might be because no source files were instrumented for coverage${NC}"
    echo -e "${YELLOW}Try running: bazel coverage //tests:all --instrumentation_filter=\"//src/...\"${NC}"
    exit 1
fi

# Copy coverage data to our reports directory if needed
if [ "$LCOV_FILE" != "coverage_reports/coverage.lcov" ]; then
    cp "$LCOV_FILE" coverage_reports/coverage.lcov
    echo -e "${GREEN}Coverage data saved to coverage_reports/coverage.lcov${NC}"
else
    echo -e "${GREEN}Coverage data already in coverage_reports/coverage.lcov${NC}"
fi

# Try to generate HTML report if genhtml is available
if command -v genhtml >/dev/null 2>&1; then
    echo -e "${BLUE}Generating HTML coverage report...${NC}"
    genhtml coverage_reports/coverage.lcov \
        --output-directory coverage_reports/html \
        --title "Mo Compiler Coverage Report" \
        --show-details \
        --legend
    echo -e "${GREEN}HTML report generated: coverage_reports/html/index.html${NC}"
    
    # Try to open the report if on macOS
    if [[ "$OSTYPE" == "darwin"* ]]; then
        echo -e "${BLUE}Opening coverage report...${NC}"
        open coverage_reports/html/index.html
    fi
else
    echo -e "${YELLOW}genhtml not found. Install lcov to generate HTML reports:${NC}"
    echo -e "${YELLOW}  brew install lcov${NC}"
fi

# Display summary from LCOV file
echo
echo -e "${BLUE}=== Coverage Summary ===${NC}"

# Display summary from LCOV file
if command -v lcov >/dev/null 2>&1; then
    echo -e "${GREEN}Using lcov for summary:${NC}"
    lcov --summary coverage_reports/coverage.lcov 2>/dev/null | grep -E "(lines|functions|branches)" || {
        echo -e "${YELLOW}lcov summary failed, trying manual parsing...${NC}"
        # Manual LCOV parsing
        awk -F: '
            /^LH:/ { lh += $2 }
            /^LF:/ { lf += $2 }
            /^FNH:/ { fnh += $2 }
            /^FNF:/ { fnf += $2 }
            /^BRH:/ { brh += $2 }
            /^BRF:/ { brf += $2 }
            END {
                if (lf > 0) printf "  lines......: %.1f%% (%d of %d lines)\n", (lh/lf)*100, lh, lf
                if (fnf > 0) printf "  functions..: %.1f%% (%d of %d functions)\n", (fnh/fnf)*100, fnh, fnf  
                if (brf > 0) printf "  branches...: %.1f%% (%d of %d branches)\n", (brh/brf)*100, brh, brf
                if (lf == 0 && fnf == 0) print "  No coverage data found in LCOV file"
            }
        ' coverage_reports/coverage.lcov
    }
else
    echo -e "${YELLOW}Install lcov for detailed coverage summary${NC}"
    echo -e "${YELLOW}  brew install lcov${NC}"
fi

echo
echo -e "${GREEN}Coverage analysis complete!${NC}"
echo -e "Reports saved in: ${BLUE}coverage_reports/${NC}"