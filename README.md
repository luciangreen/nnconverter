# Prolog Algorithm → Mathematical Formula Neuronet Converter

## Overview

This system transforms Prolog algorithms into mathematical formulas and neuronet-like inductive representations. The key breakthrough capability is **generating mathematical formulas for recursive algorithms** and proving their correctness using mathematical induction.

## Key Capabilities

🔥 **NEW: Mathematical Formula Generation**
- Converts recursive algorithms into explicit mathematical formulas
- Proves formulas using mathematical induction (base case + inductive step)
- Verifies formulas against actual algorithm execution
- **Example**: `sum_list([1,2,...,n])` → Formula: `n(n+1)/2`

The converter implements seven core modules:

1. **Complexity Finder** (`complexity_finder/2`) - Analyzes computational complexity (O(1), O(n), O(n²), etc.)
2. **Type Finder** (`type_finder/2`) - Infers data types (list, atom, string, number, compound term)
3. **Inductive Transformation Engine** (`inductive_transform/2`) - Converts recursive predicates to base + inductive step form
4. **Pattern Unfolding Module** (`pattern_unfold/2`) - **COMPLETED**: Expands predicate calls to inline definitions, showing explicit call relationships
5. **Grammar Generator** (`grammar_generate/2`) - Creates CFG-like grammars for lists, atoms, and strings
6. **Mathematical Formula Generator** (`formula_generate/2`) - **COMPLETED**: Extracts mathematical patterns and formulas
7. **Inductive Proof System** (`inductive_proof/3`) - **COMPLETED**: Generates mathematical induction proofs
8. **Inductive Insertion Module** (`inductive_insert/3`) - **COMPLETED**: Optimizes code by replacing patterns with inductive equivalents

## Installation

Requires SWI-Prolog 8.4+ (tested with SWI-Prolog 9.0.4).

```bash
# Install SWI-Prolog (Ubuntu/Debian)
sudo apt install swi-prolog

# Or on macOS with Homebrew
brew install swi-prolog
```

## Usage

### Basic Usage

```prolog
% Load the module
?- use_module(neuronet_converter).

% Define an algorithm as list of clauses
Algorithm = [
    (sum_list([], 0) :- true),
    (sum_list([H|T], S) :- sum_list(T, S1), S is H + S1)
].

% Convert to neuronet representation with mathematical formulas
?- convert_algorithm(Algorithm, Neuronet).
```

### Mathematical Formula Generation

```prolog
% Generate mathematical formulas from algorithms
?- formula_generate(InductiveForm, Formulas).

% Example output:
% combined_formula(sum_list, sum(empty_list)=0, sum(list_n)=sum(list_n_minus_1)+head_element)

% Prove formulas using mathematical induction
?- inductive_proof(Formula, BaseProof, StepProof).

% Verify formulas against test cases
?- verify_formula(Formula, [[1,2,3], [1,2,3,4,5]], VerificationResult).
```

### Running Examples

```bash
# Run comprehensive demonstration (recommended)
swipl -q -t "demo, halt" comprehensive_demo.pl

# Run mathematical formula tests
swipl -q -t "test_sum_formula, halt" test_sum_formula.pl
swipl -q -t "test_factorial_formula, halt" test_factorial_formula.pl

# Run enhanced feature tests (NEW)
swipl -q -t "test_additional_algorithms, halt" test_additional_algorithms.pl
swipl -q -t "test_enhanced_features, halt" test_enhanced_features.pl
swipl -q -t "demo_workflow, halt" test_enhanced_features.pl

# Run original neuronet examples
swipl -q -t "test_neuronet, halt" test_neuronet.pl
swipl -q -t "example_conversion, halt" neuronet_example.pl
```

### Example Output

```
=== Testing Sum of First N Natural Numbers Formula ===

Generated Mathematical Formula:
  Base Case: sum(empty_list)=0
  Inductive Step: sum(list_n)=sum(list_n_minus_1)+head_element

Testing Formula: 1+2+...+n = n(n+1)/2
✓ n=1: sum=1, formula=1 (MATCH)
✓ n=2: sum=3, formula=3 (MATCH)  
✓ n=3: sum=6, formula=6 (MATCH)
✓ n=4: sum=10, formula=10 (MATCH)
✓ n=5: sum=15, formula=15 (MATCH)

=== Inductive Proof ===
Base Case Proof: Sum of empty list equals 0
Inductive Step Proof: If sum(T) holds for list T, then sum([H|T]) = sum(T) + H

=== Formula Verification ===
Verification Result: verification_result(all_tests_passed,5,5)
```

## Files

**Core System:**
- `neuronet_converter.pl` - Main converter module with all 7 components including mathematical formula generation
- `comprehensive_demo.pl` - **NEW**: Complete demonstration of mathematical formula capabilities

**Examples and Tests:**
- `test_sum_formula.pl` - Test sum of first n natural numbers formula (1+2+...+n = n(n+1)/2)
- `test_factorial_formula.pl` - Test factorial formula generation and proof
- `test_additional_algorithms.pl` - **NEW**: Test list length, append, reverse, and fibonacci algorithms
- `test_enhanced_features.pl` - **NEW**: Test pattern unfolding and inductive insertion capabilities
- `neuronet_example.pl` - Worked examples with sum_list/2 and reverse/2  
- `neuronet_demo.pl` - Multiple algorithm demonstrations
- `test_neuronet.pl` - Basic functionality tests

**Documentation:**
- `NEURONET_CONVERTER_README.md` - This comprehensive guide

## API Reference

### Main Conversion

```prolog
convert_algorithm(+Algorithm, -Neuronet)
```
Converts a list of Prolog clauses into a structured neuronet representation with mathematical formulas.

**Parameters:**
- `Algorithm`: List of Prolog clauses `[(Head :- Body), ...]`
- `Neuronet`: Dictionary containing all transformation stages including `formulas`

### Mathematical Formula Components

```prolog
formula_generate(+InductiveForm, -Formulas)
inductive_proof(+Formula, -BaseProof, -StepProof)  
verify_formula(+Formula, +TestInputs, -VerificationResult)
```

### Individual Components

```prolog
complexity_finder(+Algorithm, -ComplexityInfo)
type_finder(+Algorithm, -TypeInfo)
inductive_transform(+Algorithm, -InductiveForm)
pattern_unfold(+InductiveForm, -UnfoldedForm)
grammar_generate(+UnfoldedForm, -Grammar)
inductive_insert(+UnfoldedForm, +Grammar, -OptimisedForm)
```

## Example Algorithms

The system has been tested with and generates formulas for:

1. **Sum List** - O(n) list summation → **Formula**: `sum(1+2+...+n) = n(n+1)/2`
2. **Factorial** - O(n) recursive factorial → **Formula**: `factorial(n) = n * factorial(n-1)`
3. **List Length** - O(n) list length calculation
5. **List Append** - O(n) list concatenation → **Formula**: `append([], L, L)` and `append([H|T], L, [H|R]) :- append(T, L, R)`
6. **List Reverse** - O(n²) naive list reversal → **Formula**: Complex recursive pattern with append operations
7. **Fibonacci** - O(2^n) exponential recursive sequence → **Formula**: `fib(n) = fib(n-1) + fib(n-2)`
8. **Power Function** - O(n) exponentiation → **Formula**: `power(x, n) = x * power(x, n-1)`

## Mathematical Induction Proofs

The system generates complete inductive proofs:

**Example: Sum of First N Natural Numbers**
- **Base Case**: `sum([]) = 0` ✓
- **Inductive Hypothesis**: Assume `sum([1,2,...,k]) = k(k+1)/2`
- **Inductive Step**: Prove `sum([1,2,...,k,k+1]) = (k+1)(k+2)/2`
  - `sum([1,2,...,k,k+1]) = sum([1,2,...,k]) + (k+1)`
  - `= k(k+1)/2 + (k+1)` [by hypothesis]
  - `= (k+1)(k/2 + 1) = (k+1)(k+2)/2` ✓
- **Conclusion**: Formula holds for all n ≥ 1

## Workflow

1. **Input**: Prolog algorithm as list of clauses
2. **Complexity Analysis**: Detect recursion patterns and classify complexity
3. **Type Inference**: Infer argument types for each predicate
4. **Inductive Transformation**: Convert to base case + inductive step form
5. **Pattern Unfolding**: Expand all predicate calls inline with cross-predicate relationships
6. **Grammar Generation**: Create CFG-like grammar rules
7. **Mathematical Formula Generation**: Extract mathematical patterns and formulas
8. **Inductive Insertion**: Optimize code by replacing patterns with inductive equivalents
9. **Inductive Proof Generation**: Create mathematical induction proofs
10. **Formula Verification**: Test formulas against algorithm execution
11. **Output**: Structured neuronet representation with proven mathematical formulas

## Applications

- **Mathematical Education**: Teaching recursion and mathematical induction
- **Algorithm Analysis**: Understanding algorithmic complexity and structure  
- **Formula Discovery**: Automatically discovering mathematical formulas from code
- **Proof Automation**: Generating mathematical proofs for recursive algorithms
- **AI Education**: Teaching inductive reasoning and mathematical thinking
- **Neuronet Design**: Creating logic-based neural network architectures

## Compatibility

- **Language**: Prolog (SWI-Prolog 8.4+)
- **Platform**: Cross-platform (Linux, macOS, Windows)
- **Dependencies**: None (pure Prolog implementation)

## License

This project is part of the Philosophy repository by luciangreen.