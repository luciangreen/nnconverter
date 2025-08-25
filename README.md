# Prolog Algorithm → Mathematical Formula Neuronet Converter

## Overview

This system transforms Prolog algorithms into mathematical formulas and neuronet-like inductive representations. The key breakthrough capability is **generating mathematical formulas for recursive algorithms** and proving their correctness using mathematical induction.

## 🆕 NEW FEATURES: Polynomial Optimization System

**Enhanced with polynomial coefficient discovery and systematic optimization:**

🔥 **nn_induction_optimisation(B,C)** - Finds polynomial coefficients B,C such that B*n² + C*n fits mathematical patterns (e.g., n(n+1)/2)

🔥 **Extended Polynomial Fitting** - Support for increasing degree polynomials (quadratic, cubic, quartic, etc.)

🔥 **Systematic Coefficient Discovery** - Advanced methods beyond fixed coefficient sets

🔥 **Cognitive Code Generation** - Outputs executable Prolog/Starlog code from mathematical formulas

🔥 **Neuro-Optimized Integration** - Full pipeline integration with CFG generator and mathematical induction

## Key Capabilities

**Mathematical Formula Generation:**
- Converts recursive algorithms into explicit mathematical formulas
- Proves formulas using mathematical induction (base case + inductive step)
- Verifies formulas against actual algorithm execution
- **Example**: `sum_list([1,2,...,n])` → Formula: `n(n+1)/2`

**Polynomial Optimization:**
- `nn_induction_optimisation(B,C)` finds coefficients for quadratic formulas
- Extended support for degree 3, 4, 5+ polynomials
- Systematic coefficient search with mathematical constants
- Integration with existing mathematical induction proofs

The converter implements eight core modules:

1. **Complexity Finder** (`complexity_finder/2`) - Analyzes computational complexity (O(1), O(n), O(n²), etc.)
2. **Type Finder** (`type_finder/2`) - Infers data types (list, atom, string, number, compound term)
3. **Inductive Transformation Engine** (`inductive_transform/2`) - Converts recursive predicates to base + inductive step form
4. **Pattern Unfolding Module** (`pattern_unfold/2`) - Expands predicate calls to inline definitions, showing explicit call relationships
5. **Grammar Generator** (`grammar_generate/2`) - Creates CFG-like grammars for lists, atoms, and strings
6. **Mathematical Formula Generator** (`formula_generate/2`) - Extracts mathematical patterns and formulas
7. **Inductive Proof System** (`inductive_proof/3`) - Generates mathematical induction proofs
8. **Polynomial Optimization System** (`nn_induction_optimisation/2`, `polynomial_fit/3`) - **NEW**: Advanced polynomial coefficient discovery

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

### Enhanced Polynomial Optimization Usage

```prolog
% Use the specific predicate from the problem statement
?- nn_induction_optimisation(B, C).
% Result: B=0.5, C=0.5 (for formula 0.5*n^2 + 0.5*n = n(n+1)/2)

% Extended polynomial fitting for increasing degrees
?- polynomial_fit([[1,1], [2,8], [3,27]], 3, Coefficients).
% Result: Coefficients=[1,0,0,0] (for formula n^3)

% Generate cognitive code from mathematical formulas
?- Formula = combined_formula(sum_list, sum(empty_list)=0, sum(list_n)=sum(list_n_minus_1)+head_element),
   generate_cognitive_code(Formula, CognitiveCode).

% Enhanced conversion with polynomial optimization
?- enhanced_convert_algorithm(Algorithm, EnhancedNeuronet).
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

# NEW: Run enhanced polynomial optimization demo
swipl -q -t "main_demo, halt" final_demo.pl
swipl -q -t "summary, halt" final_demo.pl

# Test new polynomial optimization features
swipl -q -t "test_polynomial_optimization, halt" test_polynomial_optimization.pl
swipl -q -t "test_core_functionality, halt" test_core.pl

# Run mathematical formula tests
swipl -q -t "test_sum_formula, halt" test_sum_formula.pl
swipl -q -t "test_factorial_formula, halt" test_factorial_formula.pl

# Run enhanced feature tests
swipl -q -t "test_additional_algorithms, halt" test_additional_algorithms.pl
swipl -q -t "test_enhanced_features, halt" test_enhanced_features.pl
swipl -q -t "demo_workflow, halt" test_enhanced_features.pl

# Run original neuronet examples  
swipl -q -t "test_neuronet, halt" test_neuronet.pl
swipl -q -t "example_conversion, halt" neuronet_example.pl
```

### Example Output

```
=== Enhanced nnconverter with Polynomial Optimization ===

Testing nn_induction_optimisation/2:
✓ Found coefficients: B=0.5, C=0.5
Verification:
  ✓ n=1: 1.0 (expected 1)
  ✓ n=2: 3.0 (expected 3)
  ✓ n=3: 6.0 (expected 6)
  ✓ n=4: 10.0 (expected 10)
  ✓ n=5: 15.0 (expected 15)

Testing Extended Polynomial Fitting:
  Quadratic (n^2): ✓ Found coefficients [1,0,0]
  Cubic (n^3): ✓ Found coefficients [1,0,0,0]
  
Generated Cognitive Code:
  cognitive_predicate(sum_list,[
    clause(sum_list,[[],0],true),
    clause(sum_list,[[H|T],S],
           (recursive_call(sum_list,[T,S1]),arithmetic(S,H+S1)))
  ])

=== Mathematical Induction Integration ===
Base Case: sum(empty_list)=0
Inductive Step: sum(list_n)=sum(list_n_minus_1)+head_element
Polynomial Formula: 0.5*n^2 + 0.5*n = n(n+1)/2
```

## Files

**Core System:**
- `neuronet_converter.pl` - Main converter module with all 8 components including polynomial optimization
- `comprehensive_demo.pl` - Complete demonstration of mathematical formula capabilities
- `final_demo.pl` - **NEW**: Enhanced demonstration with polynomial optimization
- `enhanced_demo.pl` - **NEW**: Complete integration example

**Enhanced Tests:**
- `test_polynomial_optimization.pl` - **NEW**: Comprehensive polynomial optimization tests
- `test_core.pl` - **NEW**: Core nn_induction_optimisation functionality tests
- `test_sum_formula.pl` - Test sum of first n natural numbers formula (1+2+...+n = n(n+1)/2)
- `test_factorial_formula.pl` - Test factorial formula generation and proof
- `test_additional_algorithms.pl` - Test list length, append, reverse, and fibonacci algorithms
- `test_enhanced_features.pl` - Test pattern unfolding and inductive insertion capabilities
- `neuronet_example.pl` - Worked examples with sum_list/2 and reverse/2  
- `neuronet_demo.pl` - Multiple algorithm demonstrations
- `test_neuronet.pl` - Basic functionality tests

**Documentation:**
- `NEURONET_CONVERTER_README.md` - This comprehensive guide

## API Reference

### Main Conversion

```prolog
convert_algorithm(+Algorithm, -Neuronet)
enhanced_convert_algorithm(+Algorithm, -EnhancedNeuronet)
```
Converts a list of Prolog clauses into a structured neuronet representation with mathematical formulas and polynomial optimization.

### Polynomial Optimization Components

```prolog
nn_induction_optimisation(+B, +C)
polynomial_fit(+DataPoints, +Degree, -Coefficients)  
find_polynomial_coefficients(+DataPoints, +Degree, -Coefficients, -Error)
generate_coefficient_candidates(-Candidates)
evaluate_polynomial(+X, +Coefficients, -Result)
```

### Mathematical Formula & Cognitive Code

```prolog
formula_generate(+InductiveForm, -Formulas)
inductive_proof(+Formula, -BaseProof, -StepProof)  
verify_formula(+Formula, +TestInputs, -VerificationResult)
generate_cognitive_code(+Formula, -CognitiveCode)
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