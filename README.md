# GenOpt.jl

[![Build Status](https://github.com/blegat/GenOpt.jl/actions/workflows/ci.yml/badge.svg?branch=main)](https://github.com/blegat/GenOpt.jl/actions?query=workflow%3ACI)
[![codecov](https://codecov.io/gh/blegat/GenOpt.jl/branch/main/graph/badge.svg)](https://codecov.io/gh/blegat/GenOpt.jl)

Pack groups of similar constraints of sum of similar terms into lightweight generators.
The ExaModels extension passes these generators to
[ExaModels](https://github.com/exanauts/ExaModels.jl) to accelerate automatic
differentiation on CPU or GPU.
See the [presentation at JuMP-dev 2025](https://jump.dev/meetings/jumpdev2025/).

## License

`GenOpt.jl` is licensed under the [MIT license](https://github.com/blegat/GenOpt.jl/blob/main/LICENSE.md).

## Installation

Install GenOpt as follows:
```julia
import Pkg
Pkg.add("GenOpt")
```

## Use with JuMP

To create a group of constraints, give `ParametrizedArray` as `container` keyword:
```julia
using GenOpt
@constraint(
    model,
    [i in 1:n],
    x[1, i] == x0[i],
    container = ParametrizedArray,
)
```
To create a grouped sum, use `lazy_sum` as follows:
```julia
@objective(
    model,
    Min,
    lazy_sum(0.5 * R[j] * (u[i, j]^2) for i in 1:N, j in 1:p),
)
```
## Solve polynomial models with HiGHS

Register the generator and scalar polynomial bridges to use affine and
quadratic lazy sums with HiGHS:

```julia
using JuMP, GenOpt
import HiGHS

model = Model(HiGHS.Optimizer)
add_bridge(model, GenOpt.FunctionGeneratorBridge)
add_bridge(model, GenOpt.PolynomialFunctionBridge)
@variable(model, x[1:4])
@constraint(model, [i in 1:4], x[i] >= 0, container = ParametrizedArray)
@constraint(model, lazy_sum(x[i] + 1 for i in 1:4) <= 12)
@objective(model, Min, lazy_sum((x[i] - i)^2 for i in 1:4))
optimize!(model)
```

GenOpt tracks known polynomial expressions using `PolynomialFunction{F}`, where
`F` is the affine or quadratic output type. The shared builders accumulate lazy
sum terms directly into that type when the optimizer needs it. HiGHS supports
quadratic objectives, but not quadratic constraints. Ordinary, untagged
`MOI.ScalarNonlinearFunction` inputs are not handled by these polynomial bridges.

Polynomial lowering currently requires filters to refer only to the iterators
bound by their sum. A filter depending on an enclosing generator raises an
explicit error.

Affine and quadratic constraint families are prepared once before their rows
are expanded. GenOpt resolves coefficient and index columns, then fills
independent scalar functions using typed loops. Functions are processed in
blocks of 65,536 rows to bound temporary function storage. Forms that cannot
be prepared use the scalar expansion path. Preparation supports common
arithmetic, literal unit and square powers, and dense arrays with up to two
dimensions. Other nonlinear numeric expressions keep MOI's scalar evaluation
semantics.

To compare the two expansion paths on the facility coordinate constraints and
the LQCP PDE constraints from the JuMP paper benchmarks, run:

```sh
julia --startup-file=no --project=test benchmark/generator_expansion.jl
```

The script warms both paths, alternates three samples, and prints construction
time, GC time, and allocated bytes as CSV. It uses the same MOI storage for
both paths and does not solve the models. Typed columns use additional
temporary memory; the output functions and term vectors remain independent
for every row.

On an Intel i7-12700K with Julia 1.13.3 and one thread, the median of three
warmed runs was:

| Constraint family | Rows | Scalar | Prepared | Allocated bytes, scalar → prepared |
| --- | ---: | ---: | ---: | ---: |
| Facility coordinates, n = 100 | 1,020,100 | 6.58 s | 0.208 s | 11.60 GB → 0.53 GB |
| LQCP PDE, n = 2000 | 3,998,000 | 5.62 s | 1.82 s | 4.20 GB → 3.01 GB |

These measurements cover one constraint family at a time. The
[raw samples](benchmark/results/2026-10-31/expansion.csv) include GC time;
[metadata](benchmark/results/2026-10-31/metadata.json) records the environment.
[JuMPy model comparisons](benchmark/results/2026-10-31/complete-models.csv)
use a rebuilt JuliaC library. Timings exclude compilation and warmup;
the metadata records the build and validation setup.

## Solve with ExaModels

Install ExaModels and a solver such as MadNLP:

```julia
import Pkg
Pkg.add(["ExaModels", "MadNLP"])
```

Loading both GenOpt and ExaModels automatically enables the extension. Use
`ExaModels.Optimizer` to solve a JuMP model while preserving its grouped
constraints and sums:

```julia
using JuMP, GenOpt
import ExaModels
import MadNLP

model = Model(() -> ExaModels.Optimizer(MadNLP.madnlp))
@variable(model, x[1:4])
@constraint(model, [i in 1:4], x[i] >= 2, container = ParametrizedArray)
@constraint(model, [i in 1:4], x[i] <= 3, container = ParametrizedArray)
@objective(model, Min, lazy_sum((x[i] - i)^2 for i in 1:4))
optimize!(model)

value.(x) # approximately [2, 2, 3, 3]
objective_value(model) # approximately 2
```

For an NVIDIA GPU, install and load CUDA, then pass its backend to the
optimizer:

```julia
import Pkg
Pkg.add("CUDA")
using CUDA

set_optimizer(model, () -> ExaModels.Optimizer(MadNLP.madnlp, CUDABackend()))
optimize!(model)
```

See the [quadrotor](examples/quadrotor) and [optimal power flow](examples/opf)
examples for larger models.
