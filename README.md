# GenOpt.jl

[![Build Status](https://github.com/blegat/GenOpt.jl/actions/workflows/ci.yml/badge.svg?branch=main)](https://github.com/blegat/GenOpt.jl/actions?query=workflow%3ACI)
[![codecov](https://codecov.io/gh/blegat/GenOpt.jl/branch/main/graph/badge.svg)](https://codecov.io/gh/blegat/GenOpt.jl)

Pack groups of similar constraints of sum of similar terms into lightweight generators.
This representation can then be used by [ExaModels](https://github.com/exanauts/ExaModels.jl)
to accelerate the Automatic Differentiation on GPU.
See the [presentation at JuMP-dev 2025](https://jump.dev/meetings/jumpdev2025/).

## License

`GenOpt.jl` is licensed under the [MIT license](https://github.com/blegat/GenOpt.jl/blob/main/LICENSE.md).

## Installation

The package is not registered yet so to install `GenOpt` using `Pkg.add`, do:
```julia
import Pkg
Pkg.add("https://github.com/blegat/GenOpt.jl")
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
To a grouped sum, use `lazy_sum` as follows:
```julia
@objective(
    model,
    Min,
    lazy_sum(0.5 * R[j] * (u[i, j]^2) for i in 1:N, j in 1:p),
)
```
To solve it on GPU with ExaModels, use
```julia
set_optimizer(model, () -> GenOpt.ExaOptimizer(madnlp, CUDABackend()))
optimize!(model)
```
