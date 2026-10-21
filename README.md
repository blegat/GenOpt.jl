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
