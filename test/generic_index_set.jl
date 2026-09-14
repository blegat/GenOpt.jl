# Copyright (c) 2024: Benoît Legat and contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

# Integration test for using a non-`Vector` index set (e.g. `keys(dict)`) with a GenOpt
# `container` constraint.
#
# Why it is useful: models are frequently indexed by the keys of a `Dict` (component ids,
# names, ...) rather than by `1:n`, exactly like PowerModels. `keys(dict)` is iterable but not
# integer-indexable, so the iterator machinery must not assume `axe[1]`. This builds a
# vectorized constraint indexed over `keys(dict)` and checks it solves.

module TestGenericIndexSet

using Test
import JuMP
import GenOpt
import HiGHS
import MathOptInterface as MOI

function runtests()
    for name in names(@__MODULE__; all = true)
        if startswith("$(name)", "test_")
            @testset "$(name)" begin
                getfield(@__MODULE__, name)()
            end
        end
    end
    return
end

function _model()
    inner = HiGHS.Optimizer()
    MOI.set(inner, MOI.RawOptimizerAttribute("output_flag"), false)
    optimizer = MOI.Bridges.full_bridge_optimizer(inner, Float64)
    MOI.Bridges.add_bridge(optimizer, GenOpt.FunctionGeneratorBridge{Float64})
    return JuMP.direct_model(optimizer)
end

function test_constraint_indexed_over_dict_keys()
    demand = Dict(1 => 3.0, 2 => 5.0, 3 => 4.0)
    model = _model()
    JuMP.@variable(model, x[1:3, 1:1])
    JuMP.@objective(model, Min, sum(x))
    # Index the family of constraints by `keys(demand)`, not `1:n`.
    JuMP.@constraint(
        model,
        [i in keys(demand)],
        x[i, 1] >= demand[i],
        container = GenOpt.ParametrizedArray,
    )
    JuMP.optimize!(model)
    @test JuMP.termination_status(model) == MOI.OPTIMAL
    @test JuMP.value(x[1, 1]) ≈ 3.0
    @test JuMP.value(x[2, 1]) ≈ 5.0
    @test JuMP.value(x[3, 1]) ≈ 4.0
end

end

TestGenericIndexSet.runtests()
