# Copyright (c) 2024: Benoît Legat and contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

# Integration test for indexing a vector of JuMP variables at a *computed* position inside a
# GenOpt `container` constraint, e.g. `x[i + 1]`.
#
# Why it is useful: coupling consecutive entries of a variable vector (`x[i+1]` relative to
# element `i`) is the bread-and-butter of time-stepping / recurrence models. With
# `container = GenOpt.ParametrizedArray` the whole family is one MOI `FunctionGenerator`, and
# that requires indexing the variable vector with the expression `i + 1` built from the
# iterator. This builds such a model as an end user would and checks it solves.

module TestArrayVariableIndex

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

function test_shifted_index_into_variable_vector()
    rhs = [1.0, 2.0]
    model = _model()
    JuMP.@variable(model, x[1:3] >= 0)
    JuMP.@objective(model, Min, sum(x))
    # A single `FunctionGenerator` constraint `x[i+1] >= rhs[i]` for `i in 1:2`.
    JuMP.@constraint(
        model,
        [i in 1:2],
        x[i+1] >= rhs[i],
        container = GenOpt.ParametrizedArray,
    )
    JuMP.optimize!(model)
    @test JuMP.termination_status(model) == MOI.OPTIMAL
    # x[1] hits its 0 bound, x[2] >= 1, x[3] >= 2.
    @test JuMP.value.(x) ≈ [0.0, 1.0, 2.0]
    @test JuMP.objective_value(model) ≈ 3.0
end

end

TestArrayVariableIndex.runtests()
