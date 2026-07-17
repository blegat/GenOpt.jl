# Copyright (c) 2024: Benoît Legat and contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

# Integration test for a double-bounded (interval) constraint `lb[i] <= f(i) <= ub[i]` built
# with a GenOpt `container`.
#
# Why it is useful: a JuMP user routinely writes range constraints with *per-element* bounds
# that depend on the index (e.g. an angle/pressure/temperature window `lb[i] <= f <= ub[i]`).
# With `container = ParametrizedArray` the whole family becomes one MOI object, and the bounds
# must be carried per element. This builds such a constraint and checks the resulting MOI
# function/set is a single `FunctionGenerator` in a `VectorInterval` with the expected bounds.

module TestIntervalConstraint

using Test
import JuMP
import GenOpt
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

function test_per_element_interval_bounds()
    lb = [1.0, 2.0]
    ub = [3.0, 4.0]
    model = JuMP.Model()
    JuMP.@variable(model, x[1:2, 1:1])
    JuMP.@constraint(
        model,
        [i in 1:2],
        lb[i] <= x[i, 1] <= ub[i],
        container = GenOpt.ParametrizedArray,
    )
    backend = JuMP.backend(model)
    types = MOI.get(backend, MOI.ListOfConstraintTypesPresent())
    F, S = only(t for t in types if t[1] <: GenOpt.FunctionGenerator)
    @test S <: GenOpt.VectorInterval
    ci = only(MOI.get(backend, MOI.ListOfConstraintIndices{F,S}()))
    set = MOI.get(backend, MOI.ConstraintSet(), ci)
    # A single vectorized constraint holding both rows, with the per-element bounds.
    @test MOI.dimension(set) == 2
    @test set.lower ≈ lb
    @test set.upper ≈ ub
end

end

TestIntervalConstraint.runtests()
