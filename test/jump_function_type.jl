# Copyright (c) 2024: Benoît Legat and contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

# Integration test that a JuMP model containing a GenOpt `container` constraint can be
# introspected with `list_of_constraint_types` (and hence its constraints reconstructed at
# the JuMP level).
#
# Why it is useful: a JuMP user (and JuMP's own `show`) queries `list_of_constraint_types`,
# which must map the stored MOI `FunctionGenerator` back to its JuMP function type. Without
# `jump_function_type` for `FunctionGenerator`, that query errors on any model built with a
# `container` constraint.

module TestShowModel

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

function test_list_of_constraint_types()
    b = [1.0, 2.0]
    model = JuMP.Model()
    JuMP.@variable(model, x[1:2, 1:1])
    JuMP.@constraint(
        model,
        [i in 1:2],
        x[i, 1] >= b[i],
        container = GenOpt.ParametrizedArray,
    )
    # Goes through `jump_function_type(::FunctionGenerator)`; the container constraint is
    # reported at the JuMP level as an `ExprGenerator`.
    types = JuMP.list_of_constraint_types(model)
    @test any(F <: GenOpt.ExprGenerator for (F, S) in types)
    @test JuMP.jump_function_type(model, GenOpt.FunctionGenerator{MOI.ScalarAffineFunction{Float64}}) <:
          GenOpt.ExprGenerator
end

end

TestShowModel.runtests()
