# Copyright (c) 2024: Benoît Legat and contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

# Integration test for printing the (MOI-level) function of a GenOpt `container` constraint.
#
# Why it is useful: to inspect/debug a model, a user prints its constraints. The function
# stored for a `container` constraint is a `ScalarNonlinearFunction` template containing
# GenOpt's variable-array and `IteratorIndex` placeholders; MOI's printing asks each node for
# a string. Without a `_to_string` for those placeholders, showing such a function errors.

module TestPrintMOI

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

function test_show_container_constraint_function()
    b = [1.0, 2.0]
    model = JuMP.Model()
    JuMP.@variable(model, x[1:2, 1:1])
    JuMP.@constraint(
        model,
        [i in 1:2],
        x[i, 1] >= b[i],
        container = GenOpt.ParametrizedArray,
    )
    backend = JuMP.backend(model)
    F, S = only(
        t for t in MOI.get(backend, MOI.ListOfConstraintTypesPresent()) if
        t[1] <: GenOpt.FunctionGenerator
    )
    ci = only(MOI.get(backend, MOI.ListOfConstraintIndices{F,S}()))
    func = MOI.get(backend, MOI.ConstraintFunction(), ci).func
    str = sprint(show, func)
    @test str isa String
    @test occursin("i_1", str)  # IteratorIndex placeholder rendered
    @test occursin("X", str)    # variable-array placeholder rendered
end

end

TestPrintMOI.runtests()
