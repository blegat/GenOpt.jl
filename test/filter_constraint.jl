# Copyright (c) 2024: Benoît Legat and contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

# Integration test for a filtered `lazy_sum` used inside a constraint of a JuMP model.
#
# Why it is useful: "sum over the members of a group" is ubiquitous (flow balance at a node,
# assignment per category, ...). `lazy_sum(x[j] for j in J if group[j] == i)` expresses that
# lazily, and it must survive all the way to the MOI model as a `FilteredSumGenerator` (a
# single filtered-sum object per row) rather than being expanded or dropped. This builds such
# a grouped-balance constraint and checks the stored MOI function keeps the filtered sum.

module TestFilterConstraint

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

_has_filtered_sum(x) =
    if x isa MOI.ScalarNonlinearFunction
        any(_has_filtered_sum, x.args)
    else
        x isa GenOpt.FilteredSumGenerator
    end

function test_grouped_balance_constraint()
    group = Dict(1 => 1, 2 => 1, 3 => 2)   # variable j belongs to group `group[j]`
    demand = [3.0, 5.0]
    model = JuMP.Model()
    JuMP.@variable(model, x[1:3, 1:1])
    JuMP.@constraint(
        model,
        con[i in 1:2],
        demand[i] == GenOpt.lazy_sum(x[j, 1] for j in 1:3 if group[j] == i),
    )
    backend = JuMP.backend(model)
    func = MOI.get(backend, MOI.ConstraintFunction(), JuMP.index(con[1]))
    # The filtered sum is preserved in the stored MOI function.
    @test _has_filtered_sum(func)
end

end

TestFilterConstraint.runtests()
