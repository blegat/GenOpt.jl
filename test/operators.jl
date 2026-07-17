# Copyright (c) 2024: Benoît Legat and contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

# Scope: building expressions (`iterator`, `ExprTemplate`, `lazy_sum`, filtered or not) and
# the JuMP/MOI functions they lower to, without solving. Add any new test about how an
# expression is *built* or *stored* here rather than in a new file.

module TestOperators

using Test
using GenOpt
import JuMP
import MathOptInterface as MOI
import HiGHS

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

function _test_iterator(it, values)
    @test it isa IteratorValues
    @test getindex.(it.iterators[1].values, it.value_index) == values
end

function _test_template(et, values)
    @test et isa ExprTemplate
    for i in eachindex(values)
        @test index_iterators(et.expr, (et.iterators[1].values[i],)) ==
              values[i]
    end
end

function test_getindex()
    d1 = Dict(:a => -1, :b => 1)
    d2 = Dict(:a => π, :b => 0.0)

    i = GenOpt.iterator([:a, :b])

    _test_iterator(d1[i], [-1, 1])
    _test_iterator(d2[i], Real[π, 0.0])
    return
end

function test_filtered_dict()
    # `lazy_sum(... if dict[j] == i)` must support a `Dict` in the filter, not just a
    # `Vector` (used e.g. in the OPF example for `arc_bus`/`gen_bus` maps).
    model = JuMP.Model()
    JuMP.@variable(model, x[1:3])
    grp = Dict(1 => 1, 2 => 1, 3 => 2)
    s = GenOpt.lazy_sum(x[j] for j in 1:3 if grp[j] == 1)
    @test s isa GenOpt.FilteredLazySum
    @test s.expr.head == :getindex
    return
end

function test_filtered_array()
    model = JuMP.Model()
    JuMP.@variable(model, x[1:3])
    group = [1, 1, 2]
    s = GenOpt.lazy_sum(x[j] for j in 1:3 if group[j] == 1)
    @test s isa GenOpt.FilteredLazySum
    @test s.filter.head == :(==)
    return
end

function test_filtered_sum_moi_utilities()
    model = JuMP.Model()
    JuMP.@variable(model, x[1:3])
    group = [1, 1, 2]
    lazy = GenOpt.lazy_sum(x[j] for j in 1:3 if group[j] == 1)
    generator = JuMP.moi_function(lazy)

    copied = copy(generator)
    @test copied isa GenOpt.FilteredSumGenerator
    @test copied.func !== generator.func
    @test copied.filter === generator.filter
    @test MOI.Utilities.is_canonical(copied)
    @test MOI.Utilities.canonicalize!(copied) === copied
    @test MOI.Utilities.map_indices(MOI.Utilities.IndexMap(), copied) === copied
    @test MOI.Utilities.map_indices(identity, copied) === copied
    return
end

function test_jump_function_type()
    model = JuMP.Model()
    F = MOI.ScalarAffineFunction{Float64}
    @test JuMP.jump_function_type(model, GenOpt.SumGenerator{F}) ==
          GenOpt.LazySum{JuMP.AffExpr,JuMP.VariableRef}
    @test JuMP.jump_function_type(model, GenOpt.FilteredSumGenerator{F}) ==
          GenOpt.FilteredLazySum{JuMP.AffExpr,JuMP.VariableRef}
    return
end

function test_lazy_sum_promotion()
    model = JuMP.Model()
    JuMP.@variable(model, x[1:3])
    s = GenOpt.lazy_sum(x[j] for j in 1:3)
    @test GenOpt.MA.promote_operation(+, JuMP.AffExpr, typeof(s)) ==
          JuMP.GenericNonlinearExpr{JuMP.VariableRef}
    return
end

function test_variable_array_expr_index()
    model = JuMP.Model()
    JuMP.@variable(model, x[1:3])
    s = GenOpt.lazy_sum(x[j+1] for j in 0:2)
    @test s isa GenOpt.LazySum
    @test s.expr.head == :getindex
    @test s.expr.args[2] isa JuMP.GenericNonlinearExpr
    return
end

function test_unfiltered_vector_of_variables()
    model = JuMP.Model()
    JuMP.@variable(model, x[1:3])
    s = GenOpt.lazy_sum(x[j] for j in 1:3)
    @test s isa GenOpt.LazySum
    @test s.expr.head == :getindex
    return
end

function test_univariate()
    i = GenOpt.iterator([2, -3])
    _test_template(+i, [2, -3])
    _test_template(-i, [-2, 3])
    return
end

function test_multivariate()
    i, j = GenOpt.iterators(([2, -3], [1, -1]))
    _test_template(i + 1, [3, -2])
    _test_template(2 - i, [0, 5])
    ij = i + j
    @test ij isa GenOpt.ExprTemplate{Int,JuMP.VariableRef}
    model = JuMP.Model()
    JuMP.@variable(model, x)
    ijx = ij + x
    @test ijx isa GenOpt.ExprTemplate{JuMP.AffExpr,JuMP.VariableRef}
    ijxx = ijx * x
    @test ijxx isa GenOpt.ExprTemplate{JuMP.QuadExpr,JuMP.VariableRef}
end

function test_lazy_sum_sum()
    model = JuMP.Model()
    JuMP.@variable(model, x[1:2])
    a = lazy_sum(x[i]^2 for i in 1:2)
    b = lazy_sum(2 * x[i] for i in 1:2)
    @test a isa GenOpt.LazySum{JuMP.QuadExpr,JuMP.VariableRef}
    @test b isa GenOpt.LazySum{JuMP.AffExpr,JuMP.VariableRef}
    JuMP.@objective(model, Min, a + b)
    F = MOI.get(model, MOI.ObjectiveFunctionType())
    @test F == MOI.ScalarNonlinearFunction
    func = MOI.get(model, MOI.ObjectiveFunction{F}())
    @test func.head == :+
    @test func.args[1] isa SumGenerator{MOI.ScalarQuadraticFunction{Float64}}
    @test func.args[2] isa SumGenerator{MOI.ScalarAffineFunction{Float64}}
    return
end

_has_filtered_sum(x) =
    if x isa MOI.ScalarNonlinearFunction
        any(_has_filtered_sum, x.args)
    else
        x isa GenOpt.FilteredSumGenerator
    end

# Integration test for a filtered `lazy_sum` used inside a constraint of a JuMP model.
#
# Why it is useful: "sum over the members of a group" is ubiquitous (flow balance at a node,
# assignment per category, ...). `lazy_sum(x[j] for j in J if group[j] == i)` expresses that
# lazily, and it must survive all the way to the MOI model as a `FilteredSumGenerator` (a
# single filtered-sum object per row) rather than being expanded or dropped.
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
    return
end

function _model()
    inner = HiGHS.Optimizer()
    MOI.set(inner, MOI.Silent(), true)
    optimizer = MOI.Bridges.full_bridge_optimizer(inner, Float64)
    MOI.Bridges.add_bridge(optimizer, GenOpt.FunctionGeneratorBridge{Float64})
    return JuMP.direct_model(optimizer)
end

function test_shifted_index_into_variable_vector()
    rhs = [1.0, 2.0]
    model = _model()
    JuMP.@variable(model, x[1:3] >= 0)
    JuMP.@objective(model, Min, sum(x))
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

end  # module

TestOperators.runtests()
