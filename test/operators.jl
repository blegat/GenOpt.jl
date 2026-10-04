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

_eq(a, b) = a == b
function _eq(a::JuMP.AbstractJuMPScalar, b::JuMP.AbstractJuMPScalar)
    return JuMP.isequal_canonical(a, b)
end

function _test_template(et, values)
    @test et isa ExprTemplate
    for i in eachindex(values)
        @test _eq(
            index_iterators(et.expr, (et.iterators[1].values[i],)),
            values[i],
        )
    end
end

function test_variable_vect()
    model = JuMP.Model()
    JuMP.@variable(model, x[1:3])
    i = GenOpt.iterator([3, 1])
    _test_template(x[i], GenOpt._getindex_expr.(Ref(x), [3, 1]))
    return
end

# Indexing a `DenseAxisArray` of variables by an iterator, the way the OPF example builds its
# model: `@variable(model, va[keys(ref[:bus])])` is indexed by the PowerModels component ids,
# so the axis is a `Dict` lookup rather than `1:n`, and the constraints index it both directly
# (`va[i]`) and through a data `Dict` (`va[f_bus[i]]`).
function test_dense_axis_array_getindex()
    bus = [101, 202, 303]           # component ids, not positions
    model = JuMP.Model()
    JuMP.@variable(model, va[bus])
    i = GenOpt.iterator([303, 101])
    # The keys are looked up in the axis, so the template indexes `va.data` by position.
    _test_template(va[i], GenOpt._getindex_expr.(Ref(va.data), [3, 1]))

    # `f_bus[i]` is itself an iterator value, as in `va[f_bus[i]]`.
    f_bus = Dict(1 => 202, 2 => 101)
    j = GenOpt.iterator([2, 1])
    _test_template(va[f_bus[j]], GenOpt._getindex_expr.(Ref(va.data), [1, 2]))
    return
end

function test_dense_axis_array_one_to_getindex()
    # An axis that already is `1:n`: the key is the position, no `Dict` to go through.
    model = JuMP.Model()
    JuMP.@variable(model, y[1:3], container = JuMP.Containers.DenseAxisArray)
    i = GenOpt.iterator([3, 1])
    _test_template(y[i], GenOpt._getindex_expr.(Ref(y.data), [3, 1]))
    return
end

function test_vect_getindex()
    v = [-1, 1, 4, -2]
    i = GenOpt.iterator([3, 1])
    _test_iterator(v[i], [4, -1])
    _test_iterator(v[i+1], [-2, 1])
    _test_iterator(v[4-i], [-1, 4])
    return
end

function test_dict_getindex()
    d1 = Dict(:a => -1, :b => 1)
    d2 = Dict(:a => π, :b => 0.0)

    i = GenOpt.iterator([:a, :b])

    _test_iterator(d1[i], [-1, 1])
    _test_iterator(d2[i], Real[π, 0.0])

    # A computed index, so the key is the value of the expression. Unlike an `Array`, a
    # `Dict` needs no conversion back to `Int` since `hash(3.0) == hash(3)`.
    d3 = Dict(1 => -1, 2 => 1, 3 => 4)
    j = GenOpt.iterator([2, 1])
    _test_iterator(d3[j+1], [4, 1])
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

function test_template_coefficient_conversion()
    for T in (Float32, Float64)
        model = JuMP.GenericModel{T}()
        JuMP.@variable(model, x[1:3])
        i = GenOpt.iterator([1, 2, 3])
        @test (i+1).expr.args[2] === 1
        @test (x[i]+0).expr.args[2] === zero(T)
        @test (0+x[i]).expr.args[1] === zero(T)
        @test (1*x[i]).expr.args[1] === one(T)
        @test (x[i]*1).expr.args[2] === one(T)
        @test (x[i]/2).expr.args[2] === T(2)
        quadratic = x[i]^2
        V = JuMP.variable_ref_type(model)
        @test quadratic isa GenOpt.ExprTemplate{JuMP.GenericQuadExpr{T,V},V}
        @test quadratic.expr.args[2] === 2
    end
    return
end

function test_data_array_multiple_iterator_indices()
    data = [3.0, 5.0, 7.0, 11.0, 13.0, 17.0]
    i = GenOpt.iterator(0:1)
    j = GenOpt.iterator(0:2)
    expr = data[3*i+j+1]
    @test expr isa ExprTemplate{Float64,JuMP.VariableRef}
    @test length.(expr.iterators) == [2, 3]
    func = JuMP.moi_function(expr.expr)
    for a in 0:1, b in 0:2
        @test GenOpt._expand(func, [(a,), (b,)]) == data[3*a+b+1]
    end
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

function test_independent_iterators()
    @test GenOpt.Iterator{Int}([1, 2]).values == [1, 2]
    i = GenOpt.iterator([1, 2])
    j = GenOpt.iterator([10, 20, 30])
    expr = i + j
    @test expr isa GenOpt.ExprTemplate{Int,JuMP.VariableRef}
    @test length.(expr.iterators) == [2, 3]
    @test GenOpt.index_iterators(expr.expr, ((2,), (30,))) == 32
    reverse_expr = j + i
    combined = expr + reverse_expr
    @test length.(combined.iterators) == [2, 3]
    @test GenOpt.index_iterators(combined.expr, ((2,), (30,))) == 64
    @test length((i + i).iterators) == 1
    same_values = GenOpt.iterator([1, 2])
    @test length((i + same_values).iterators) == 2
    return
end

function test_independent_iterators_mapped_values()
    i = GenOpt.iterator([1, 2])
    j = GenOpt.iterator([10, 20])
    old_expr = i + j
    data = [3, 5]
    mapped = data[i]
    expr = old_expr + mapped
    @test length.(expr.iterators) == [2, 2]
    @test GenOpt.index_iterators(expr.expr, ((2, 5), (20,))) == 27
    second_map = [7, 11][i]
    expr = expr + second_map
    @test length.(expr.iterators) == [2, 2]
    @test GenOpt.index_iterators(expr.expr, ((2, 5, 11), (20,))) == 38
    @test length(first(expr.iterators).values[1]) == 3
    @test length(first(old_expr.iterators).values[1]) == 1
    return
end

function test_divergent_iterator_mappings()
    i = GenOpt.iterator([1, 2])
    j = GenOpt.iterator([1])
    merged = (i + j).iterators
    a = GenOpt.IteratorValues(copy(merged), GenOpt.IteratorIndex(1), 1)
    b = GenOpt.IteratorValues(copy(merged), GenOpt.IteratorIndex(1), 1)
    left = [10, 20][a]
    right = [100, 200][b]
    @test_throws ArgumentError left + right
    later = [1000, 2000][b]
    @test_throws ArgumentError left + later
    return
end

function test_independent_iterators_matrix_index()
    model = JuMP.Model()
    JuMP.@variable(model, x[1:2, 1:3])
    i = GenOpt.iterator([1, 2])
    j = GenOpt.iterator([1, 2, 3])
    expr = x[i, j]
    @test expr isa GenOpt.ExprTemplate{JuMP.VariableRef,JuMP.VariableRef}
    @test length.(expr.iterators) == [2, 3]
    f = JuMP.moi_function(expr.expr)
    @test GenOpt._expand(f, [(2,), (3,)]) == JuMP.index(x[2, 3])
    return
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

end  # module

TestOperators.runtests()
