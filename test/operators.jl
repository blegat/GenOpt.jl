# Copyright (c) 2024: Benoît Legat and contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

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
