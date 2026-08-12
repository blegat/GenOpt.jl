# Copyright (c) 2024: Benoît Legat and contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

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

end  # module

TestOperators.runtests()
