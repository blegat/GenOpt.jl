# Copyright (c) 2024: Benoît Legat and contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestOperators

using Test
using GenOpt
import JuMP

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
    @test it.iterators[1].length == length(values)
    @test it.values == values
end

function test_getindex()
    d1 = Dict(:a => -1, :b => 1)
    d2 = Dict(:a => π, :b => 0.0)

    i = GenOpt.iterator([:a, :b])

    _test_iterator(d1[i], [-1, 1])
    _test_iterator(d2[i], Real[π, 0.0])
end

function test_univariate()
    i = GenOpt.iterator([2, -3])
    _test_iterator(+i, [2, -3])
    _test_iterator(-i, [-2, 3])
end

function test_multivariate()
    i, j = GenOpt.iterators(([2, -3], [1, -1]))
    _test_iterator(i + 1, [3, -2])
    _test_iterator(2 - i, [0, 5])
    ij = i + j
    @test ij isa GenOpt.ExprTemplate{Int,JuMP.VariableRef}
    model = JuMP.Model()
    JuMP.@variable(model, x)
    ijx = ij + x
    @test ijx isa GenOpt.ExprTemplate{JuMP.AffExpr,JuMP.VariableRef}
    ijxx = ijx * x
    @test ijxx isa GenOpt.ExprTemplate{JuMP.QuadExpr,JuMP.VariableRef}
end

end  # module

TestOperators.runtests()
