# Copyright (c) 2024: Benoît Legat and contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestJuMP

using Test
using JuMP
using GenOpt

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

function test_container()
    model = Model()
    @variable(model, x)
    @variable(model, y[1:2])

    d1 = Dict(:a => -1, :b => 1)
    d2 = Dict(:a => π, :b => 0.0)

    keys = [:a, :b]

    con_ref = @constraint(
        model,
        [i in keys],
        x + d1[i] >= d2[i],
        container = ParametrizedArray
    )
    con = constraint_object(con_ref.constraint)
    @test con isa IteratedConstraint
    con_it_expr = jump_function(con)
    @test con_it_expr isa ExprGenerator
    con_expr = con_it_expr.expr.expr
    @test sprint(show, con_ref) ==
          "$ParametrizedArray(((x + getindex($IteratorIndex(1), 2)) - getindex($IteratorIndex(1), 3)) - 0, $Iterator{Tuple{Symbol, Int64, Real}}(Tuple{Symbol, Int64, Real}[(:a, -1, π), (:b, 1, 0.0)]) ∈ MathOptInterface.Nonnegatives(2), $IteratorValues[iterator([:a, :b])])"
    @test sprint(show, MIME"text/latex"(), con_ref) ==
          "$ParametrizedArray(((x + getindex($IteratorIndex(1), 2)) - getindex($IteratorIndex(1), 3)) - 0, $Iterator{Tuple{Symbol, Int64, Real}}(Tuple{Symbol, Int64, Real}[(:a, -1, π), (:b, 1, 0.0)]) ∈ MathOptInterface.Nonnegatives(2), $IteratorValues[iterator([:a, :b])])"
    @test sprint(show, con_expr) ==
          "((x + getindex($IteratorIndex(1), 2)) - getindex($IteratorIndex(1), 3)) - 0"
    @test sprint(show, MIME"text/latex"(), con_expr) ==
          "\$ {\\left({\\left({x} + {\\textsf{getindex}\\left({$IteratorIndex(1)}, {2}\\right)}\\right)} - {\\textsf{getindex}\\left({$IteratorIndex(1)}, {3}\\right)}\\right)} - {0} \$"

    i = GenOpt.iterator(keys)
    expr = x + d1[i] - d2[i]
    # TODO There are still `IteratorIndex`, JuMP function does not
    # convert them back
    @test_broken JuMP.isequal_canonical(con_expr, expr)
end

function test_ind2sub()
    # `bridge.jl` expands a generator with `CartesianIndices` so `_ind2sub`
    # must agree with it, otherwise `getindex` and the bridge would disagree
    # on which entry is the `i`th one.
    for size in ([4], [2, 3], [3, 1, 2])
        indices = CartesianIndices(Tuple(size))
        for i in eachindex(IndexLinear(), indices)
            @test GenOpt._ind2sub(size, i) == collect(Tuple(indices[i]))
        end
    end
    return
end

function test_generator_getindex()
    model = Model()
    @variable(model, x)
    con_ref = @constraint(
        model,
        [i in 1:2, j in 1:3],
        x >= 10 * i + j,
        container = ParametrizedArray,
    )
    gen = constraint_object(con_ref.constraint).func
    @test gen isa ExprGenerator
    @test size(gen) == (6,)
    @test length(gen) == 6
    # The first iterator varies fastest, like `CartesianIndices`.
    @test [sprint(show, e) for e in gen] == ["(x - $c) - 0" for c in [11, 21, 12, 22, 13, 23]]
    @test_throws BoundsError gen[7]
    return
end

end  # module

TestJuMP.runtests()
