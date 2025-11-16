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
          "GenOpt.ParametrizedArray(((x + (Real[π, 0.0][i])) - (Real[π, 0.0][i])) - 0.0, GenOpt.Iterator{Real}(Real[π, 0.0]) ∈ MathOptInterface.Nonnegatives(2), GenOpt.IteratorValues{Vector{Symbol}}[iterator([:a, :b])])"
    @test sprint(show, MIME"text/latex"(), con_ref) ==
          "GenOpt.ParametrizedArray(((x + (Real[π, 0.0][i])) - (Real[π, 0.0][i])) - 0.0, GenOpt.Iterator{Real}(Real[π, 0.0]) ∈ MathOptInterface.Nonnegatives(2), GenOpt.IteratorValues{Vector{Symbol}}[iterator([:a, :b])])"
    @test sprint(show, con_expr) ==
          "((x + (Real[π, 0.0][i])) - (Real[π, 0.0][i])) - 0.0"
    @test sprint(show, MIME"text/latex"(), con_expr) ==
          "\$ {\\left({\\left({x} + {\\left(Real[π, 0.0][i]\\right)}\\right)} - {\\left(Real[π, 0.0][i]\\right)}\\right)} - {0.0} \$"

    i = GenOpt.iterator(keys)
    expr = x + d1[i] - d2[i]
    # TODO There are still `IteratorIndex`, JuMP function does not
    # convert them back
    @test_broken JuMP.isequal_canonical(con_expr, expr)
end

end  # module

TestJuMP.runtests()
