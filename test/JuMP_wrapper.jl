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

    con_ref = @constraint(model, [i in keys], x + d1[i] >= d2[i], container = ParametrizedArray)
    con = constraint_object(con_ref.constraint)
    @test con isa IteratedConstraint
    con_it_expr = jump_function(con)
    @test con_it_expr isa IteratedExpr
    con_expr = con_it_expr.expr
    @test sprint(show, con_ref) == "ParametrizedArray(((x + (IteratorIndex(1, 1))) - (IteratorIndex(2, 2))) - 0.0, Iterator(2, [-1.0, 1.0, 3.141592653589793, 0.0]), Iterator(2, [-1.0, 1.0, 3.141592653589793, 0.0]) ∈ MathOptInterface.Nonnegatives(4), (iterator([:a, :b]),))"
    @test sprint(show, MIME"text/latex"(), con_ref) == "ParametrizedArray(((x + (IteratorIndex(1, 1))) - (IteratorIndex(2, 2))) - 0.0, Iterator(2, [-1.0, 1.0, 3.141592653589793, 0.0]), Iterator(2, [-1.0, 1.0, 3.141592653589793, 0.0]) ∈ MathOptInterface.Nonnegatives(4), (iterator([:a, :b]),))"
    @test sprint(show, con_expr) == "((x + (IteratorIndex(1, 1))) - (IteratorIndex(2, 2))) - 0.0"
    @test sprint(show, MIME"text/latex"(), con_expr) == "\$ {\\left({\\left({x} + {\\left(IteratorIndex(1, 1)\\right)}\\right)} - {\\left(IteratorIndex(2, 2)\\right)}\\right)} - {0.0} \$"

    i = GenOpt.iterator(keys)
    expr = x + d1[i] - d2[i]
    # TODO There are still `IteratorIndex`, JuMP function does not
    # convert them back
    @test_broken JuMP.isequal_canonical(con_expr, expr)
end

end  # module

TestJuMP.runtests()
