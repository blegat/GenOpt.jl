# Copyright (c) 2024: Benoît Legat and contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

# Scope: how GenOpt's types print, i.e. everything in `src/print.jl`. Add new
# tests about printing here rather than in a new file.
#
# The point of a `container` constraint is that a whole family is stored as one
# template plus the iterators it ranges over. It has to *print* that way too: one
# row with a `∀` clause, not the expanded family that MOI's generic vector
# printing would produce by calling `scalarize`.

module TestPrint

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

# The string MOI prints for the single generator constraint of `model`.
function _constraint_string(model, mime = MIME("text/plain"))
    backend = JuMP.backend(model)
    F, S = only(
        t for t in MOI.get(backend, MOI.ListOfConstraintTypesPresent()) if
        t[1] <: GenOpt.FunctionGenerator
    )
    ci = only(MOI.get(backend, MOI.ListOfConstraintIndices{F,S}()))
    return MOI.Utilities._to_string(mime, backend, ci)
end

function _generator(model)
    backend = JuMP.backend(model)
    F, S = only(
        t for t in MOI.get(backend, MOI.ListOfConstraintTypesPresent()) if
        t[1] <: GenOpt.FunctionGenerator
    )
    ci = only(MOI.get(backend, MOI.ListOfConstraintIndices{F,S}()))
    return MOI.get(backend, MOI.ConstraintFunction(), ci)
end

function test_group_constraint_string()
    b = [1.0, 2.0]
    model = JuMP.Model()
    JuMP.@variable(model, x[1:2, 1:1])
    JuMP.@constraint(
        model,
        [i in 1:2],
        x[i, 1] >= b[i],
        container = GenOpt.ParametrizedArray,
    )
    # The variable array keeps its name, the iterator prints as `i`, the data
    # folded into the iterator prints as its column, and the `- 0` left over
    # from building the template is dropped.
    @test _constraint_string(model) == "x[i,1] - p₂[i] >= 0.0  ∀ i ∈ {1, 2}"
    return
end

function test_group_constraint_is_not_expanded()
    b = [1.0, 2.0]
    model = JuMP.Model()
    JuMP.@variable(model, x[1:2, 1:1])
    JuMP.@constraint(
        model,
        [i in 1:2],
        x[i, 1] >= b[i],
        container = GenOpt.ParametrizedArray,
    )
    str = _constraint_string(model)
    # One row, and none of the expanded members of the family.
    @test count(isequal('\n'), str) == 0
    @test !occursin("x[1,1]", str)
    @test !occursin("x[2,1]", str)
    return
end

function test_show_function_generator()
    b = [1.0, 2.0]
    model = JuMP.Model()
    JuMP.@variable(model, x[1:2, 1:1])
    JuMP.@constraint(
        model,
        [i in 1:2],
        x[i, 1] >= b[i],
        container = GenOpt.ParametrizedArray,
    )
    # Showing the function on its own goes through a model without names, so
    # the variable array is anonymous, but the family is still not expanded.
    @test sprint(show, _generator(model)) == "X[i,1] - p₂[i]  ∀ i ∈ {1, 2}"
    return
end

function test_two_iterators_get_distinct_names()
    b = [1.0, 2.0]
    c = [4.0, 5.0, 6.0]
    model = JuMP.Model()
    JuMP.@variable(model, x[1:2, 1:3])
    JuMP.@constraint(
        model,
        [i in 1:2, j in 1:3],
        b[i] * x[i, j] <= c[j],
        container = GenOpt.ParametrizedArray,
    )
    @test _constraint_string(model) ==
          "p₂[i]*x[i,j] - p₂[j] <= 0.0  ∀ i ∈ {1, 2}, j ∈ {1, 2, 3}"
    return
end

function test_precedence_keeps_parentheses()
    b = [1.0, 2.0]
    model = JuMP.Model()
    JuMP.@variable(model, x[1:2, 1:1])
    JuMP.@constraint(
        model,
        [i in 1:2],
        (b[i] - x[i, 1]) * b[i] <= 0,
        container = GenOpt.ParametrizedArray,
    )
    # The sum binds less tightly than the product, so it keeps its parentheses,
    # and the second lookup through `i` is a second column.
    @test _constraint_string(model) ==
          "(p₂[i] - x[i,1])*p₃[i] <= 0.0  ∀ i ∈ {1, 2}"
    return
end

function test_equality_prints_elementwise_set()
    b = [1.0, 2.0]
    model = JuMP.Model()
    JuMP.@variable(model, x[1:2, 1:1])
    JuMP.@constraint(
        model,
        [i in 1:2],
        x[i, 1] == b[i],
        container = GenOpt.ParametrizedArray,
    )
    # `Zeros` applies to every element, so it prints as the scalar `== 0.0`
    # the rows share rather than as the vector set.
    @test _constraint_string(model) == "x[i,1] - p₂[i] == 0.0  ∀ i ∈ {1, 2}"
    return
end

function test_long_domain_is_elided()
    model = JuMP.Model()
    JuMP.@variable(model, x[1:20, 1:1])
    JuMP.@constraint(
        model,
        [i in 1:20],
        x[i, 1] >= 0,
        container = GenOpt.ParametrizedArray,
    )
    # A group prints on one line, so a long domain is summarised.
    @test _constraint_string(model) == "x[i,1] >= 0.0  ∀ i ∈ {1, 2, 3, …, 20}"
    return
end

function test_latex()
    b = [1.0, 2.0]
    model = JuMP.Model()
    JuMP.@variable(model, x[1:2, 1:1])
    JuMP.@constraint(
        model,
        [i in 1:2],
        x[i, 1] >= b[i],
        container = GenOpt.ParametrizedArray,
    )
    str = _constraint_string(model, MIME("text/latex"))
    @test occursin("\\ge", str)
    @test occursin("\\forall", str)
    return
end

end  # module

TestPrint.runtests()
