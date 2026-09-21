# Copyright (c) 2024: Benoît Legat and contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

# Scope: the JuMP-level API (containers, macros, `Model` interaction), without solving.
# Add new tests of the user-facing JuMP layer here rather than in a new file.

module TestJuMP

using Test
using JuMP
using GenOpt
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

# A minimal JuMP extension keeps the model/backend and variable-reference types
# concrete without requiring a development version of JuMP.
struct ExtensionModel{T,B<:MOI.ModelLike} <: JuMP.AbstractModel
    backend::B
end

struct ExtensionVariable{M<:ExtensionModel} <: JuMP.AbstractVariableRef
    model::M
    index::MOI.VariableIndex
end

JuMP.value_type(::Type{<:ExtensionModel{T}}) where {T} = T
JuMP.value_type(::Type{ExtensionVariable{M}}) where {M} = JuMP.value_type(M)
function JuMP.variable_ref_type(::Type{M}) where {M<:ExtensionModel}
    return ExtensionVariable{M}
end
JuMP.owner_model(variable::ExtensionVariable) = variable.model
JuMP.index(variable::ExtensionVariable) = variable.index
JuMP.backend(model::ExtensionModel) = model.backend
function JuMP.jump_function_type(
    model::ExtensionModel,
    ::Type{MOI.VariableIndex},
)
    return JuMP.variable_ref_type(model)
end
function MOI.get(model::ExtensionModel, attr::MOI.AbstractModelAttribute)
    return MOI.get(JuMP.backend(model), attr)
end
function JuMP.constraint_ref_with_index(
    model::ExtensionModel,
    index::MOI.ConstraintIndex{
        <:MOI.AbstractVectorFunction,
        <:MOI.AbstractVectorSet,
    },
)
    return JuMP.ConstraintRef(model, index, JuMP.VectorShape())
end

function test_extension_variable_arrays()
    for T in (Float32, Float64)
        backend = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{T}())
        model = ExtensionModel{T,typeof(backend)}(backend)
        V = JuMP.variable_ref_type(model)
        variables = [V(model, index) for index in MOI.add_variables(backend, 4)]
        array = reshape(variables, 2, 2)
        generated = GenOpt.to_generator(array)
        @test generated isa GenOpt.ArrayOfVariables{T,2,V,typeof(model)}
        @test typeof(GenOpt.ArrayOfVariables(model, 0, size(array))) ===
              typeof(generated)
        other_type = T === Float32 ? Float64 : Float32
        @test_throws ArgumentError GenOpt.ArrayOfVariables{other_type,2}(
            model,
            0,
            size(array),
        )
        @test eltype(generated) === V
        @test generated.model === model
        @test size(generated) == (2, 2)
        @test JuMP.index(generated[2, 1]) == JuMP.index(array[2, 1])
        @test JuMP.owner_model(generated[1, 2]) === model

        moi_array = JuMP.moi_function(generated)
        @test moi_array isa GenOpt.ContiguousArrayOfVariables{2}
        restored = JuMP.jump_function(model, moi_array)
        @test typeof(restored) === typeof(generated)
        @test restored.model === model
        @test [JuMP.index(restored[i, j]) for i in 1:2, j in 1:2] == JuMP.index.(array)

        i = GenOpt.iterator(1:2)
        template = array[i, 1]
        @test template isa GenOpt.ExprTemplate{V,V}
        generator = GenOpt.ExprGenerator(template)
        constraint =
            JuMP.build_constraint(error, generator, MOI.Nonnegatives(2))
        @test constraint isa GenOpt.IteratedConstraint{V,V,MOI.Nonnegatives}
        F = GenOpt.FunctionGenerator{MOI.VariableIndex}
        @test JuMP.moi_function_type(typeof(generator)) === F
        @test JuMP.jump_function_type(model, F) === typeof(generator)
        @test JuMP.jump_function_type(
            model,
            GenOpt.SumGenerator{MOI.VariableIndex},
        ) === GenOpt.LazySum{V,V}
        @test JuMP.jump_function_type(
            model,
            GenOpt.FilteredSumGenerator{MOI.VariableIndex},
        ) === GenOpt.FilteredLazySum{V,V}

        function_ = JuMP.moi_function(generator)
        index = MOI.add_constraint(backend, function_, constraint.set)
        @test JuMP.num_constraints(
            model,
            typeof(generator),
            MOI.Nonnegatives,
        ) == 1
        references =
            JuMP.all_constraints(model, typeof(generator), MOI.Nonnegatives)
        @test length(references) == 1
        @test JuMP.index(only(references)) == index
        @test JuMP.owner_model(only(references)) === model
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

function test_index_iterators_scalar_arguments()
    model = Model()
    @variable(model, x)
    i = GenOpt.iterator([1, 2])
    expr = (x + i)^2
    expanded = GenOpt.index_iterators(expr.expr, ((2,),))
    @test expanded isa GenericNonlinearExpr{VariableRef}
    @test expanded.head == :^
    @test length(expanded.args) == 2
    @test expanded.args[2] == 2
    inner = expanded.args[1]
    @test inner isa GenericNonlinearExpr{VariableRef}
    @test inner.head == :+
    @test length(inner.args) == 2
    @test inner.args[1] === x
    @test inner.args[2] == 2
    # Unlike the mixed expression/number arguments above, these reconstructions
    # produce a homogeneous vector of expressions. It is an argument list, not
    # one vector-valued argument to the nonlinear operator.
    for (head, expr, count) in
        ((:sin, sin(x + i), 1), (:*, (x + i) * (x + i), 2))
        expanded = GenOpt.index_iterators(expr.expr, ((2,),))
        @test expanded isa GenericNonlinearExpr{VariableRef}
        @test expanded.head == head
        @test length(expanded.args) == count
        for inner in expanded.args
            @test inner isa GenericNonlinearExpr{VariableRef}
            @test inner.head == :+
            @test inner.args == Any[x, 2]
        end
    end
    return
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

# Interval constraint `lb[i] <= f(i) <= ub[i]` under `container = ParametrizedArray`.
#
# Why it is useful: range constraints whose bounds depend on the index (an angle/pressure/
# temperature window) are routine. The whole family must become a single `FunctionGenerator`
# in one `MOI.HyperRectangle` carrying the per-element bounds, not one row per index.
function test_per_element_interval_bounds()
    lb = [1.0, 2.0]
    ub = [3.0, 4.0]
    model = Model()
    @variable(model, x[1:2, 1:1])
    @constraint(
        model,
        [i in 1:2],
        lb[i] <= x[i, 1] <= ub[i],
        container = ParametrizedArray,
    )
    b = backend(model)
    types = MOI.get(b, MOI.ListOfConstraintTypesPresent())
    F, S = only(t for t in types if t[1] <: GenOpt.FunctionGenerator)
    @test S <: MOI.HyperRectangle
    ci = only(MOI.get(b, MOI.ListOfConstraintIndices{F,S}()))
    set = MOI.get(b, MOI.ConstraintSet(), ci)
    # A single vectorized constraint holding both rows, with the per-element bounds.
    @test MOI.dimension(set) == 2
    @test set.lower ≈ lb
    @test set.upper ≈ ub
    return
end

# Interval constraint with a *constant* bound, e.g. `1 <= f(i) <= ub[i]`: the constant side
# is not an `IteratorValues` so it must be broadcast to every element of the generator.
function test_constant_interval_bounds()
    ub = [3.0, 4.0]
    model = Model()
    @variable(model, x[1:2, 1:1])
    @constraint(
        model,
        [i in 1:2],
        1 <= x[i, 1] <= ub[i],
        container = ParametrizedArray,
    )
    @constraint(
        model,
        [i in 1:2],
        -1 <= x[i, 1] <= 2,
        container = ParametrizedArray,
    )
    b = backend(model)
    types = MOI.get(b, MOI.ListOfConstraintTypesPresent())
    F, S = only(t for t in types if t[1] <: GenOpt.FunctionGenerator)
    @test S <: MOI.HyperRectangle
    sets = map(MOI.get(b, MOI.ListOfConstraintIndices{F,S}())) do ci
        return MOI.get(b, MOI.ConstraintSet(), ci)
    end
    @test length(sets) == 2
    mixed, constant = sets
    # The constant `1` is repeated for each index, the vector bound is kept as is.
    @test mixed.lower ≈ [1.0, 1.0]
    @test mixed.upper ≈ ub
    # Both bounds constant: still a single vectorized constraint of dimension 2.
    @test constant.lower ≈ [-1.0, -1.0]
    @test constant.upper ≈ [2.0, 2.0]
    return
end

# `list_of_constraint_types` on a model holding a GenOpt `container` constraint, and hence
# `show(model)`, which calls it.
#
# Why it is useful: a JuMP user (and JuMP's own `show`) queries `list_of_constraint_types`,
# which must map the stored MOI `FunctionGenerator` back to its JuMP function type. Without
# `jump_function_type` for `FunctionGenerator`, that query errors on any model built with a
# `container` constraint.
function test_list_of_constraint_types()
    b = [1.0, 2.0]
    model = Model()
    @variable(model, x[1:2, 1:1])
    @constraint(
        model,
        [i in 1:2],
        x[i, 1] >= b[i],
        container = ParametrizedArray,
    )
    # Goes through `jump_function_type(::FunctionGenerator)`; the container constraint is
    # reported at the JuMP level as an `ExprGenerator`.
    types = list_of_constraint_types(model)
    @test any(F <: GenOpt.ExprGenerator for (F, S) in types)
    F = GenOpt.FunctionGenerator{MOI.ScalarAffineFunction{Float64}}
    @test jump_function_type(model, F) <: GenOpt.ExprGenerator

    # Round trip: the JuMP type maps back to the MOI type it was built from, and back.
    E = jump_function_type(model, F)
    @test moi_function_type(E) == F
    @test jump_function_type(model, moi_function_type(E)) == E

    # `show(::Model)` counts the constraints of every `(F, S)` of
    # `list_of_constraint_types`, so it needs `num_constraints` for an `ExprGenerator`. The
    # whole family counts as the one vectorized constraint it is.
    _, S = only(t for t in types if t[1] <: GenOpt.ExprGenerator)
    @test num_constraints(model, E, S) == 1
    @test occursin("num_constraints: 1", sprint(show, model))
    # `all_constraints` is the same gap, reached by `print(model)` and by a user asking for
    # the constraints of that type.
    @test length(all_constraints(model, E, S)) == 1
    return
end

end  # module

TestJuMP.runtests()
