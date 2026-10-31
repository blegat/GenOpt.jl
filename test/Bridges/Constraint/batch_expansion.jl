# Copyright (c) 2024: Benoît Legat and contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestBatchExpansion

using Test
import GenOpt
import MathOptInterface as MOI

include("../../../benchmark/generator_expansion.jl")

@testset "expansion benchmark families" begin
    for (kind, rows) in ((:coordinate, 48), (:pde, 6)), batch in (false, true)
        model = GeneratorExpansionBenchmark.build(kind, 3, batch)
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.ScalarAffineFunction{Float64},
                MOI.EqualTo{Float64},
            }(),
        ) == rows
    end
end

snf(head, args...) = MOI.ScalarNonlinearFunction(head, Any[args...])

function check_expansion(
    ::Type{F},
    expr,
    iterators;
    fast = true,
) where {T,F<:Union{MOI.ScalarAffineFunction{T},MOI.ScalarQuadraticFunction{T}}}
    generator = GenOpt.FunctionGenerator{F}(expr, GenOpt.Iterator[iterators...])
    actual, reference = MOI.Utilities.Model{T}(), MOI.Utilities.Model{T}()
    MOI.add_variables(actual, 24)
    MOI.add_variables(reference, 24)
    set = MOI.EqualTo(zero(T))
    batch = GenOpt._try_batch_constraints(actual, generator, set)
    @test (batch !== nothing) == fast
    if batch === nothing
        batch = GenOpt.add_constraints(actual, generator, set)
    end
    scalar = GenOpt._add_constraints_scalar(reference, generator, set)
    @test length(batch) == length(scalar)
    for (a, b) in zip(batch, scalar)
        @test MOI.get(actual, MOI.ConstraintFunction(), a) ≈
              MOI.get(reference, MOI.ConstraintFunction(), b)
        @test MOI.constant(MOI.get(actual, MOI.ConstraintSet(), a)) ≈
              MOI.constant(MOI.get(reference, MOI.ConstraintSet(), b))
    end
    return
end

@testset "typed columns and Cartesian row order" begin
    iterators = GenOpt.Iterator[
        GenOpt.Iterator([(1, 0.5), (2, 1.5), (3, 2.5)]),
        GenOpt.Iterator([(1,), (2,), (3,), (4,)]),
    ]
    context = GenOpt._BatchContext(iterators)
    first = GenOpt._batch_column(context, 1, 1)
    second = GenOpt._batch_column(context, 2, 1)
    data = GenOpt._batch_column(context, 1, 2)
    @test eltype(first) == Int
    @test eltype(data) == Float64
    @test collect(first) == repeat([1, 2, 3], 4)
    @test collect(second) == repeat([1, 2, 3, 4]; inner = 3)
    @test collect(data) == repeat([0.5, 1.5, 2.5], 4)
    @test first === GenOpt._batch_column(context, 1, 1)
    @test_throws BoundsError first[0]
    @test_throws BoundsError first[13]
    for values in (
        [(1, 1.0f0), (2, 2.0f0)],
        [(1, big"1.0"), (2, big"2.0")],
        [(1, MOI.VariableIndex(1)), (2, MOI.VariableIndex(2))],
        [(1, :first), (2, :second)],
        [(1, "first"), (2, "second")],
    )
        context = GenOpt._BatchContext(GenOpt.Iterator[GenOpt.Iterator(values)])
        column = GenOpt._batch_column(context, 1, 2)
        @test collect(column) == last.(values)
    end
end

@testset "affine and quadratic templates agree with scalar expansion" begin
    a, b = GenOpt.IteratorIndex(1), GenOpt.IteratorIndex(2)
    i, j = snf(:getindex, a, 1), snf(:getindex, b, 1)
    data = snf(:getindex, a, 2)
    index = snf(:+, i, snf(:*, 3, snf(:-, j, 1)))
    block = GenOpt.ContiguousArrayOfVariables(0, (12,))
    variable = snf(:getindex, block, index)
    matrix = reshape(collect(1.0:12.0), 3, 4)
    coefficient = snf(:getindex, matrix, i, j)
    x = MOI.VariableIndex(1)
    iterators = [
        GenOpt.Iterator([(1, 0.5), (2, 1.5), (3, 2.5)]),
        GenOpt.Iterator([(1,), (2,), (3,), (4,)]),
    ]
    for T in (Float32, Float64, BigFloat)
        F = MOI.ScalarAffineFunction{T}
        affine = snf(:-, snf(:+, snf(:*, coefficient, variable), x), data)
        check_expansion(F, affine, iterators)
        check_expansion(F, snf(:-, variable), iterators)
        check_expansion(F, snf(:/, affine, snf(:+, data, 1)), iterators)
        check_expansion(F, snf(:^, affine, 1), iterators)
        check_expansion(F, snf(:+, snf(:exp, data), x), iterators; fast = false)
        Q = MOI.ScalarQuadraticFunction{T}
        # Variable 1 sometimes coincides with the indexed variable. Diagonal
        # coefficients must be doubled separately for each row.
        check_expansion(
            Q,
            snf(:*, snf(:+, variable, data), snf(:-, x, data)),
            iterators,
        )
        check_expansion(Q, snf(:^, affine, 2), iterators)
        check_expansion(Q, snf(:*, 2, variable, x), iterators)
        check_expansion(Q, snf(:*, snf(:^, variable, 2), data), iterators)
        check_expansion(Q, snf(:*, data, snf(:^, variable, 2)), iterators)
    end
end

@testset "scalar functions and term vectors have independent ownership" begin
    for F in (
        MOI.ScalarAffineFunction{Float64},
        MOI.ScalarQuadraticFunction{Float64},
    )
        functions = GenOpt._batch_functions(F, 3, 1, 1)
        GenOpt._batch_fill_constants!(functions, Ref(2.0))
        GenOpt._batch_fill_affine!(
            functions,
            1,
            Ref(3.0),
            MOI.VariableIndex.(1:3),
        )
        @test functions[1] !== functions[2]
        @test GenOpt._batch_affine_terms(functions[1]) !==
              GenOpt._batch_affine_terms(functions[2])
        functions[1].constant = 9.0
        GenOpt._batch_affine_terms(functions[1])[1] =
            MOI.ScalarAffineTerm(4.0, MOI.VariableIndex(1))
        @test functions[2].constant == 2.0
        @test only(GenOpt._batch_affine_terms(functions[2])).coefficient == 3.0
        if F <: MOI.ScalarQuadraticFunction
            @test functions[1].quadratic_terms !== functions[2].quadratic_terms
        end
    end
end

function test_fill_allocations(::Type{T}) where {T}
    rows = 4096
    functions =
        GenOpt._batch_functions(MOI.ScalarQuadraticFunction{T}, rows, 1, 1)
    variables = MOI.VariableIndex.(1:rows)
    coefficients = GenOpt._BatchAxis(T[1, 2, 3, 4], 1, rows)
    constant, variable = Ref(one(T)), Ref(MOI.VariableIndex(1))
    for coef in (
            constant,
            coefficients,
            GenOpt._batch_kernel_column(T, constant),
            GenOpt._batch_kernel_column(T, coefficients),
        ),
        vars in (
            variable,
            variables,
            GenOpt._batch_kernel_column(MOI.VariableIndex, variable),
            GenOpt._batch_kernel_column(MOI.VariableIndex, variables),
        )

        GenOpt._batch_fill_constants!(functions, coef)
        GenOpt._batch_fill_affine!(functions, 1, coef, vars)
        GenOpt._batch_fill_quadratic!(functions, 1, coef, vars, variables)
        @test (@allocated GenOpt._batch_fill_constants!(functions, coef)) == 0
        @test (@allocated GenOpt._batch_fill_affine!(
            functions,
            1,
            coef,
            vars,
        )) == 0
        @test (@allocated GenOpt._batch_fill_quadratic!(
            functions,
            1,
            coef,
            vars,
            variables,
        )) == 0
    end
    return
end
@testset "typed fill loops allocate nothing" begin
    test_fill_allocations(Float32)
    test_fill_allocations(Float64)
end

@testset "empty domains and constant families" begin
    x = MOI.VariableIndex(1)
    for F in (
        MOI.ScalarAffineFunction{Float64},
        MOI.ScalarQuadraticFunction{Float64},
    )
        check_expansion(F, snf(:+, x, 2.0), GenOpt.Iterator[])
        check_expansion(F, snf(:sin, x), [GenOpt.Iterator(Int[])])
    end
end

@testset "parameter keys and variable-valued iterators" begin
    index = GenOpt.IteratorIndex(1)
    data = Dict(:first => 2.0, :second => 3.0)
    expr = snf(:+, MOI.VariableIndex(1), snf(:getindex, data, index))
    check_expansion(
        MOI.ScalarAffineFunction{Float64},
        expr,
        [GenOpt.Iterator([:first, :second])];
        fast = false,
    )
    expr = snf(:*, index, 2.0)
    check_expansion(
        MOI.ScalarAffineFunction{Float64},
        expr,
        [GenOpt.Iterator(MOI.VariableIndex.(1:3))],
    )
end

@testset "typed data lookup and numeric function barriers" begin
    i, j = GenOpt.IteratorIndex(1), GenOpt.IteratorIndex(2)
    x = MOI.VariableIndex(1)
    F = MOI.ScalarAffineFunction{Float64}
    check_expansion(
        F,
        snf(:getindex, GenOpt.ContiguousArrayOfVariables(0, ())),
        GenOpt.Iterator[],
    )
    for T in (Int, Float32, Float64, BigFloat, MOI.VariableIndex)
        data = T === MOI.VariableIndex ? MOI.VariableIndex.(1:4) : T[1, 2, 3, 4]
        for (collection, indices) in
            ((data, (i,)), (reshape(data, 2, 2), (i, j)))
            expr = snf(:+, x, snf(:getindex, collection, indices...))
            check_expansion(
                F,
                expr,
                [GenOpt.Iterator([1, 2]), GenOpt.Iterator([1, 2])],
            )
            fixed = snf(:getindex, collection, ones(Int, length(indices))...)
            check_expansion(F, snf(:+, x, fixed), GenOpt.Iterator[])
        end
        check_expansion(F, snf(:getindex, fill(first(data))), GenOpt.Iterator[])
    end
    for T in (Float32, Float64, BigFloat),
        head in (:+, :-, :abs, :sin, :cos, :exp, :log, :sqrt)

        if T === BigFloat && head in (:sin, :cos, :log)
            continue
        end
        check_expansion(
            MOI.ScalarAffineFunction{T},
            snf(:+, x, snf(head, i)),
            [GenOpt.Iterator(T[1, 2])];
            fast = head in (:+, :-, :abs),
        )
        check_expansion(
            MOI.ScalarAffineFunction{T},
            snf(:+, x, snf(head, T(2))),
            GenOpt.Iterator[];
            fast = head in (:+, :-, :abs),
        )
    end
    for T in (Float32, Float64, BigFloat)
        expr = snf(:+, x, snf(:+, i, one(T), T(2)))
        check_expansion(
            MOI.ScalarAffineFunction{T},
            expr,
            [GenOpt.Iterator(T[1, 2])],
        )
    end
end

@testset "embedded polynomials and independent lazy-sum scopes" begin
    index = GenOpt.IteratorIndex(1)
    variable = snf(:getindex, GenOpt.ContiguousArrayOfVariables(0, (3,)), index)
    sum = GenOpt.SumGenerator{MOI.ScalarAffineFunction{Float64}}(
        variable,
        GenOpt.Iterator[GenOpt.Iterator([1, 2, 3])],
    )
    iterators = [GenOpt.Iterator([2, 3])]
    check_expansion(
        MOI.ScalarAffineFunction{Float64},
        snf(:*, index, sum),
        iterators,
    )
    check_expansion(
        MOI.ScalarQuadraticFunction{Float64},
        snf(:^, snf(:+, sum, index), 2),
        iterators,
    )
    affine = MOI.ScalarAffineFunction(
        [MOI.ScalarAffineTerm(2.0, MOI.VariableIndex(1))],
        3.0,
    )
    quadratic = MOI.ScalarQuadraticFunction(
        [
            MOI.ScalarQuadraticTerm(
                4.0,
                MOI.VariableIndex(1),
                MOI.VariableIndex(1),
            ),
        ],
        affine.terms,
        3.0,
    )
    check_expansion(
        MOI.ScalarAffineFunction{Float64},
        snf(:*, index, affine),
        iterators,
    )
    check_expansion(
        MOI.ScalarQuadraticFunction{Float64},
        snf(:*, index, quadratic),
        iterators,
    )
    polynomial = GenOpt.PolynomialFunction{typeof(affine)}(snf(:+, variable, 1))
    check_expansion(
        MOI.ScalarAffineFunction{Float64},
        snf(:+, polynomial, 1),
        [GenOpt.Iterator([1, 2, 3])],
    )
end

@testset "unsupported preparations retain scalar fallback" begin
    index = GenOpt.IteratorIndex(1)
    block = GenOpt.ContiguousArrayOfVariables(0, (2, 2, 2))
    expr = snf(:getindex, block, index, 1, 1)
    check_expansion(
        MOI.ScalarAffineFunction{Float64},
        expr,
        [GenOpt.Iterator([1, 2])];
        fast = false,
    )
    expr = snf(:getindex, GenOpt.ContiguousArrayOfVariables(0, (3,)), index)
    generator = GenOpt.FunctionGenerator{MOI.ScalarAffineFunction{Float64}}(
        snf(:sin, expr),
        [GenOpt.Iterator([1, 2, 3])],
    )
    model = MOI.Utilities.Model{Float64}()
    MOI.add_variables(model, 3)
    @test GenOpt._try_batch_constraints(model, generator, MOI.EqualTo(0.0)) ===
          nothing
    @test_throws InexactError GenOpt.add_constraints(
        model,
        generator,
        MOI.EqualTo(0.0),
    )
end

@testset "batch boundaries preserve row offsets" begin
    rows = GenOpt._BATCH_ROWS + 3
    model = MOI.Utilities.Model{Float64}()
    x = MOI.add_variable(model)
    index = GenOpt.IteratorIndex(1)
    template = snf(:-, snf(:*, index, x), index)
    generator = GenOpt.FunctionGenerator{MOI.ScalarAffineFunction{Float64}}(
        template,
        [GenOpt.Iterator(collect(1:rows))],
    )
    indices = GenOpt.add_constraints(model, generator, MOI.EqualTo(0.0))
    @test length(indices) == rows
    for row in (1, GenOpt._BATCH_ROWS, GenOpt._BATCH_ROWS + 1, rows)
        func = MOI.get(model, MOI.ConstraintFunction(), indices[row])
        @test only(func.terms).coefficient == row
        @test only(func.terms).variable == x
        @test MOI.get(model, MOI.ConstraintSet(), indices[row]).value == row
    end
end

@testset "numeric preprocessing and general fallbacks" begin
    index, other = GenOpt.IteratorIndex(1), GenOpt.IteratorIndex(2)
    block = GenOpt.ContiguousArrayOfVariables(0, (12,))
    iterators = [GenOpt.Iterator([1, 2]), GenOpt.Iterator([1, 2, 3])]
    for computed in (
        snf(:+, index, other, 1),
        snf(:+, 4, snf(:-, index)),
        snf(:+, snf(:^, index, 1), other),
    )
        check_expansion(
            MOI.ScalarAffineFunction{Float64},
            snf(:getindex, block, computed),
            iterators,
        )
    end
    for op in (:+, :-, :sin, :cos, :log, :sqrt, :abs)
        expr = snf(:+, MOI.VariableIndex(1), snf(op, index))
        check_expansion(
            MOI.ScalarAffineFunction{Float64},
            expr,
            [GenOpt.Iterator([1, 2])];
            fast = op in (:+, :-, :abs),
        )
    end
    check_expansion(
        MOI.ScalarAffineFunction{Float64},
        snf(:+, MOI.VariableIndex(1), snf(:atan, index)),
        [GenOpt.Iterator([1, 2])];
        fast = false,
    )
    mixed = [GenOpt.Iterator(Any[(1,), (2,)])]
    lookup = snf(:getindex, block, snf(:getindex, index, 1))
    check_expansion(
        MOI.ScalarAffineFunction{Float64},
        lookup,
        mixed;
        fast = false,
    )
    check_expansion(
        MOI.ScalarAffineFunction{Float64},
        snf(:getindex, fill(2.0)),
        GenOpt.Iterator[],
    )
    check_expansion(
        MOI.ScalarAffineFunction{Float64},
        snf(:+),
        GenOpt.Iterator[];
        fast = false,
    )
    check_expansion(
        MOI.ScalarAffineFunction{Float64},
        snf(:+, MOI.VariableIndex(1), snf(:^, index, 0.5)),
        [GenOpt.Iterator([1, 2])];
        fast = false,
    )
    # MOI's NaN-aware sine intentionally does not implement BigFloat. Keep
    # the scalar path's error instead of evaluating a different Base method.
    big_sine = GenOpt.FunctionGenerator{MOI.ScalarAffineFunction{BigFloat}}(
        snf(:+, MOI.VariableIndex(1), snf(:sin, index)),
        [GenOpt.Iterator(BigFloat[1, 2])],
    )
    big_model = MOI.Utilities.Model{BigFloat}()
    MOI.add_variable(big_model)
    @test GenOpt._try_batch_constraints(
        big_model,
        big_sine,
        MOI.EqualTo(big"0"),
    ) === nothing
    @test_throws MethodError GenOpt.add_constraints(
        big_model,
        big_sine,
        MOI.EqualTo(big"0"),
    )
    unary_product = GenOpt.FunctionGenerator{MOI.ScalarAffineFunction{Float64}}(
        snf(:*, 1),
        GenOpt.Iterator[],
    )
    scalar_model = MOI.Utilities.Model{Float64}()
    @test GenOpt._try_batch_constraints(
        scalar_model,
        unary_product,
        MOI.EqualTo(0.0),
    ) === nothing
    @test_throws KeyError GenOpt.add_constraints(
        scalar_model,
        unary_product,
        MOI.EqualTo(0.0),
    )
    # Numeric subexpressions promote all arguments before evaluation. A mixed
    # n-ary expression must not be rounded in Float32 before seeing Float64.
    mixed_precision = snf(:+, Float32(2^24), 1.0f0, -Float64(2^24))
    check_expansion(
        MOI.ScalarAffineFunction{Float64},
        snf(:+, MOI.VariableIndex(1), mixed_precision),
        GenOpt.Iterator[];
        fast = true,
    )
    check_expansion(
        MOI.ScalarAffineFunction{Rational{Int}},
        snf(:-, MOI.VariableIndex(1), 1),
        GenOpt.Iterator[];
        fast = false,
    )
    model = MOI.Utilities.Model{Float64}()
    x = MOI.add_variable(model)
    for (F, expr) in (
        (MOI.ScalarAffineFunction{Float64}, snf(:*, x, x)),
        (MOI.ScalarQuadraticFunction{Float64}, snf(:*, x, x, x)),
        (MOI.ScalarQuadraticFunction{Float64}, snf(:^, x, 3)),
        (MOI.ScalarAffineFunction{Float64}, snf(:/, x, x)),
    )
        generator = GenOpt.FunctionGenerator{F}(expr, GenOpt.Iterator[])
        @test GenOpt._try_batch_constraints(
            model,
            generator,
            MOI.EqualTo(0.0),
        ) === nothing
        @test_throws InexactError GenOpt.add_constraints(
            model,
            generator,
            MOI.EqualTo(0.0),
        )
    end
end

end
