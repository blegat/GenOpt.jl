# Copyright (c) 2024: Benoît Legat and contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestPolynomialBuilder

using Test
import GenOpt
import MathOptInterface as MOI

_snf(head, args...) = MOI.ScalarNonlinearFunction(head, Any[args...])

function _variable_sum(indices)
    return GenOpt.SumGenerator{MOI.VariableIndex}(
        _snf(
            :getindex,
            GenOpt.ContiguousArrayOfVariables(0, (3,)),
            GenOpt.IteratorIndex(1),
        ),
        GenOpt.Iterator[GenOpt.Iterator(indices)],
    )
end

function _build(::Type{F}, expression) where {F}
    return @inferred GenOpt._build_function(F, expression, Any[])
end

function test_affine_and_wrapped_sums()
    x = MOI.VariableIndex.(1:3)
    sum = _variable_sum([1, 3])
    for T in (Float32, Float64)
        F = MOI.ScalarAffineFunction{T}
        expected = MOI.ScalarAffineFunction(
            [MOI.ScalarAffineTerm(one(T), xi) for xi in x[[1, 3]]],
            zero(T),
        )
        @test _build(F, sum) ≈ expected
        expression = _snf(:+, sum, T(2))
        wrapped = GenOpt.PolynomialFunction{F}(expression)
        expected.constant = T(2)
        @test _build(F, wrapped) ≈ expected
        @test _build(F, _snf(:-, wrapped)) ≈
              MOI.Utilities.operate(-, T, expected)
        @test _build(F, _snf(:/, wrapped, T(2))) ≈
              MOI.Utilities.operate(/, T, expected, T(2))
        for exponent in (1, one(T))
            @test _build(F, _snf(:^, wrapped, exponent)) ≈ expected
        end
    end
    return
end

function test_quadratic_sums_and_products()
    x = MOI.VariableIndex.(1:3)
    left, right = _variable_sum([1, 2]), _variable_sum([2, 3])
    for T in (Float32, Float64)
        F = MOI.ScalarQuadraticFunction{T}
        square_sum =
            GenOpt.SumGenerator{F}(_snf(:^, left.func, 2), left.iterators)
        expected = MOI.ScalarQuadraticFunction(
            [MOI.ScalarQuadraticTerm(T(2), xi, xi) for xi in x[1:2]],
            MOI.ScalarAffineTerm{T}[],
            zero(T),
        )
        @test _build(F, square_sum) ≈ expected
        product = _snf(:*, left, right)
        expected = MOI.ScalarQuadraticFunction(
            [
                MOI.ScalarQuadraticTerm(one(T), x[1], x[2]),
                MOI.ScalarQuadraticTerm(one(T), x[1], x[3]),
                MOI.ScalarQuadraticTerm(T(2), x[2], x[2]),
                MOI.ScalarQuadraticTerm(one(T), x[2], x[3]),
            ],
            MOI.ScalarAffineTerm{T}[],
            zero(T),
        )
        @test _build(F, product) ≈ expected
        square = _snf(:^, left, 2)
        expected = MOI.ScalarQuadraticFunction(
            [
                MOI.ScalarQuadraticTerm(T(2), x[1], x[1]),
                MOI.ScalarQuadraticTerm(T(2), x[1], x[2]),
                MOI.ScalarQuadraticTerm(T(2), x[2], x[2]),
            ],
            MOI.ScalarAffineTerm{T}[],
            zero(T),
        )
        @test _build(F, square) ≈ expected
        tagged = GenOpt.PolynomialFunction{F}(square)
        @test _build(F, tagged) ≈ expected
        @test _build(F, _snf(:/, tagged, T(2))) ≈
              MOI.Utilities.operate(/, T, expected, T(2))
        @test _build(F, _snf(:^, tagged, one(T))) ≈ expected
    end
    return
end

function test_constant_sum_coefficients()
    x = MOI.VariableIndex(1)
    for T in (Float32, Float64)
        F = MOI.ScalarAffineFunction{T}
        data = T[nextfloat(one(T)), 2, 3]
        sum = GenOpt.SumGenerator{F}(
            _snf(:getindex, data, GenOpt.IteratorIndex(1)),
            GenOpt.Iterator[GenOpt.Iterator([1, 2, 3])],
        )
        for (coefficient, expected) in (
            (_snf(:+, sum, one(T)), Base.sum(data) + one(T)),
            (_snf(:sin, sum), sin(Base.sum(data))),
        )
            @test GenOpt._try_const(T, coefficient, Any[]) === expected
            expression = _snf(:*, coefficient, x)
            reference = MOI.ScalarAffineFunction(
                [MOI.ScalarAffineTerm(expected, x)],
                zero(T),
            )
            @test _build(F, expression) ≈ reference
            wrapped = GenOpt.PolynomialFunction{F}(coefficient)
            @test GenOpt._try_const(T, wrapped, Any[]) === expected
            @test _build(F, _snf(:*, wrapped, x)) ≈ reference
            divisor = _snf(:+, sum, one(T))
            reference = MOI.ScalarAffineFunction(
                [
                    MOI.ScalarAffineTerm(
                        T(one(T) / (Base.sum(data) + one(T))),
                        x,
                    ),
                ],
                zero(T),
            )
            @test _build(F, _snf(:/, x, divisor)) ≈ reference
        end
        empty_sum = GenOpt.SumGenerator{F}(
            sum.func,
            GenOpt.Iterator[GenOpt.Iterator(Int[])],
        )
        @test GenOpt._try_const(T, empty_sum, Any[]) === zero(T)
        @test _build(F, empty_sum) ≈ convert(F, zero(T))
    end
    return
end

function test_filtered_sums()
    sum = _variable_sum([1, 2, 3])
    for T in (Float32, Float64),
        F in (MOI.ScalarAffineFunction{T}, MOI.ScalarQuadraticFunction{T})

        for (value, expected) in ((2, MOI.VariableIndex(2)), (4, zero(T)))
            filtered = GenOpt.FilteredSumGenerator{MOI.VariableIndex}(
                sum.func,
                sum.iterators,
                GenOpt.FilterExpression(
                    :(==),
                    Any[GenOpt.IteratorIndex(1), value],
                ),
            )
            @test _build(F, filtered) ≈ convert(F, expected)
        end
        i = GenOpt.iterator([1, 2, 3])
        wrapped_index = _snf(:getindex, GenOpt.IteratorIndex(1), 1)
        filtered = GenOpt.FilteredSumGenerator{MOI.VariableIndex}(
            _snf(
                :getindex,
                GenOpt.ContiguousArrayOfVariables(0, (3,)),
                wrapped_index,
            ),
            i.iterators,
            GenOpt.FilterExpression(:(==), Any[GenOpt._Filtered(i), 2]),
        )
        @test _build(F, filtered) ≈ convert(F, MOI.VariableIndex(2))
        excluded = GenOpt.FilteredSumGenerator{MOI.VariableIndex}(
            sum.func,
            sum.iterators,
            GenOpt.FilterExpression(
                :!,
                Any[GenOpt.FilterExpression(
                    :(==),
                    Any[GenOpt.IteratorIndex(1), 2],
                )],
            ),
        )
        expected = MOI.ScalarAffineFunction(
            [
                MOI.ScalarAffineTerm(one(T), MOI.VariableIndex(i)) for
                i in (1, 3)
            ],
            zero(T),
        )
        @test _build(F, excluded) ≈ convert(F, expected)
    end
    return
end

function test_sum_domain_shapes()
    for T in (Float32, Float64)
        F = MOI.ScalarAffineFunction{T}
        constant = GenOpt.SumGenerator{F}(_snf(:+, T(2)), GenOpt.Iterator[])
        @test _build(F, constant) ≈ convert(F, T(2))
        sum = GenOpt.SumGenerator{MOI.VariableIndex}(
            _snf(
                :getindex,
                GenOpt.ContiguousArrayOfVariables(0, (1, 3)),
                GenOpt.IteratorIndex(1),
                GenOpt.IteratorIndex(2),
            ),
            GenOpt.Iterator[GenOpt.Iterator([1]), GenOpt.Iterator([1, 3])],
        )
        expected = MOI.ScalarAffineFunction(
            [
                MOI.ScalarAffineTerm(one(T), MOI.VariableIndex(i)) for
                i in (1, 3)
            ],
            zero(T),
        )
        @test _build(F, sum) ≈ expected
    end
    return
end

function test_filter_scope_validation()
    for T in (Float32, Float64), axis in ([1, 2], Int[])
        F = MOI.ScalarAffineFunction{T}
        inner = GenOpt.iterator(axis)
        outer = GenOpt.iterator([1, 2])
        index = _snf(:getindex, GenOpt.IteratorIndex(1), 1)
        sum = GenOpt.FilteredSumGenerator{MOI.VariableIndex}(
            _snf(:getindex, GenOpt.ContiguousArrayOfVariables(0, (3,)), index),
            inner.iterators,
            GenOpt.FilterExpression(:(==), Any[inner, GenOpt._Filtered(outer)]),
        )
        # Both iterators use ordinal 1, but have distinct binding identities.
        # Treating the outer iterator as local would silently select all terms.
        @test_throws r"Correlated lazy-sum filters" _build(F, sum)
        generator = GenOpt.FunctionGenerator{F}(_snf(:+, sum), outer.iterators)
        model = MOI.Utilities.Model{T}()
        MOI.add_variables(model, 3)
        @test_throws r"Correlated lazy-sum filters" GenOpt.add_constraints(
            model,
            generator,
            MOI.LessThan(zero(T)),
        )
    end
    for T in (Float32, Float64)
        F = MOI.ScalarAffineFunction{T}
        selected = GenOpt.iterator([1, 2, 3])
        other = GenOpt.iterator([1])
        sum = GenOpt.FilteredSumGenerator{MOI.VariableIndex}(
            _snf(
                :getindex,
                GenOpt.ContiguousArrayOfVariables(0, (3,)),
                _snf(:getindex, GenOpt.IteratorIndex(2), 1),
            ),
            GenOpt.Iterator[only(other.iterators), only(selected.iterators)],
            GenOpt.FilterExpression(:(==), Any[selected, 2]),
        )
        # A local reference may have a different ordinal in its source domain.
        @test _build(F, sum) ≈ convert(F, MOI.VariableIndex(2))
    end
    return
end

function test_nested_iterator_scopes()
    x = MOI.VariableIndex.(1:3)
    inner = _variable_sum([1, 3])
    outer = _variable_sum([2, 3])
    for T in (Float32, Float64)
        F = MOI.ScalarQuadraticFunction{T}
        nested =
            GenOpt.SumGenerator{F}(_snf(:*, outer.func, inner), outer.iterators)
        expected = MOI.ScalarQuadraticFunction(
            [
                MOI.ScalarQuadraticTerm(one(T), x[2], x[1]),
                MOI.ScalarQuadraticTerm(one(T), x[2], x[3]),
                MOI.ScalarQuadraticTerm(one(T), x[3], x[1]),
                MOI.ScalarQuadraticTerm(T(2), x[3], x[3]),
            ],
            MOI.ScalarAffineTerm{T}[],
            zero(T),
        )
        @test _build(F, nested) ≈ expected
        @test GenOpt._build_function(F, nested, Any[99]) ≈ expected
        affine = MOI.ScalarAffineFunction{T}
        generator = GenOpt.FunctionGenerator{affine}(
            _snf(:-, inner, outer.func),
            outer.iterators,
        )
        model = MOI.Utilities.Model{T}()
        MOI.add_variables(model, 3)
        rows = GenOpt.add_constraints(model, generator, MOI.LessThan(zero(T)))
        @test length(rows) == 2
        for (row, variable) in zip(rows, x[2:3])
            reference = MOI.ScalarAffineFunction(
                [
                    MOI.ScalarAffineTerm(one(T), x[1]),
                    MOI.ScalarAffineTerm(one(T), x[3]),
                    MOI.ScalarAffineTerm(-one(T), variable),
                ],
                zero(T),
            )
            @test MOI.get(model, MOI.ConstraintFunction(), row) ≈ reference
        end
    end
    return
end

function test_incompatible_polynomial_promises()
    sum = _variable_sum([1, 2])
    for T in (Float32, Float64)
        affine = MOI.ScalarAffineFunction{T}
        quadratic = MOI.ScalarQuadraticFunction{T}
        square = _snf(:*, sum, sum)
        @test_throws InexactError _build(
            affine,
            GenOpt.PolynomialFunction{affine}(square),
        )
        cubic = _snf(:*, square, sum)
        @test_throws InexactError _build(
            quadratic,
            GenOpt.PolynomialFunction{quadratic}(cubic),
        )
        for F in (affine, quadratic)
            @test_throws InexactError _build(
                F,
                _snf(:/, sum, MOI.VariableIndex(3)),
            )
        end
    end
    return
end

function test_wrapped_iterator_index_chain()
    expression = _snf(:getindex, GenOpt.IteratorIndex(1), 1)
    values = Any[GenOpt.IteratorIndex(2), (2,)]
    @test @inferred(GenOpt._eval_index(expression, values)) === 2.0
    return
end

function test_literal_exponent_types()
    for (value, unit, square) in (
        (0, false, false),
        (1, true, false),
        (2, false, true),
        (0.0, false, false),
        (1.0, true, false),
        (2.0, false, true),
        (0.0f0, false, false),
        (1.0f0, true, false),
        (2.0f0, false, true),
        (1 // 1, true, false),
        (2 // 1, false, true),
        (MOI.VariableIndex(1), false, false),
    )
        @test (@inferred GenOpt._is_unit_exponent(value)) === unit
        @test (@inferred GenOpt._is_square_exponent(value)) === square
    end
    sum = _variable_sum([1, 2])
    for T in (Float32, Float64)
        affine = MOI.ScalarAffineFunction(
            [MOI.ScalarAffineTerm(one(T), MOI.VariableIndex(i)) for i in 1:2],
            zero(T),
        )
        for exponent in (1.0, 1.0f0),
            F in (MOI.ScalarAffineFunction{T}, MOI.ScalarQuadraticFunction{T})

            @test _build(F, _snf(:^, sum, exponent)) ≈ convert(F, affine)
        end
        quadratic = MOI.ScalarQuadraticFunction(
            [
                MOI.ScalarQuadraticTerm(
                    T(2),
                    MOI.VariableIndex(1),
                    MOI.VariableIndex(1),
                ),
                MOI.ScalarQuadraticTerm(
                    T(2),
                    MOI.VariableIndex(1),
                    MOI.VariableIndex(2),
                ),
                MOI.ScalarQuadraticTerm(
                    T(2),
                    MOI.VariableIndex(2),
                    MOI.VariableIndex(2),
                ),
            ],
            MOI.ScalarAffineTerm{T}[],
            zero(T),
        )
        for exponent in (2.0, 2.0f0)
            @test _build(typeof(quadratic), _snf(:^, sum, exponent)) ≈ quadratic
        end
    end
    return
end

function test_constant_sum_integer_precision()
    F = MOI.ScalarAffineFunction{BigFloat}
    large = Int64(2)^53 + 1
    sum = GenOpt.SumGenerator{F}(
        _snf(:getindex, [large, Int64(2)], GenOpt.IteratorIndex(1)),
        GenOpt.Iterator[GenOpt.Iterator([1, 2])],
    )
    expected = BigFloat(large) + BigFloat(2)
    @test GenOpt._try_const(BigFloat, sum, Any[]) == expected
    lowered = _build(F, _snf(:*, sum, MOI.VariableIndex(1)))
    @test only(lowered.terms).coefficient == expected
    @test only(lowered.terms).coefficient isa BigFloat
    for polynomial in (
        MOI.ScalarAffineFunction(
            [MOI.ScalarAffineTerm(1.0, MOI.VariableIndex(1))],
            0.0,
        ),
        MOI.ScalarQuadraticFunction(
            [
                MOI.ScalarQuadraticTerm(
                    2.0,
                    MOI.VariableIndex(1),
                    MOI.VariableIndex(1),
                ),
            ],
            MOI.ScalarAffineTerm{Float64}[],
            0.0,
        ),
    )
        embedded = GenOpt.SumGenerator{typeof(polynomial)}(
            _snf(:+, polynomial),
            GenOpt.Iterator[],
        )
        @test GenOpt._try_const(Float64, embedded, Any[]) === nothing
    end
    return
end

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

end

TestPolynomialBuilder.runtests()
