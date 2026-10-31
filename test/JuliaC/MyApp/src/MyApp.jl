# Copyright (c) 2024: Benoît Legat and contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module MyApp

import GenOpt
import MathOptInterface as MOI

# Exercise the actual bridge on concrete storage, without unrelated bridges,
# a solver, or a dynamically typed UniversalFallback model.
MOI.Utilities.@model(
    BridgeModel,
    (),
    (MOI.EqualTo, MOI.GreaterThan, MOI.LessThan),
    (),
    (),
    (),
    (MOI.ScalarAffineFunction, MOI.ScalarQuadraticFunction),
    (),
    (),
)

const Affine = MOI.ScalarAffineFunction{Float64}
const Quadratic = MOI.ScalarQuadraticFunction{Float64}

snf(head, args...) = MOI.ScalarNonlinearFunction(head, Any[args...])

function bridge(model, ::Type{F}, ::Type{S}, expr, iterators, set) where {F,S}
    generator = GenOpt.FunctionGenerator{F}(expr, iterators)
    return MOI.Bridges.Constraint.bridge_constraint(
        GenOpt.FunctionGeneratorBridge{Float64,F,S},
        model,
        generator,
        set,
    )
end

function check_metadata(
    model,
    result::GenOpt.FunctionGeneratorBridge{T,F,S},
    n,
) where {T,F,S}
    @assert MOI.get(result, MOI.NumberOfConstraints{F,S}()) == n
    indices = MOI.get(result, MOI.ListOfConstraintIndices{F,S}())
    @assert length(indices) == n
    @assert MOI.get(model, MOI.NumberOfConstraints{F,S}()) == n
    @assert MOI.dimension(MOI.get(model, MOI.ConstraintSet(), result)) == n
    return indices
end

function check_affine(::Type{S}, set) where {S}
    model = BridgeModel{Float64}()
    x = MOI.add_variables(model, 3)
    block = GenOpt.ContiguousArrayOfVariables(0, (3,))
    i = GenOpt.IteratorIndex(1)
    price = [2.0, 4.0, 7.0]
    # The iterator value is both an integer index and a Float64 coefficient.
    expr = snf(
        :-,
        snf(:+, snf(:*, i, snf(:getindex, block, i)), snf(:getindex, price, i)),
        1,
    )
    result = bridge(
        model,
        Affine,
        S,
        expr,
        GenOpt.Iterator[GenOpt.Iterator([1, 2, 3])],
        set,
    )
    indices = check_metadata(model, result, 3)
    for k in 1:3
        f = MOI.get(model, MOI.ConstraintFunction(), indices[k])
        @assert f.constant == 0.0
        @assert length(f.terms) == 1
        @assert f.terms[1].variable == x[k]
        @assert f.terms[1].coefficient == Float64(k)
        @assert MOI.constant(MOI.get(model, MOI.ConstraintSet(), indices[k])) ==
                1.0 - price[k]
    end
    MOI.delete(model, result)
    @assert MOI.get(model, MOI.NumberOfConstraints{Affine,S}()) == 0
    return
end

function check_matrix()
    model = BridgeModel{Float64}()
    x = MOI.add_variables(model, 4)
    block = GenOpt.ContiguousArrayOfVariables(0, (2, 2))
    i, j = GenOpt.IteratorIndex(1), GenOpt.IteratorIndex(2)
    demand = [1.0 3.0; 2.0 4.0]
    expr = snf(:-, snf(:getindex, block, i, j), snf(:getindex, demand, i, j))
    iterators =
        GenOpt.Iterator[GenOpt.Iterator([1, 2]), GenOpt.Iterator([1, 2])]
    result = bridge(
        model,
        Affine,
        MOI.EqualTo{Float64},
        expr,
        iterators,
        MOI.Zeros(4),
    )
    indices = check_metadata(model, result, 4)
    # Cartesian-product order has the first iterator varying fastest.
    for k in 1:4
        f = MOI.get(model, MOI.ConstraintFunction(), indices[k])
        @assert f.constant == 0.0
        @assert length(f.terms) == 1
        @assert f.terms[1].variable == x[k]
        @assert f.terms[1].coefficient == 1.0
        @assert MOI.get(model, MOI.ConstraintSet(), indices[k]).value ==
                demand[k]
    end
    return
end

function check_wrapped_iterator()
    model = BridgeModel{Float64}()
    x = MOI.add_variables(model, 3)
    block = GenOpt.ContiguousArrayOfVariables(0, (3,))
    # JuMP prepares iterator values as tuples and indexes into those tuples.
    i = snf(:getindex, GenOpt.IteratorIndex(1), 1)
    # Promote mixed integer and floating-point columns before an n-ary sum.
    expr = snf(:-, snf(:getindex, block, snf(:+, i, snf(:*, 0.0, i), 1)), 2.0)
    iterators = GenOpt.Iterator[GenOpt.Iterator([(1,), (2,)])]
    result = bridge(
        model,
        Affine,
        MOI.EqualTo{Float64},
        expr,
        iterators,
        MOI.Zeros(2),
    )
    indices = check_metadata(model, result, 2)
    for k in 1:2
        f = MOI.get(model, MOI.ConstraintFunction(), indices[k])
        @assert length(f.terms) == 1
        @assert f.terms[1].variable == x[k+1]
        @assert f.terms[1].coefficient == 1.0
        @assert MOI.get(model, MOI.ConstraintSet(), indices[k]).value == 2.0
    end
    return
end

function check_quadratic()
    model = BridgeModel{Float64}()
    x = MOI.add_variables(model, 2)
    block = GenOpt.ContiguousArrayOfVariables(0, (2,))
    i = GenOpt.IteratorIndex(1)
    xi = snf(:getindex, block, i)
    # Products of affine factors, literal squares, and integer coefficients.
    expr = snf(
        :+,
        snf(:*, snf(:+, xi, 1), snf(:-, xi, 1)),
        snf(:^, xi, 2),
        snf(:*, i, xi),
    )
    iterators = GenOpt.Iterator[GenOpt.Iterator([1, 2])]
    result = bridge(
        model,
        Quadratic,
        MOI.LessThan{Float64},
        expr,
        iterators,
        MOI.Nonpositives(2),
    )
    indices = check_metadata(model, result, 2)
    for k in 1:2
        f = MOI.get(model, MOI.ConstraintFunction(), indices[k])
        @assert f.constant == 0.0
        diagonal = 0.0
        for term in f.quadratic_terms
            @assert term.variable_1 == x[k]
            @assert term.variable_2 == x[k]
            diagonal += term.coefficient
        end
        @assert diagonal == 4.0
        coefficient = 0.0
        for term in f.affine_terms
            @assert term.variable == x[k]
            coefficient += term.coefficient
        end
        @assert coefficient == Float64(k)
        @assert MOI.get(model, MOI.ConstraintSet(), indices[k]).upper == 1.0
    end
    return
end

function check_empty()
    model = BridgeModel{Float64}()
    MOI.add_variable(model)
    block = GenOpt.ContiguousArrayOfVariables(0, (1,))
    expr = snf(:getindex, block, GenOpt.IteratorIndex(1))
    iterators = GenOpt.Iterator[GenOpt.Iterator(Int[])]
    result = bridge(
        model,
        Affine,
        MOI.EqualTo{Float64},
        expr,
        iterators,
        MOI.Zeros(0),
    )
    check_metadata(model, result, 0)
    return
end

function check_no_iterators()
    model = BridgeModel{Float64}()
    x = MOI.add_variable(model)
    # A Cartesian product of no iterators expands once, not zero times.
    expr = snf(:-, snf(:*, 2, x), 3.0)
    result = bridge(
        model,
        Affine,
        MOI.GreaterThan{Float64},
        expr,
        GenOpt.Iterator[],
        MOI.Nonnegatives(1),
    )
    indices = check_metadata(model, result, 1)
    f = MOI.get(model, MOI.ConstraintFunction(), only(indices))
    @assert f.constant == 0.0
    @assert length(f.terms) == 1
    @assert f.terms[1].variable == x
    @assert f.terms[1].coefficient == 2.0
    @assert MOI.get(model, MOI.ConstraintSet(), only(indices)).lower == 3.0
    return
end

function check_typed_polynomials()
    model = BridgeModel{Float64}()
    x = MOI.add_variables(model, 3)
    block = GenOpt.ContiguousArrayOfVariables(0, (3,))
    i = GenOpt.IteratorIndex(1)
    sum = GenOpt.SumGenerator{Affine}(
        snf(:*, i, snf(:getindex, block, i)),
        GenOpt.Iterator[GenOpt.Iterator([1, 2, 3])],
    )
    affine = GenOpt.PolynomialFunction{Affine}(snf(:+, sum, 1.0))
    lowered = convert(Affine, affine)
    @assert lowered.constant == 1.0
    @assert length(lowered.terms) == 3
    @assert [term.coefficient for term in lowered.terms] == [1.0, 2.0, 3.0]
    result = MOI.Bridges.Constraint.bridge_constraint(
        GenOpt.PolynomialFunctionBridge{Float64,Affine,MOI.LessThan{Float64}},
        model,
        affine,
        MOI.LessThan(10.0),
    )
    @assert MOI.get(model, MOI.ConstraintSet(), result.constraint).upper == 9.0
    @assert MOI.get(model, MOI.ConstraintFunction(), result.constraint).constant ==
            0.0
    for exponent in (1, 1.0, 1.0f0)
        identity = GenOpt.PolynomialFunction{Affine}(snf(:^, sum, exponent))
        f = convert(Affine, identity)
        @assert length(f.terms) == 3
        @assert f.constant == 0.0
    end
    for exponent in (2.0, 2.0f0)
        square = GenOpt.PolynomialFunction{Quadratic}(snf(:^, sum, exponent))
        @assert length(convert(Quadratic, square).quadratic_terms) == 9
    end
    quadratic = GenOpt.PolynomialFunction{Quadratic}(snf(:^, sum, 2))
    objective_bridge = MOI.Bridges.Objective.bridge_objective(
        MOI.Bridges.Objective.FunctionConversionBridge{
            Float64,
            Quadratic,
            typeof(quadratic),
        },
        model,
        quadratic,
    )
    q = MOI.get(model, MOI.ObjectiveFunction{Quadratic}())
    @assert length(q.quadratic_terms) == 9
    @assert isempty(q.affine_terms)
    @assert q.constant == 0.0
    @assert MOI.get(objective_bridge, MOI.NumberOfVariables()) == 0
    @assert MOI.get(model, MOI.ConstraintFunction(), result).func isa
            MOI.ScalarNonlinearFunction
    @assert length(x) == 3
    return
end

function main(args::Vector{String})::Cint
    check_affine(MOI.GreaterThan{Float64}, MOI.Nonnegatives(3))
    check_affine(MOI.LessThan{Float64}, MOI.Nonpositives(3))
    check_affine(MOI.EqualTo{Float64}, MOI.Zeros(3))
    check_matrix()
    check_wrapped_iterator()
    check_quadratic()
    check_empty()
    check_no_iterators()
    check_typed_polynomials()
    return 0
end

@static if VERSION >= v"1.11"
    @main
end

end # module MyApp
