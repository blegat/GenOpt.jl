# Copyright (c) 2024: Benoît Legat and contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    FunctionGeneratorBridge{T,F,S}

Bridge that expands a `FunctionGenerator{F}` constraint into individual
scalar `F`-in-`S` constraints.

The `FunctionGenerator` contains a template `MOI.ScalarNonlinearFunction`
with `IteratorIndex` placeholders. This bridge substitutes concrete values
from the iterators for each combination and converts each expanded expression
to the type `F` declared by the `FunctionGenerator{F}` type parameter.
If the expanded expression cannot be converted to `F`, an error is thrown.
"""
struct FunctionGeneratorBridge{T,F,S} <: MOI.Bridges.Constraint.AbstractBridge
    constraints::Vector{MOI.ConstraintIndex{F,S}}
end

function MOI.Bridges.Constraint.bridge_constraint(
    ::Type{FunctionGeneratorBridge{T,F,S}},
    model::MOI.ModelLike,
    func::FunctionGenerator{F},
    set::MOI.Utilities.VectorLinearSet,
) where {T,F,S}
    scalar_set = S(zero(T))
    constraints = MOI.ConstraintIndex{F,S}[]
    sizes = Tuple(length.(func.iterators))
    for idx in CartesianIndices(sizes)
        values = [
            func.iterators[k].values[idx[k]] for k in eachindex(func.iterators)
        ]
        expanded = _expand(func.func, values)
        scalar_func = convert(F, expanded)
        ci = MOI.Utilities.normalize_and_add_constraint(
            model,
            scalar_func,
            scalar_set,
        )
        push!(constraints, ci)
    end
    return FunctionGeneratorBridge{T,F,S}(constraints)
end

function MOI.supports_constraint(
    ::Type{FunctionGeneratorBridge{T}},
    ::Type{<:FunctionGenerator},
    ::Type{<:MOI.Utilities.VectorLinearSet},
) where {T}
    return true
end

function MOI.Bridges.Constraint.concrete_bridge_type(
    ::Type{FunctionGeneratorBridge{T}},
    ::Type{FunctionGenerator{F}},
    VS::Type{<:MOI.Utilities.VectorLinearSet},
) where {T,F}
    S = MOI.Utilities.scalar_set_type(VS, T)
    return FunctionGeneratorBridge{T,F,S}
end

function MOI.Bridges.added_constraint_types(
    ::Type{<:FunctionGeneratorBridge{T,F,S}},
) where {T,F,S}
    return Tuple{Type,Type}[(F, S)]
end

function MOI.Bridges.added_constrained_variable_types(
    ::Type{<:FunctionGeneratorBridge},
)
    return Tuple{Type}[]
end

function MOI.get(
    bridge::FunctionGeneratorBridge{T,F,S},
    ::MOI.NumberOfConstraints{F,S},
) where {T,F,S}
    return length(bridge.constraints)
end

function MOI.get(
    bridge::FunctionGeneratorBridge{T,F,S},
    ::MOI.ListOfConstraintIndices{F,S},
) where {T,F,S}
    return copy(bridge.constraints)
end

function MOI.get(
    ::MOI.ModelLike,
    ::MOI.ConstraintSet,
    bridge::FunctionGeneratorBridge{T,F,S},
) where {T,F,S}
    return MOI.Utilities.set_with_dimension(
        MOI.Utilities.vector_set_type(S),
        length(bridge.constraints),
    )
end

function MOI.delete(model::MOI.ModelLike, bridge::FunctionGeneratorBridge)
    for ci in bridge.constraints
        MOI.delete(model, ci)
    end
    return
end

# --- Expression expansion ---

"""
    _expand(func, values)

Recursively expand an `MOI.ScalarNonlinearFunction` template by substituting
`IteratorIndex` placeholders with concrete `values`.

Evaluates `:getindex` nodes (for variable and data lookups) and simplifies
pure-numeric subexpressions.
"""
function _expand(func::MOI.ScalarNonlinearFunction, values)
    args = [_expand(arg, values) for arg in func.args]
    if func.head == :getindex
        collection = args[1]
        indices = [_to_index(a) for a in args[2:end]]
        return getindex(collection, indices...)
    end
    if all(_is_numeric, args)
        return _eval_op(func.head, args)
    end
    return MOI.ScalarNonlinearFunction(func.head, args)
end

_expand(idx::IteratorIndex, values) = values[idx.value]
_expand(x, _) = x  # constants, MOI.VariableIndex, etc.

_is_numeric(::Number) = true
_is_numeric(_) = false

_to_index(x::Integer) = Int(x)
_to_index(x::AbstractFloat) = Int(x)
_to_index(x) = x

function _eval_op(head::Symbol, args::Vector)
    registry = MOI.Nonlinear.OperatorRegistry()
    float_args = Float64.(args)
    if length(float_args) == 1
        return MOI.Nonlinear.eval_univariate_function(
            registry,
            head,
            float_args[1],
        )
    else
        return MOI.Nonlinear.eval_multivariate_function(
            registry,
            head,
            float_args,
        )
    end
end
# --- Conversion from expanded ScalarNonlinearFunction to MOI scalar functions ---
# `bridge_constraint` calls `convert(F, expanded)` where `F` is the function
# type of the `FunctionGenerator`. MOI does not provide a generic
# `convert(::ScalarAffineFunction, ::ScalarNonlinearFunction)` /
# `convert(::ScalarQuadraticFunction, ::ScalarNonlinearFunction)`, so we
# define them here by walking the `:+`, `:-`, `:*`, `:^` head structure.

function Base.convert(
    ::Type{MOI.ScalarAffineFunction{T}},
    expr::MOI.ScalarNonlinearFunction,
) where {T}
    terms, constant = _collect_affine_terms(T, expr)
    terms === nothing &&
        throw(InexactError(:convert, MOI.ScalarAffineFunction{T}, expr))
    return MOI.ScalarAffineFunction(terms, T(constant))
end

function _collect_affine_terms(
    ::Type{T},
    expr::MOI.ScalarNonlinearFunction,
) where {T}
    if expr.head == :+ && length(expr.args) == 2
        t1, c1 = _collect_affine_terms(T, expr.args[1])
        t2, c2 = _collect_affine_terms(T, expr.args[2])
        (t1 === nothing || t2 === nothing) && return (nothing, zero(T))
        return (vcat(t1, t2), c1 + c2)
    elseif expr.head == :- && length(expr.args) == 2
        t1, c1 = _collect_affine_terms(T, expr.args[1])
        t2, c2 = _collect_affine_terms(T, expr.args[2])
        (t1 === nothing || t2 === nothing) && return (nothing, zero(T))
        neg_t2 = [MOI.ScalarAffineTerm(-t.coefficient, t.variable) for t in t2]
        return (vcat(t1, neg_t2), c1 - c2)
    elseif expr.head == :- && length(expr.args) == 1
        t1, c1 = _collect_affine_terms(T, expr.args[1])
        t1 === nothing && return (nothing, zero(T))
        neg_t1 = [MOI.ScalarAffineTerm(-t.coefficient, t.variable) for t in t1]
        return (neg_t1, -c1)
    elseif expr.head == :* && length(expr.args) == 2
        a1, a2 = expr.args
        if a1 isa Number && a2 isa MOI.VariableIndex
            return ([MOI.ScalarAffineTerm(T(a1), a2)], zero(T))
        elseif a2 isa Number && a1 isa MOI.VariableIndex
            return ([MOI.ScalarAffineTerm(T(a2), a1)], zero(T))
        elseif a1 isa Number && a2 isa MOI.ScalarNonlinearFunction
            t2, c2 = _collect_affine_terms(T, a2)
            t2 === nothing && return (nothing, zero(T))
            scaled = [
                MOI.ScalarAffineTerm(T(a1) * t.coefficient, t.variable) for
                t in t2
            ]
            return (scaled, T(a1) * c2)
        elseif a2 isa Number && a1 isa MOI.ScalarNonlinearFunction
            t1, c1 = _collect_affine_terms(T, a1)
            t1 === nothing && return (nothing, zero(T))
            scaled = [
                MOI.ScalarAffineTerm(T(a2) * t.coefficient, t.variable) for
                t in t1
            ]
            return (scaled, T(a2) * c1)
        elseif a1 isa Number && a2 isa Number
            return (MOI.ScalarAffineTerm{T}[], T(a1 * a2))
        else
            return (nothing, zero(T))
        end
    else
        return (nothing, zero(T))
    end
end

function _collect_affine_terms(::Type{T}, x::MOI.VariableIndex) where {T}
    return ([MOI.ScalarAffineTerm(one(T), x)], zero(T))
end

function _collect_affine_terms(::Type{T}, x::Number) where {T}
    return (MOI.ScalarAffineTerm{T}[], T(x))
end

function Base.convert(
    ::Type{MOI.ScalarQuadraticFunction{T}},
    expr::MOI.ScalarNonlinearFunction,
) where {T}
    result = _collect_quadratic_terms(T, expr)
    result === nothing &&
        throw(InexactError(:convert, MOI.ScalarQuadraticFunction{T}, expr))
    aff, quad, constant = result
    return MOI.ScalarQuadraticFunction(quad, aff, T(constant))
end

# Returns `(affine_terms, quadratic_terms, constant)` or `nothing` if `expr`
# is not (at most) quadratic. Quadratic terms follow MOI's convention:
# `coefficient * x_1 * x_2` for off-diagonal, `(1/2) * coefficient * x^2` on
# the diagonal — so an `x*y` (x !== y) term stores coefficient `1`, while
# an `x^2` term stores coefficient `2`.
function _collect_quadratic_terms(
    ::Type{T},
    expr::MOI.ScalarNonlinearFunction,
) where {T}
    if expr.head == :+ && length(expr.args) == 2
        r1 = _collect_quadratic_terms(T, expr.args[1])
        r2 = _collect_quadratic_terms(T, expr.args[2])
        (r1 === nothing || r2 === nothing) && return nothing
        a1, q1, c1 = r1
        a2, q2, c2 = r2
        return (vcat(a1, a2), vcat(q1, q2), c1 + c2)
    elseif expr.head == :- && length(expr.args) == 2
        r1 = _collect_quadratic_terms(T, expr.args[1])
        r2 = _collect_quadratic_terms(T, expr.args[2])
        (r1 === nothing || r2 === nothing) && return nothing
        a1, q1, c1 = r1
        a2, q2, c2 = r2
        neg_a = [MOI.ScalarAffineTerm(-t.coefficient, t.variable) for t in a2]
        neg_q = [
            MOI.ScalarQuadraticTerm(-t.coefficient, t.variable_1, t.variable_2)
            for t in q2
        ]
        return (vcat(a1, neg_a), vcat(q1, neg_q), c1 - c2)
    elseif expr.head == :- && length(expr.args) == 1
        r = _collect_quadratic_terms(T, expr.args[1])
        r === nothing && return nothing
        a, q, c = r
        neg_a = [MOI.ScalarAffineTerm(-t.coefficient, t.variable) for t in a]
        neg_q = [
            MOI.ScalarQuadraticTerm(-t.coefficient, t.variable_1, t.variable_2)
            for t in q
        ]
        return (neg_a, neg_q, -c)
    elseif expr.head == :* && length(expr.args) == 2
        a1, a2 = expr.args
        r1 = _collect_quadratic_terms(T, a1)
        r2 = _collect_quadratic_terms(T, a2)
        (r1 === nothing || r2 === nothing) && return nothing
        af1, q1, c1 = r1
        af2, q2, c2 = r2
        # Refuse cases that would push the degree above 2.
        if !isempty(q1) && !isempty(q2)
            return nothing
        elseif !isempty(q1) && !isempty(af2)
            return nothing
        elseif !isempty(q2) && !isempty(af1)
            return nothing
        end
        # (af1 + q1 + c1) * (af2 + q2 + c2)
        aff = MOI.ScalarAffineTerm{T}[]
        quad = MOI.ScalarQuadraticTerm{T}[]
        for t in af2
            push!(aff, MOI.ScalarAffineTerm(c1 * t.coefficient, t.variable))
        end
        for t in q2
            push!(
                quad,
                MOI.ScalarQuadraticTerm(
                    c1 * t.coefficient,
                    t.variable_1,
                    t.variable_2,
                ),
            )
        end
        for t in af1
            push!(aff, MOI.ScalarAffineTerm(c2 * t.coefficient, t.variable))
        end
        for t in q1
            push!(
                quad,
                MOI.ScalarQuadraticTerm(
                    c2 * t.coefficient,
                    t.variable_1,
                    t.variable_2,
                ),
            )
        end
        for t1 in af1, t2 in af2
            coef = t1.coefficient * t2.coefficient
            v1, v2 = t1.variable, t2.variable
            if v1 == v2
                push!(quad, MOI.ScalarQuadraticTerm(T(2) * coef, v1, v2))
            else
                push!(quad, MOI.ScalarQuadraticTerm(coef, v1, v2))
            end
        end
        return (aff, quad, c1 * c2)
    elseif expr.head == :^ && length(expr.args) == 2
        base, power = expr.args
        if power == 2
            r = _collect_quadratic_terms(T, base)
            r === nothing && return nothing
            af, q, c = r
            !isempty(q) && return nothing # would be quartic
            aff = MOI.ScalarAffineTerm{T}[]
            quad = MOI.ScalarQuadraticTerm{T}[]
            for t in af
                push!(aff, MOI.ScalarAffineTerm(T(2) * c * t.coefficient, t.variable))
            end
            for t1 in af, t2 in af
                coef = t1.coefficient * t2.coefficient
                v1, v2 = t1.variable, t2.variable
                if v1 == v2
                    push!(quad, MOI.ScalarQuadraticTerm(T(2) * coef, v1, v2))
                else
                    push!(quad, MOI.ScalarQuadraticTerm(coef, v1, v2))
                end
            end
            return (aff, quad, c * c)
        elseif power == 1
            return _collect_quadratic_terms(T, base)
        else
            return nothing
        end
    else
        return nothing
    end
end

function _collect_quadratic_terms(::Type{T}, x::MOI.VariableIndex) where {T}
    return (
        [MOI.ScalarAffineTerm(one(T), x)],
        MOI.ScalarQuadraticTerm{T}[],
        zero(T),
    )
end

function _collect_quadratic_terms(::Type{T}, x::Number) where {T}
    return (
        MOI.ScalarAffineTerm{T}[],
        MOI.ScalarQuadraticTerm{T}[],
        T(x),
    )
end
