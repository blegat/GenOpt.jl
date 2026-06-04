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
    nit = length(func.iterators)
    values = Vector{Any}(undef, nit)
    for idx in CartesianIndices(sizes)
        for k in 1:nit
            values[k] = func.iterators[k].values[idx[k]]
        end
        scalar_func = _build_function(F, func.func, values)
        ci = MOI.Utilities.normalize_and_add_constraint(
            model,
            scalar_func,
            scalar_set,
        )
        push!(constraints, ci)
    end
    return FunctionGeneratorBridge{T,F,S}(constraints)
end

# `_build_function(F, expr, values)` walks the template once and produces a
# concrete `F`. Only specialized fast paths exist — there is no generic
# `Base.convert` fallback.

function _build_function(
    ::Type{MOI.ScalarAffineFunction{T}},
    expr,
    values,
) where {T}
    out = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm{T}[], zero(T))
    _expand_affine!(out, expr, values, one(T))
    return out
end

function _build_function(
    ::Type{MOI.ScalarQuadraticFunction{T}},
    expr,
    values,
) where {T}
    out = MOI.ScalarQuadraticFunction(
        MOI.ScalarQuadraticTerm{T}[],
        MOI.ScalarAffineTerm{T}[],
        zero(T),
    )
    _expand_quadratic!(out, expr, values, one(T))
    return out
end

function _build_function(
    ::Type{MOI.ScalarNonlinearFunction},
    expr,
    values,
)
    return _expand(expr, values)
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

# --- In-place affine expansion ---
#
# `_expand_affine!(out, expr, values, coef)` walks `expr` once and pushes
# `coef * <leaf>` contributions directly into `out.terms` / `out.constant`,
# substituting `IteratorIndex` from `values` and resolving `:getindex` on
# concrete integer indices. It allocates only when `out.terms` needs to grow,
# so a caller that `sizehint!`s the vector to the final term count gets an
# allocation-free recursion.

@inline function _add_constant!(
    out::MOI.ScalarAffineFunction{T},
    c::T,
) where {T}
    out.constant += c
    return
end

function _expand_affine!(
    out::MOI.ScalarAffineFunction{T},
    expr::MOI.ScalarNonlinearFunction,
    values,
    coef::T,
) where {T}
    h = expr.head
    args = expr.args
    if h === :+
        for a in args
            _expand_affine!(out, a, values, coef)
        end
        return
    elseif h === :-
        n = length(args)
        if n == 2
            _expand_affine!(out, args[1], values, coef)
            _expand_affine!(out, args[2], values, -coef)
        elseif n == 1
            _expand_affine!(out, args[1], values, -coef)
        else
            error("Unexpected arity $n for `-`")
        end
        return
    elseif h === :* && length(args) == 2
        a1, a2 = args[1], args[2]
        c1 = _try_const(T, a1, values)
        if c1 !== nothing
            _expand_affine!(out, a2, values, coef * c1)
            return
        end
        c2 = _try_const(T, a2, values)
        if c2 !== nothing
            _expand_affine!(out, a1, values, coef * c2)
            return
        end
        throw(InexactError(:_expand_affine!, MOI.ScalarAffineFunction{T}, expr))
    elseif h === :getindex
        _expand_getindex_affine!(out, args, values, coef)
        return
    else
        c = _try_const(T, expr, values)
        if c !== nothing
            _add_constant!(out, coef * c)
            return
        end
        throw(InexactError(:_expand_affine!, MOI.ScalarAffineFunction{T}, expr))
    end
end

function _expand_affine!(
    out::MOI.ScalarAffineFunction{T},
    x::Number,
    values,
    coef::T,
) where {T}
    _add_constant!(out, coef * T(x))
    return
end

function _expand_affine!(
    out::MOI.ScalarAffineFunction{T},
    x::MOI.VariableIndex,
    values,
    coef::T,
) where {T}
    push!(out.terms, MOI.ScalarAffineTerm(coef, x))
    return
end

function _expand_affine!(
    out::MOI.ScalarAffineFunction{T},
    x::IteratorIndex,
    values,
    coef::T,
) where {T}
    return _expand_affine!(out, values[x.value], values, coef)
end

# Affine `:getindex` dispatcher. Branching on the concrete type of the
# collection lets the compiler resolve `getindex` statically (no boxing of
# the indexed atom). Common collections: `ContiguousArrayOfVariables` for
# variable arrays, `AbstractArray{<:Number}` for data lookups.
function _expand_getindex_affine!(
    out::MOI.ScalarAffineFunction{T},
    args,
    values,
    coef::T,
) where {T}
    coll = args[1]
    if coll isa ContiguousArrayOfVariables
        v = _getindex_concrete(coll, args, values)::MOI.VariableIndex
        push!(out.terms, MOI.ScalarAffineTerm(coef, v))
    elseif coll isa AbstractArray{T}
        x = _getindex_concrete(coll, args, values)::T
        out.constant += coef * x
    elseif coll isa AbstractArray{<:Number}
        x = _getindex_concrete(coll, args, values)
        out.constant += coef * T(x::Number)
    else
        atom = _resolve_getindex(args, values)
        _expand_affine!(out, atom, values, coef)
    end
    return
end

# Resolve a `:getindex` arg list with a concretely-typed collection. Taking
# `coll` as the first positional argument lets the caller dispatch (e.g.
# `coll isa ContiguousArrayOfVariables`) so the `getindex` result type is
# inferable — no boxing on the way back out.
function _getindex_concrete(coll, args, values)
    n = length(args) - 1
    if n == 1
        return getindex(coll, _to_int(args[2], values))
    elseif n == 2
        return getindex(
            coll,
            _to_int(args[2], values),
            _to_int(args[3], values),
        )
    elseif n == 3
        return getindex(
            coll,
            _to_int(args[2], values),
            _to_int(args[3], values),
            _to_int(args[4], values),
        )
    else
        idx = ntuple(k -> _to_int(args[k+1], values), n)
        return getindex(coll, idx...)
    end
end

# `coll` here is `Any` (from `args[1]`); used as the slow / fallback path.
_resolve_getindex(args, values) = _getindex_concrete(args[1], args, values)

# Resolves an `args[k]` slot to an `Int`. The `::Int` return annotation
# erases the Any returned from the type-unstable `args[k]` lookup, so the
# caller doesn't pay boxing for the result.
@inline function _to_int(x, values)::Int
    if x isa Integer
        return Int(x)
    elseif x isa AbstractFloat
        return Int(x)
    elseif x isa IteratorIndex
        return _to_int(values[x.value], values)
    elseif x isa MOI.ScalarNonlinearFunction
        v = _try_const(Float64, x, values)
        v === nothing && error("Cannot resolve `getindex` index: $x")
        return Int(v)
    else
        error("Cannot resolve `getindex` index: $x")
    end
end

# Returns `T(x)` if `x` resolves to a numeric constant (after substituting
# `IteratorIndex`es from `values`); otherwise `nothing`. Stays type-stable
# via the small `Union{T,Nothing}` return.
@inline _try_const(::Type{T}, x::Number, _) where {T} = T(x)
@inline _try_const(::Type{T}, ::MOI.VariableIndex, _) where {T} = nothing
@inline function _try_const(::Type{T}, x::IteratorIndex, values) where {T}
    v = values[x.value]
    return v isa Number ? T(v) : nothing
end
function _try_const(
    ::Type{T},
    expr::MOI.ScalarNonlinearFunction,
    values,
) where {T}
    h = expr.head
    if h === :getindex
        atom = _resolve_getindex(expr.args, values)
        return atom isa Number ? T(atom) : nothing
    end
    args = expr.args
    n = length(args)
    if h === :- && n == 1
        c = _try_const(T, args[1], values)
        return c === nothing ? nothing : -c
    elseif (h === :+ || h === :- || h === :* || h === :/) && n == 2
        c1 = _try_const(T, args[1], values)
        c1 === nothing && return nothing
        c2 = _try_const(T, args[2], values)
        c2 === nothing && return nothing
        return h === :+ ? c1 + c2 :
               h === :- ? c1 - c2 :
               h === :* ? c1 * c2 : c1 / c2
    elseif h === :^ && n == 2
        c1 = _try_const(T, args[1], values)
        c1 === nothing && return nothing
        c2 = _try_const(T, args[2], values)
        c2 === nothing && return nothing
        return c1^c2
    end
    return nothing
end

# --- In-place quadratic expansion ---
#
# Like `_expand_affine!` but the target is `ScalarQuadraticFunction`. Variable
# / variable products (and base^2) push into `out.quadratic_terms`; everything
# else pushes into `out.affine_terms` and `out.constant`. Quadratic terms
# follow MOI's convention: diagonal coefficients are stored doubled.

function _expand_quadratic!(
    out::MOI.ScalarQuadraticFunction{T},
    expr::MOI.ScalarNonlinearFunction,
    values,
    coef::T,
) where {T}
    h = expr.head
    args = expr.args
    if h === :+
        for a in args
            _expand_quadratic!(out, a, values, coef)
        end
        return
    elseif h === :-
        n = length(args)
        if n == 2
            _expand_quadratic!(out, args[1], values, coef)
            _expand_quadratic!(out, args[2], values, -coef)
        elseif n == 1
            _expand_quadratic!(out, args[1], values, -coef)
        else
            error("Unexpected arity $n for `-`")
        end
        return
    elseif h === :* && length(args) == 2
        a1, a2 = args[1], args[2]
        c1 = _try_const(T, a1, values)
        if c1 !== nothing
            _expand_quadratic!(out, a2, values, coef * c1)
            return
        end
        c2 = _try_const(T, a2, values)
        if c2 !== nothing
            _expand_quadratic!(out, a1, values, coef * c2)
            return
        end
        # Both sides have variables → product is quadratic. Materialize each
        # side as an affine form (using the fast affine path) and multiply.
        saf1 = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm{T}[], zero(T))
        _expand_affine!(saf1, a1, values, one(T))
        saf2 = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm{T}[], zero(T))
        _expand_affine!(saf2, a2, values, one(T))
        _accumulate_product!(out, saf1, saf2, coef)
        return
    elseif h === :^ && length(args) == 2 && args[2] == 2
        saf = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm{T}[], zero(T))
        _expand_affine!(saf, args[1], values, one(T))
        _accumulate_product!(out, saf, saf, coef)
        return
    elseif h === :getindex
        coll = args[1]
        if coll isa ContiguousArrayOfVariables
            v = _getindex_concrete(coll, args, values)
            push!(out.affine_terms, MOI.ScalarAffineTerm(coef, v))
            return
        elseif coll isa AbstractArray && eltype(coll) <: Number
            x = _getindex_concrete(coll, args, values)
            out.constant += coef * T(x)
            return
        else
            atom = _resolve_getindex(args, values)
            _expand_quadratic!(out, atom, values, coef)
            return
        end
    else
        c = _try_const(T, expr, values)
        if c !== nothing
            out.constant += coef * c
            return
        end
        throw(InexactError(:_expand_quadratic!, MOI.ScalarQuadraticFunction{T}, expr))
    end
end

function _expand_quadratic!(
    out::MOI.ScalarQuadraticFunction{T},
    x::Number,
    values,
    coef::T,
) where {T}
    out.constant += coef * T(x)
    return
end

function _expand_quadratic!(
    out::MOI.ScalarQuadraticFunction{T},
    x::MOI.VariableIndex,
    values,
    coef::T,
) where {T}
    push!(out.affine_terms, MOI.ScalarAffineTerm(coef, x))
    return
end

function _expand_quadratic!(
    out::MOI.ScalarQuadraticFunction{T},
    x::IteratorIndex,
    values,
    coef::T,
) where {T}
    return _expand_quadratic!(out, values[x.value], values, coef)
end

# Push `coef * (saf1.constant + Σ t1.coef*v1) * (saf2.constant + Σ t2.coef*v2)`
# into `out`'s affine + quadratic + constant slots, following MOI's diagonal-
# stored-doubled convention.
function _accumulate_product!(
    out::MOI.ScalarQuadraticFunction{T},
    saf1::MOI.ScalarAffineFunction{T},
    saf2::MOI.ScalarAffineFunction{T},
    coef::T,
) where {T}
    for t1 in saf1.terms, t2 in saf2.terms
        c = coef * t1.coefficient * t2.coefficient
        v1, v2 = t1.variable, t2.variable
        if v1 == v2
            push!(
                out.quadratic_terms,
                MOI.ScalarQuadraticTerm(T(2) * c, v1, v2),
            )
        else
            push!(out.quadratic_terms, MOI.ScalarQuadraticTerm(c, v1, v2))
        end
    end
    if !iszero(saf2.constant)
        for t in saf1.terms
            push!(
                out.affine_terms,
                MOI.ScalarAffineTerm(coef * t.coefficient * saf2.constant, t.variable),
            )
        end
    end
    if !iszero(saf1.constant)
        for t in saf2.terms
            push!(
                out.affine_terms,
                MOI.ScalarAffineTerm(coef * saf1.constant * t.coefficient, t.variable),
            )
        end
    end
    out.constant += coef * saf1.constant * saf2.constant
    return
end
