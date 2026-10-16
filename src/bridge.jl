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
# concrete `F`. Affine and quadratic templates use specialized builders;
# other function types retain the general expansion-and-conversion path.
# Iterator values need not have coefficient type T: in `i * x[i]`, integer
# `i` must become T as a coefficient but remain an integer as an index.

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

# Covers VariableIndex and ScalarNonlinearFunction, including folded constants.
function _build_function(::Type{F}, expr, values) where {F}
    return convert(F, _expand(expr, values))
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
    # MOI evaluates in the argument type, so operations like sin need floating
    # inputs. Promote first to preserve Float32 or BigFloat operand precision.
    float_args = isempty(args) ? Float64[] : collect(float.(promote(args...)))
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

# `@_try_const T x values` expands to `_try_const(T, x, values)`'s isa-chain
# inline at the call site, returning `nothing` or a `T`. Inlining is required
# because the helper would otherwise pay one box per call when `x` is `Any`
# (from `Vector{Any}`) and dispatches into the SNF branch.
macro _try_const(T, x, values)
    T, x, values = esc(T), esc(x), esc(values)
    return quote
        let _x = $x
            if _x isa $T
                _x
            elseif _x isa Int
                $T(_x)
            elseif _x isa MOI.VariableIndex
                nothing
            elseif _x isa IteratorIndex
                _v = $values[_x.value]
                _v isa $T ? _v :
                (_v isa Int ? $T(_v) : _try_const_fallback($T, _v, $values))
            elseif _x isa MOI.ScalarNonlinearFunction
                _try_const_snf($T, _x, $values)
            else
                _try_const_fallback($T, _x, $values)
            end
        end
    end
end

# `@_expand_affine_recurse out arg values coef T` expands inline at the
# call site to the same isa-chain as `_expand_affine_typed_arg!`. We use a
# macro (rather than an `@inline` helper) because Julia's inliner declines
# to inline the helper when it's mutually recursive with `_expand_affine_snf!`,
# leaving 16 B of function-call overhead per recursion level.
macro _expand_affine_recurse(out, arg, values, coef, T)
    out, arg, values, coef, T =
        esc(out), esc(arg), esc(values), esc(coef), esc(T)
    return quote
        let _a = $arg
            if _a isa MOI.ScalarNonlinearFunction
                _expand_affine_snf!($out, _a, $values, $coef)
            elseif _a isa MOI.VariableIndex
                push!($out.terms, MOI.ScalarAffineTerm($coef, _a))
            elseif _a isa IteratorIndex
                _expand_affine_typed_arg!(
                    $out,
                    $values[_a.value],
                    $values,
                    $coef,
                )
            elseif _a isa $T
                _add_constant!($out, $coef * _a)
            elseif _a isa Int
                _add_constant!($out, $coef * $T(_a))
            elseif _a isa Number
                _add_constant!($out, $coef * $T(_a))
            else
                _expand_affine_fallback!($out, _a, $values, $coef)
            end
        end
    end
end

# Entry point. `arg` is typically read out of `Vector{Any}` (so its static
# type is `Any`). Multi-method dispatch on `arg` would pay a per-call
# allocation for the dispatch tuple; using a single method with a manual
# `isa` chain keeps the recursion statically resolved and allocation-free.
@inline function _expand_affine!(
    out::MOI.ScalarAffineFunction{T},
    arg,
    values,
    coef::T,
) where {T}
    if arg isa MOI.ScalarNonlinearFunction
        _expand_affine_snf!(out, arg::MOI.ScalarNonlinearFunction, values, coef)
    elseif arg isa MOI.VariableIndex
        push!(out.terms, MOI.ScalarAffineTerm(coef, arg::MOI.VariableIndex))
    elseif arg isa IteratorIndex
        _expand_affine!(out, values[(arg::IteratorIndex).value], values, coef)
    elseif arg isa T
        _add_constant!(out, coef * (arg::T))
    elseif arg isa Int
        _add_constant!(out, coef * T(arg::Int))
    elseif arg isa Number
        _add_constant!(out, coef * T(arg::Number))
    else
        _expand_affine_fallback!(out, arg, values, coef)
    end
    return
end

function _expand_affine_snf!(
    out::MOI.ScalarAffineFunction{T},
    expr::MOI.ScalarNonlinearFunction,
    values,
    coef::T,
) where {T}
    h = expr.head
    args = expr.args
    if h === :+
        for a in args
            @_expand_affine_recurse out a values coef T
        end
        return
    elseif h === :-
        n = length(args)
        if n == 2
            a1, a2 = args[1], args[2]
            neg_coef = -coef
            @_expand_affine_recurse out a1 values coef T
            @_expand_affine_recurse out a2 values neg_coef T
        elseif n == 1
            a1 = args[1]
            neg_coef = -coef
            @_expand_affine_recurse out a1 values neg_coef T
        else
            error("Unexpected arity $n for `-`")
        end
        return
    elseif h === :* && length(args) == 2
        a1, a2 = args[1], args[2]
        c1 = @_try_const T a1 values
        if c1 isa T
            new_coef = coef * c1
            @_expand_affine_recurse out a2 values new_coef T
            return
        end
        c2 = @_try_const T a2 values
        if c2 isa T
            new_coef = coef * c2
            @_expand_affine_recurse out a1 values new_coef T
            return
        end
        _expand_affine_fallback!(out, expr, values, coef)
    elseif h === :getindex
        _expand_getindex_affine!(out, args, values, coef)
        return
    else
        c = _try_const(T, expr, values)
        if c !== nothing
            _add_constant!(out, coef * c)
            return
        end
        _expand_affine_fallback!(out, expr, values, coef)
    end
end

# Internal recursion helper for SNF children. Mirrors the entry `_expand_affine!`
# isa-chain but tail-calls back into `_expand_affine_snf!` directly when the
# child is a `ScalarNonlinearFunction`, so the compiler keeps the dispatch
# fully static (no allocation per recursive descent).
@inline function _expand_affine_arg!(
    out::MOI.ScalarAffineFunction{T},
    arg,
    values,
    coef::T,
) where {T}
    return _expand_affine_typed_arg!(out, arg, values, coef)
end

# `_expand_affine_typed_arg!` is the common dispatch helper used by the SNF
# expander to recurse into children. Splitting it from `_expand_affine_arg!`
# (which is the public entry) lets the SNF expander call into it with the
# child value typed `Any` *without* an extra function-call hop — the
# `@inline` is honored inside the SNF expander because there's no recursion
# of `_expand_affine_arg!` calling itself. For SNF children we go straight
# to `_expand_affine_snf!` so each `:*` / `:+` level adds zero allocations.
@inline function _expand_affine_typed_arg!(
    out::MOI.ScalarAffineFunction{T},
    arg,
    values,
    coef::T,
) where {T}
    if arg isa MOI.ScalarNonlinearFunction
        _expand_affine_snf!(out, arg, values, coef)
    elseif arg isa MOI.VariableIndex
        push!(out.terms, MOI.ScalarAffineTerm(coef, arg))
    elseif arg isa IteratorIndex
        _expand_affine_typed_arg!(out, values[arg.value], values, coef)
    elseif arg isa T
        _add_constant!(out, coef * arg)
    elseif arg isa Int
        _add_constant!(out, coef * T(arg))
    elseif arg isa Number
        _add_constant!(out, coef * T(arg))
    else
        _expand_affine_fallback!(out, arg, values, coef)
    end
    return
end

# Keep uncommon forms (including embedded MOI polynomials) out of the
# allocation-free walker. Conversion still rejects non-affine expressions.
function _expand_affine_fallback!(
    out::MOI.ScalarAffineFunction{T},
    expr,
    values,
    coef::T,
) where {T}
    f = convert(MOI.ScalarAffineFunction{T}, _expand(expr, values))
    for term in f.terms
        push!(
            out.terms,
            MOI.ScalarAffineTerm(coef * term.coefficient, term.variable),
        )
    end
    out.constant += coef * f.constant
    return
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
    # Narrowing to the *parametric* `ContiguousArrayOfVariables` is not
    # enough to fully unbox `coll` out of `args::Vector{Any}` — Julia needs
    # `N` (the arity) to know the struct's size. The branches below pair a
    # concrete-`N` narrowing with the matching arity-specific getindex helper
    # so each branch has a single concrete return type (no `Union` boxing).
    # `N = 0, 1, 2` are enumerated; higher arities pay one box per call.
    if coll isa ContiguousArrayOfVariables{0} && length(args) == 1
        v = getindex(coll)
        push!(out.terms, MOI.ScalarAffineTerm(coef, v))
    elseif coll isa ContiguousArrayOfVariables{1} && length(args) == 2
        v = _getindex_concrete1(coll, args, values)
        push!(out.terms, MOI.ScalarAffineTerm(coef, v))
    elseif coll isa ContiguousArrayOfVariables{2} && length(args) == 3
        v = _getindex_concrete2(coll, args, values)
        push!(out.terms, MOI.ScalarAffineTerm(coef, v))
    elseif coll isa ContiguousArrayOfVariables
        v = _getindex_concrete(coll, args, values)
        push!(out.terms, MOI.ScalarAffineTerm(coef, v))
    elseif coll isa Vector{T} && length(args) == 2
        x = _getindex_concrete1(coll, args, values)
        out.constant += coef * x
    elseif coll isa Matrix{T} && length(args) == 3
        x = _getindex_concrete2(coll, args, values)
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

# Resolve a `:getindex` arg list with a concretely-typed collection. The
# caller dispatches on `length(args)` so each leaf method has a single
# concrete return type (otherwise the union over arities makes Julia box
# the result).
@inline function _getindex_concrete(coll, args, values)
    n = length(args) - 1
    if n == 1
        return _getindex_concrete1(coll, args, values)
    elseif n == 2
        return _getindex_concrete2(coll, args, values)
    elseif n == 3
        return _getindex_concrete3(coll, args, values)
    else
        return _getindex_concreteN(coll, args, values)
    end
end

@inline function _integer_index(x, values)
    if x isa IteratorIndex
        return _integer_index(values[x.value], values)
    elseif x isa MOI.ScalarNonlinearFunction && x.head === :getindex
        args = x.args
        if length(args) == 2 && args[1] isa IteratorIndex && args[2] isa Int
            value = getindex(values[args[1].value], args[2])
            return value isa Integer || value isa AbstractFloat
        end
        return _integer_index_fallback(x, values)
    end
    return x isa Integer ||
           x isa AbstractFloat ||
           x isa MOI.ScalarNonlinearFunction
end

@noinline function _integer_index_fallback(x, values)
    value = _expand(x, values)
    return value isa Integer || value isa AbstractFloat
end

@inline function _getindex_concrete1(coll, args, values)
    if !_integer_index(args[2], values)
        return _getindex_scalar_fallback(coll, args, values)
    end
    return getindex(coll, _to_int(args[2], values))
end

@inline function _getindex_concrete2(coll, args, values)
    if !_integer_index(args[2], values) || !_integer_index(args[3], values)
        return _getindex_scalar_fallback(coll, args, values)
    end
    return getindex(coll, _to_int(args[2], values), _to_int(args[3], values))
end

@inline function _getindex_concrete3(coll, args, values)
    if any(k -> !_integer_index(args[k], values), 2:4)
        return _getindex_scalar_fallback(coll, args, values)
    end
    return getindex(
        coll,
        _to_int(args[2], values),
        _to_int(args[3], values),
        _to_int(args[4], values),
    )
end

function _getindex_concreteN(coll, args, values)
    n = length(args) - 1
    if any(k -> !_integer_index(args[k], values), 2:length(args))
        return _getindex_scalar_fallback(coll, args, values)
    end
    idx = ntuple(k -> _to_int(args[k+1], values), n)
    return getindex(coll, idx...)
end

# Uncommon collections may themselves be templates, and their keys need not
# be integers (for example, a dictionary indexed by an iterator of strings).
function _resolve_getindex(args, values)
    coll = _expand(args[1], values)
    return _getindex_fallback(coll, args, values)
end

function _getindex_fallback(coll, args, values)
    indices = [_to_index(_expand(arg, values)) for arg in args[2:end]]
    return getindex(coll, indices...)
end

# The specialized array paths expect one scalar element. Keep their inferred
# result concrete even when a Cartesian index requires general expansion.
@noinline function _getindex_scalar_fallback(
    coll::C,
    args,
    values,
)::eltype(C) where {C}
    return _getindex_fallback(coll, args, values)
end

# Resolves an `args[k]` slot to an `Int`. The `::Int` return annotation
# erases the Any returned from the type-unstable `args[k]` lookup, so the
# caller doesn't pay boxing for the result. Index expressions may include
# small arithmetic like `IteratorIndex(1) + 1`; we fold those here via
# `_eval_index` rather than reusing `_try_const` (whose recursion would
# widen the affine hot path's inferred return type to `Any`).
@inline function _to_int(x, values)::Int
    if x isa Integer
        return Int(x)
    elseif x isa AbstractFloat
        return Int(x)
    elseif x isa IteratorIndex
        return _to_int(values[x.value], values)
    elseif x isa MOI.ScalarNonlinearFunction
        return Int(_eval_index(x, values))
    else
        error("Cannot resolve `getindex` index of type ", typeof(x))
    end
end

# Recursive folder for index arithmetic. Returns a `Float64` (so we can
# represent `÷` / `/` outputs cleanly) and errors on anything that doesn't
# fold to a constant. Kept separate from `_try_const_snf` because this
# variant's recursion is fine here — `_to_int` is only called from the
# `:getindex` path, not from the affine expression hot path.
function _eval_index(x, values)::Float64
    if x isa Integer
        return Float64(x)
    elseif x isa AbstractFloat
        return Float64(x)
    elseif x isa IteratorIndex
        return _eval_index(values[x.value], values)
    elseif x isa MOI.ScalarNonlinearFunction
        h = x.head
        args = x.args
        n = length(args)
        if h === :- && n == 1
            return -_eval_index(args[1], values)
        elseif h === :+ && n == 2
            return _eval_index(args[1], values) + _eval_index(args[2], values)
        elseif h === :- && n == 2
            return _eval_index(args[1], values) - _eval_index(args[2], values)
        elseif h === :* && n == 2
            return _eval_index(args[1], values) * _eval_index(args[2], values)
        elseif h === :/ && n == 2
            return _eval_index(args[1], values) / _eval_index(args[2], values)
        elseif h === :^ && n == 2
            return _eval_index(args[1], values)^_eval_index(args[2], values)
        elseif h === :getindex && n >= 2
            for k in 2:n
                if !_integer_index(args[k], values)
                    return Float64(_resolve_getindex(args, values))
                end
            end
            # The JuMP wrapper's `prepare(it::IteratorValues)` wraps each
            # iterator reference as `:getindex(IteratorIndex(k), value_index)`
            # because the iterator's stored values are tuples (see
            # `operators.jl::_tuple` / `prepare`). Resolve such inner
            # `:getindex` nodes here so index expressions like `k + 1` work
            # under the JuMP-built path.
            coll = args[1]
            v = _expand(coll, values)
            if n == 2
                return Float64(getindex(v, _to_int(args[2], values)))
            elseif n == 3
                return Float64(
                    getindex(
                        v,
                        _to_int(args[2], values),
                        _to_int(args[3], values),
                    ),
                )
            else
                idx = ntuple(k -> _to_int(args[k+1], values), n - 1)
                return Float64(getindex(v, idx...))
            end
        else
            value = _expand(x, values)
            if value isa Number
                return Float64(value)
            end
            error(
                "Cannot resolve `getindex` index: ScalarNonlinearFunction(:",
                h,
                ", ",
                n,
                " args)",
            )
        end
    else
        error("Cannot resolve `getindex` index of type ", typeof(x))
    end
end

# Returns `T(x)` if `x` resolves to a numeric constant (after substituting
# `IteratorIndex`es from `values`); otherwise `nothing`. Stays type-stable
# via the small `Union{T,Nothing}` return. Single-method + manual `isa`
# chain to keep callers free of dynamic-dispatch allocations.
function _try_const(::Type{T}, x, values) where {T}
    # Narrow to the *concrete* type `T` first. For T = Float64 and a literal
    # 2.0 stored as `Any`, this avoids the dynamic-dispatch `T(::Number)`
    # conversion path (~32 B / call). Every branch returns either `T` or
    # `nothing`, so Julia infers `Union{T,Nothing}` and the caller's
    # `c isa T` check stays unboxed. We avoid the abstract `isa Number`
    # branch (its `T(::Number)` conversion widens the inferred return to
    # `Any` and re-introduces ~16 B/call). Uncommon types are handled behind
    # a typed, non-inlined fallback instead.
    if x isa T
        return x
    elseif x isa Int
        return T(x)
    elseif x isa MOI.VariableIndex
        return nothing
    elseif x isa IteratorIndex
        v = values[x.value]
        if v isa T
            return v
        elseif v isa Int
            return T(v)
        else
            return _try_const_fallback(T, v, values)
        end
    elseif x isa MOI.ScalarNonlinearFunction
        return _try_const_snf(T, x, values)
    else
        return _try_const_fallback(T, x, values)
    end
end

@inline function _try_const_snf(
    ::Type{T},
    expr::MOI.ScalarNonlinearFunction,
    values,
) where {T}
    # Handle the common `data_array[idx]` lookup pattern. Each branch returns
    # either `T` or `nothing`, so Julia infers the function's return type as
    # `Union{T,Nothing}` and the caller's `c isa T` check stays unboxed.
    # General constant folding stays behind a typed, non-inlined boundary:
    # recursive inference here would introduce boxing on the hot path.
    if expr.head === :getindex
        coll = expr.args[1]
        if coll isa Vector{T} && length(expr.args) == 2
            return _getindex_concrete1(coll, expr.args, values)
        elseif coll isa Matrix{T} && length(expr.args) == 3
            return _getindex_concrete2(coll, expr.args, values)
        elseif coll isa ContiguousArrayOfVariables
            return nothing
        end
    end
    return _try_const_fallback(T, expr, values)
end

@noinline function _try_const_fallback(
    ::Type{T},
    expr,
    values,
)::Union{T,Nothing} where {T}
    # Do not materialize a substituted tree merely to discover that a common
    # polynomial contains variables. In particular, products of affine forms
    # must keep using the fast quadratic builder without extra allocations.
    if _contains_variable(expr, values)
        return nothing
    end
    value = _expand(expr, values)
    return value isa Number ? T(value) : nothing
end

# This check is deliberately conservative: an unrecognized variable lookup
# may reach the general folder, but a known variable cannot be a constant.
function _contains_variable(expr, values)
    if expr isa MOI.VariableIndex
        return true
    elseif expr isa IteratorIndex
        return _contains_variable(values[expr.value], values)
    elseif expr isa MOI.ScalarNonlinearFunction
        if expr.head === :getindex &&
           expr.args[1] isa ContiguousArrayOfVariables
            return true
        end
        for arg in expr.args
            if _contains_variable(arg, values)
                return true
            end
        end
    end
    return false
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
        # Same trick as `_expand_getindex_affine!`: enumerate the concrete
        # `N` arities so the load from `Vector{Any}` is allocation-free for
        # the common cases (`N = 0, 1, 2`).
        if coll isa ContiguousArrayOfVariables{0}
            v = _getindex_concrete(coll, args, values)
            push!(out.affine_terms, MOI.ScalarAffineTerm(coef, v))
            return
        elseif coll isa ContiguousArrayOfVariables{1}
            v = _getindex_concrete(coll, args, values)
            push!(out.affine_terms, MOI.ScalarAffineTerm(coef, v))
            return
        elseif coll isa ContiguousArrayOfVariables{2}
            v = _getindex_concrete(coll, args, values)
            push!(out.affine_terms, MOI.ScalarAffineTerm(coef, v))
            return
        elseif coll isa ContiguousArrayOfVariables
            v = _getindex_concrete(coll, args, values)
            push!(out.affine_terms, MOI.ScalarAffineTerm(coef, v))
            return
        elseif coll isa Vector{T}
            x = _getindex_concrete(coll, args, values)
            out.constant += coef * x
            return
        elseif coll isa Matrix{T}
            x = _getindex_concrete(coll, args, values)
            out.constant += coef * x
            return
        elseif coll isa AbstractArray{<:Number}
            x = _getindex_concrete(coll, args, values)
            out.constant += coef * T(x::Number)
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
        _expand_quadratic_fallback!(out, expr, values, coef)
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

function _expand_quadratic!(
    out::MOI.ScalarQuadraticFunction{T},
    expr,
    values,
    coef::T,
) where {T}
    return _expand_quadratic_fallback!(out, expr, values, coef)
end

function _expand_quadratic_fallback!(
    out::MOI.ScalarQuadraticFunction{T},
    expr,
    values,
    coef::T,
) where {T}
    f = convert(MOI.ScalarQuadraticFunction{T}, _expand(expr, values))
    for term in f.quadratic_terms
        push!(
            out.quadratic_terms,
            MOI.ScalarQuadraticTerm(
                coef * term.coefficient,
                term.variable_1,
                term.variable_2,
            ),
        )
    end
    for term in f.affine_terms
        push!(
            out.affine_terms,
            MOI.ScalarAffineTerm(coef * term.coefficient, term.variable),
        )
    end
    out.constant += coef * f.constant
    return
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
                MOI.ScalarAffineTerm(
                    coef * t.coefficient * saf2.constant,
                    t.variable,
                ),
            )
        end
    end
    if !iszero(saf1.constant)
        for t in saf2.terms
            push!(
                out.affine_terms,
                MOI.ScalarAffineTerm(
                    coef * saf1.constant * t.coefficient,
                    t.variable,
                ),
            )
        end
    end
    out.constant += coef * saf1.constant * saf2.constant
    return
end
