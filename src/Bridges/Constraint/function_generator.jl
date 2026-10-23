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
    return FunctionGeneratorBridge{T,F,S}(
        add_constraints(model, func, S(zero(T))),
    )
end

"""
    add_constraints(model, func::FunctionGenerator{F}, scalar_set)

Expand `func` into scalar constraints of its declared function type `F`, and
add them directly to `model` in Cartesian-product order. This is also the
expansion used by [`FunctionGeneratorBridge`](@ref), without requiring a
bridge optimizer. Return the vector of added constraint indices.
"""
function add_constraints(
    model::MOI.ModelLike,
    func::FunctionGenerator{F},
    scalar_set::S,
) where {F,S<:MOI.AbstractScalarSet}
    sizes = length.(func.iterators)
    constraints = Vector{MOI.ConstraintIndex{F,S}}(undef, prod(sizes))
    nit = length(func.iterators)
    positions = ones(Int, nit)
    values = Vector{Any}(undef, nit)
    for row in eachindex(constraints)
        for k in 1:nit
            # The positions stay within the lengths captured above. Inlining
            # Base's array read avoids dispatch on the erased element type.
            values[k] = @inbounds @inline getindex(
                func.iterators[k].values,
                positions[k],
            )
        end
        scalar_func = _build_function(F, func.func, values)
        ci = MOI.Utilities.normalize_and_add_constraint(
            model,
            scalar_func,
            scalar_set,
        )
        constraints[row] = ci
        # Match CartesianIndices: the first iterator changes fastest, without
        # constructing a tuple whose length becomes part of its runtime type.
        for k in 1:nit
            positions[k] += 1
            if positions[k] <= sizes[k]
                break
            end
            positions[k] = 1
        end
    end
    return constraints
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

# These leaves arrive through Vector{Any}. Keep boxed helpers generic so a
# trimmed image does not need new leaf-type specializations at runtime.
Base.@nospecializeinfer function _expand(
    @nospecialize(func::PolynomialFunction),
    values,
)
    return _expand(func.func, values)
end

const _AnySumGenerator = Union{SumGenerator,FilteredSumGenerator}

Base.@nospecializeinfer function _sum_filter(
    @nospecialize(func::SumGenerator),
    _,
)
    return true
end
Base.@nospecializeinfer function _sum_filter(
    @nospecialize(func::FilteredSumGenerator),
    values,
)::Bool
    return _expand_filter(func.filter, values, func.iterators)
end

_expand_filter(value, values) = _expand(value, values)

function _filter_iterator_position(value, iterators)::Int
    source = value.iterators[value.index.value]
    for k in eachindex(iterators)
        if iterators[k].identity === source.identity
            return k
        end
    end
    return throw(
        ArgumentError(
            "Correlated lazy-sum filters are not supported: " *
            "a filter iterator is outside the sum's domain",
        ),
    )
end

function _check_filter_scope(value, iterators)
    if value isa IteratorValues
        _filter_iterator_position(value, iterators)
    elseif value isa _Filtered
        _check_filter_scope(value.iterator, iterators)
    elseif value isa FilterExpression
        for arg in value.args
            _check_filter_scope(arg, iterators)
        end
    end
    return
end

Base.@nospecializeinfer function _check_sum_filter(
    @nospecialize(func::SumGenerator)
)
    return
end
Base.@nospecializeinfer function _check_sum_filter(
    @nospecialize(func::FilteredSumGenerator)
)
    return _check_filter_scope(func.filter, func.iterators)
end

function _expand_filter(value, values, iterators)
    iterators === nothing && return _expand_filter(value, values)
    if value isa IteratorValues
        position = _filter_iterator_position(value, iterators)
        return values[position][value.value_index]
    elseif value isa _Filtered
        return _expand_filter(value.iterator, values, iterators)
    end
    return _expand_filter(value, values)
end

function _expand_filter(filter::FilterExpression, values, iterators = nothing)
    if filter.head === :(==) && length(filter.args) == 2
        return _expand_filter(filter.args[1], values, iterators) ==
               _expand_filter(filter.args[2], values, iterators)
    end
    args = map(arg -> _expand_filter(arg, values, iterators), filter.args)
    return getfield(Base, filter.head)(args...)
end

# Each sum owns its IteratorIndex scope. Allocate its scratch arrays once,
# not once per term, and never overwrite values belonging to an outer sum.
# A callback returning false stops the traversal early.
Base.@nospecializeinfer function _foreach_sum_term(
    f::F,
    @nospecialize(func::_AnySumGenerator)
)::Bool where {F}
    _check_sum_filter(func)
    sizes = length.(func.iterators)
    nit = length(func.iterators)
    positions = ones(Int, nit)
    values = Vector{Any}(undef, nit)
    for _ in 1:prod(sizes)
        for k in 1:nit
            values[k] = @inbounds @inline getindex(
                func.iterators[k].values,
                positions[k],
            )
        end
        if _sum_filter(func, values) && !f(func.func, values)
            return false
        end
        for k in 1:nit
            positions[k] += 1
            if positions[k] <= sizes[k]
                break
            end
            positions[k] = 1
        end
    end
    return true
end

# The normal affine/quadratic walkers accumulate sums directly. This general
# expansion remains for their uncommon conversion and constant-folding paths.
Base.@nospecializeinfer function _expand(
    @nospecialize(func::_AnySumGenerator),
    _,
)
    args = Any[]
    _foreach_sum_term(func) do term, values
        push!(args, _expand(term, values))
        return true
    end
    if isempty(args)
        return 0
    elseif all(_is_numeric, args)
        return _eval_op(:+, args)
    end
    return MOI.ScalarNonlinearFunction(:+, args)
end

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
    out::Union{MOI.ScalarAffineFunction{T},MOI.ScalarQuadraticFunction{T}},
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
    body = quote
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
    # quote inserts line-number nodes pointing into the macro definition.
    # Those locations caused execution to be attributed there,
    # leaving some macro invocation lines falsely marked uncovered
    Base.remove_linenums!(body)
    return Expr(:block, __source__, body)
end

# Share the affine/quadratic leaf dispatch, expanding it inline because the
# inliner declines mutually recursive helpers and leaves boxing per level.
macro _expand_recurse(kind, out, arg, values, coef, T)
    snf = GlobalRef(@__MODULE__, Symbol("_expand_", kind, "_snf!"))
    fallback = GlobalRef(@__MODULE__, Symbol("_expand_", kind, "_fallback!"))
    terms = QuoteNode(kind === :affine ? :terms : :affine_terms)
    out, arg, values, coef, T =
        esc(out), esc(arg), esc(values), esc(coef), esc(T)
    body = quote
        let _a = $arg
            while _a isa IteratorIndex
                _a = $values[_a.value]
            end
            if _a isa MOI.ScalarNonlinearFunction
                $snf($out, _a, $values, $coef)
            elseif _a isa MOI.VariableIndex
                push!(getfield($out, $terms), MOI.ScalarAffineTerm($coef, _a))
            elseif _a isa $T
                _add_constant!($out, $coef * _a)
            elseif _a isa Int
                _add_constant!($out, $coef * $T(_a))
            elseif _a isa Number
                _add_constant!($out, $coef * $T(_a))
            else
                $fallback($out, _a, $values, $coef)
            end
        end
    end
    Base.remove_linenums!(body)
    return Expr(:block, __source__, body)
end

Base.@nospecializeinfer function _expand_affine_sum!(
    out::MOI.ScalarAffineFunction{T},
    @nospecialize(func::_AnySumGenerator),
    coef::T,
) where {T}
    _foreach_sum_term(func) do term, values
        _expand_affine!(out, term, values, coef)
        return true
    end
    return
end

Base.@nospecializeinfer function _expand_quadratic_sum!(
    out::MOI.ScalarQuadraticFunction{T},
    @nospecialize(func::_AnySumGenerator),
    coef::T,
) where {T}
    _foreach_sum_term(func) do term, values
        _expand_quadratic!(out, term, values, coef)
        return true
    end
    return
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
    @_expand_recurse affine out arg values coef T
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
            @_expand_recurse affine out a values coef T
        end
        return
    elseif h === :-
        n = length(args)
        if n == 2
            a1, a2 = args[1], args[2]
            neg_coef = -coef
            @_expand_recurse affine out a1 values coef T
            @_expand_recurse affine out a2 values neg_coef T
        elseif n == 1
            a1 = args[1]
            neg_coef = -coef
            @_expand_recurse affine out a1 values neg_coef T
        else
            error("Unexpected arity $n for `-`")
        end
        return
    elseif h === :* && length(args) == 2
        a1, a2 = args[1], args[2]
        c1 = @_try_const T a1 values
        if c1 isa T
            new_coef = coef * c1
            @_expand_recurse affine out a2 values new_coef T
            return
        end
        c2 = @_try_const T a2 values
        if c2 isa T
            new_coef = coef * c2
            @_expand_recurse affine out a1 values new_coef T
            return
        end
        _expand_affine_fallback!(out, expr, values, coef)
    elseif h === :/ && length(args) == 2
        denominator = args[2]
        constant = @_try_const T denominator values
        if constant !== nothing
            new_coef = convert(T, coef / constant)
            numerator = args[1]
            @_expand_recurse affine out numerator values new_coef T
            return
        end
        _expand_affine_fallback!(out, expr, values, coef)
    elseif h === :^ && length(args) == 2 && _is_unit_exponent(args[2])
        base = args[1]
        @_expand_recurse affine out base values coef T
        return
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

# Keep uncommon forms (including embedded MOI polynomials) out of the
# allocation-free walker. Conversion still rejects non-affine expressions.
Base.@nospecializeinfer function _expand_affine_fallback!(
    out::MOI.ScalarAffineFunction{T},
    @nospecialize(expr),
    values,
    coef::T,
) where {T}
    if expr isa PolynomialFunction
        _expand_affine_snf!(out, expr.func, values, coef)
        return
    elseif expr isa _AnySumGenerator
        _expand_affine_sum!(out, expr, coef)
        return
    end
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
    while x isa IteratorIndex
        x = values[x.value]
    end
    if x isa MOI.ScalarNonlinearFunction && x.head === :getindex
        args = x.args
        if length(args) == 2
            a1, a2 = args[1], args[2]
            if a1 isa IteratorIndex && a2 isa Int
                value = _iterator_component(values[a1.value], a2)
                return value isa Integer || value isa AbstractFloat
            end
        end
        return _integer_index_fallback(x, values)
    end
    return x isa Integer ||
           x isa AbstractFloat ||
           x isa MOI.ScalarNonlinearFunction
end

@inline function _iterator_component(value, index::Int)
    # Prepared iterators store tuples. Inline Base's field access rather than
    # dispatching on the tuple's erased element types and length.
    if value isa Tuple
        return @inline getindex(value, index)
    end
    return getindex(value, index)
end

@inline function _float_index(value)
    # Tuple components have erased types. Keep common scalar conversions
    # visible to the compiler, with the ordinary conversion as fallback.
    if value isa Float64
        return value
    elseif value isa Int
        return Float64(value)
    end
    return Float64(value)
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
Base.@nospecializeinfer @inline function _to_int(@nospecialize(x), values)::Int
    while x isa IteratorIndex
        x = values[x.value]
    end
    if x isa Int
        return x
    elseif x isa Integer
        return Int(x)
    elseif x isa AbstractFloat
        return Int(x)
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
Base.@nospecializeinfer function _eval_index(@nospecialize(x), values)::Float64
    while x isa IteratorIndex
        x = values[x.value]
    end
    if x isa Float64
        return x
    elseif x isa Int
        return Float64(x)
    elseif x isa Integer
        return Float64(x)
    elseif x isa AbstractFloat
        return Float64(x)
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
            # Keep bound-iterator access visible to a trimmed image instead
            # of dispatching `_expand` on the erased collection argument.
            v = if coll isa IteratorIndex
                value = values[coll.value]
                while value isa IteratorIndex
                    value = values[value.value]
                end
                value
            else
                _expand(coll, values)
            end
            if n == 2
                return _float_index(
                    _iterator_component(v, _to_int(args[2], values)),
                )
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
    if expr isa _AnySumGenerator
        # Fold numeric sums in the target coefficient type. `_eval_op`
        # would first float integer data, losing large-integer precision.
        out = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm{T}[], zero(T))
        _expand_affine_sum!(out, expr, one(T))
        return isempty(out.terms) ? out.constant : nothing
    end
    value = _expand(expr, values)
    return value isa Number ? T(value) : nothing
end

# This check is deliberately conservative: an unrecognized variable lookup
# may reach the general folder, but a known variable cannot be a constant.
Base.@nospecializeinfer @inline function _contains_variable(
    @nospecialize(expr),
    values,
)::Bool
    if expr isa MOI.VariableIndex
        return true
    elseif expr isa IteratorIndex
        return _contains_variable(values[expr.value], values)
    elseif expr isa PolynomialFunction
        return _contains_variable(expr.func, values)
    elseif expr isa _AnySumGenerator
        is_constant = _foreach_sum_term(expr) do term, sum_values
            return !_contains_variable(term, sum_values)
        end
        return !is_constant
    elseif expr isa MOI.ScalarAffineFunction
        return !isempty(expr.terms)
    elseif expr isa MOI.ScalarQuadraticFunction
        return !isempty(expr.affine_terms) || !isempty(expr.quadratic_terms)
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

# Like the affine walker, narrow children loaded from Vector{Any} before
# recursing, keeping ordinary polynomial operations statically dispatched.
@inline function _expand_quadratic!(
    out::MOI.ScalarQuadraticFunction{T},
    expr,
    values,
    coef::T,
) where {T}
    @_expand_recurse quadratic out expr values coef T
    return
end

function _expand_quadratic_snf!(
    out::MOI.ScalarQuadraticFunction{T},
    expr::MOI.ScalarNonlinearFunction,
    values,
    coef::T,
) where {T}
    h = expr.head
    args = expr.args
    if h === :+
        for a in args
            @_expand_recurse quadratic out a values coef T
        end
        return
    elseif h === :-
        n = length(args)
        if n == 2
            a1, a2 = args[1], args[2]
            neg_coef = -coef
            @_expand_recurse quadratic out a1 values coef T
            @_expand_recurse quadratic out a2 values neg_coef T
        elseif n == 1
            a1 = args[1]
            neg_coef = -coef
            @_expand_recurse quadratic out a1 values neg_coef T
        else
            error("Unexpected arity $n for `-`")
        end
        return
    elseif h === :* && length(args) == 2
        a1, a2 = args[1], args[2]
        c1 = @_try_const T a1 values
        if c1 !== nothing
            new_coef = coef * c1
            @_expand_recurse quadratic out a2 values new_coef T
            return
        end
        c2 = @_try_const T a2 values
        if c2 !== nothing
            new_coef = coef * c2
            @_expand_recurse quadratic out a1 values new_coef T
            return
        end
        # Both sides have variables → product is quadratic. Materialize each
        # side as an affine form (using the fast affine path) and multiply.
        saf1 = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm{T}[], zero(T))
        unit = one(T)
        @_expand_recurse affine saf1 a1 values unit T
        saf2 = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm{T}[], zero(T))
        @_expand_recurse affine saf2 a2 values unit T
        _accumulate_product!(out, saf1, saf2, coef)
        return
    elseif h === :/ && length(args) == 2
        denominator = args[2]
        constant = @_try_const T denominator values
        if constant !== nothing
            new_coef = convert(T, coef / constant)
            numerator = args[1]
            @_expand_recurse quadratic out numerator values new_coef T
            return
        end
        _expand_quadratic_fallback!(out, expr, values, coef)
    elseif h === :^ && length(args) == 2 && _is_unit_exponent(args[2])
        base = args[1]
        @_expand_recurse quadratic out base values coef T
        return
    elseif h === :^ && length(args) == 2 && _is_square_exponent(args[2])
        saf = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm{T}[], zero(T))
        unit = one(T)
        arg = args[1]
        @_expand_recurse affine saf arg values unit T
        _accumulate_product!(out, saf, saf, coef)
        return
    elseif h === :getindex
        coll = args[1]
        # Same trick as `_expand_getindex_affine!`: enumerate the concrete
        # `N` arities so the load from `Vector{Any}` is allocation-free for
        # the common cases (`N = 0, 1, 2`).
        if coll isa ContiguousArrayOfVariables{0} && length(args) == 1
            v = getindex(coll)
            push!(out.affine_terms, MOI.ScalarAffineTerm(coef, v))
            return
        elseif coll isa ContiguousArrayOfVariables{1} && length(args) == 2
            v = _getindex_concrete1(coll, args, values)
            push!(out.affine_terms, MOI.ScalarAffineTerm(coef, v))
            return
        elseif coll isa ContiguousArrayOfVariables{2} && length(args) == 3
            v = _getindex_concrete2(coll, args, values)
            push!(out.affine_terms, MOI.ScalarAffineTerm(coef, v))
            return
        elseif coll isa ContiguousArrayOfVariables
            v = _getindex_concrete(coll, args, values)
            push!(out.affine_terms, MOI.ScalarAffineTerm(coef, v))
            return
        elseif coll isa Vector{T} && length(args) == 2
            x = _getindex_concrete1(coll, args, values)
            out.constant += coef * x
            return
        elseif coll isa Matrix{T} && length(args) == 3
            x = _getindex_concrete2(coll, args, values)
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

@inline function _is_unit_exponent(value)
    # Exponents come from Vector{Any}. Narrow before comparison so a trimmed
    # image retains primitive comparisons rather than dispatching on Real.
    if value isa Int
        return value == 1
    elseif value isa Float64
        return value == 1.0
    elseif value isa Float32
        return value == 1.0f0
    end
    return value isa Real && value == 1
end

@inline function _is_square_exponent(value)
    if value isa Int
        return value == 2
    elseif value isa Float64
        return value == 2.0
    elseif value isa Float32
        return value == 2.0f0
    end
    return value isa Real && value == 2
end

Base.@nospecializeinfer function _expand_quadratic_fallback!(
    out::MOI.ScalarQuadraticFunction{T},
    @nospecialize(expr),
    values,
    coef::T,
) where {T}
    if expr isa PolynomialFunction
        _expand_quadratic_snf!(out, expr.func, values, coef)
        return
    elseif expr isa _AnySumGenerator
        _expand_quadratic_sum!(out, expr, coef)
        return
    end
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
