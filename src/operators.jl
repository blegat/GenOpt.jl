# Copyright (c) 2024: Benoît Legat and contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

const Iterators = Vector{Iterator}

"""
    struct ExprTemplate{E,V<:JuMP.AbstractVariableRef} <: JuMP.AbstractJuMPScalar
        expr::JuMP.GenericNonlinearExpr{V}
        iterators::Vector{Iterator}
    end

Represent a `JuMP.GenericNonlinearExpr` containing iterators.
Thanks to this custom type, we can define a custom `JuMP.build_constraint` method
to generate constraint of different types.
"""
struct ExprTemplate{E,V<:JuMP.AbstractVariableRef} <: JuMP.AbstractJuMPScalar
    expr::JuMP.GenericNonlinearExpr{V}
    iterators::Iterators
end

function ExprTemplate{E}(
    expr::JuMP.GenericNonlinearExpr{V},
    iterators::Iterators,
) where {E,V}
    return ExprTemplate{E,V}(expr, iterators)
end

function Base.convert(::Type{ExprTemplate{E,V}}, expr::ExprTemplate{F,V}) where {E,F,V}
    return ExprTemplate{E,V}(expr.expr, expr.iterators)
end

# Workaround for MA.promote_operation
function Base.zero(::Type{ExprTemplate{E,V}}) where {E,V}
    return ExprTemplate{E}(zero(JuMP.GenericNonlinearExpr{V}), Iterator[])
end

JuMP.variable_ref_type(::Type{ExprTemplate{E,V}}) where {E,V} = V

JuMP.check_belongs_to_model(f::ExprTemplate, model) = JuMP.check_belongs_to_model(f.expr, model)

"""
    struct IteratorValues{I}
        iterator::Iterator
        values::I
    end

Mapped values for `iterator`. The values at `i`th position
of the iterator after the mapping is `values[i]`.
In general, given an iterator `it::IteratorValues`, for any operator
`op`, `op(it)` is `IteratorValues(it, op.(it.values))`

## Examples

```julia-repl
julia> d = Dict(:a => 2, :b => -3)
Dict{Symbol, Int64} with 2 entries:
  :a => 2
  :b => -3

julia> i = iterator([:a, :b])
iterator([:a, :b])

julia> v = d[i]
iterator([2, -3])

julia> v^2
iterator([4, 9])

julia> 2v + 1
iterator([5, -5])
```
"""
struct IteratorValues
    iterators::Iterators
    index::IteratorIndex
    value_index::Int
end

function Base.show(io::IO, i::IteratorValues)
    print(io, "iterator(")
    print(io, getindex.(i.iterators[i.index.value].values, i.value_index))
    print(io, ")")
    return
end

function iterators(axes)
    iterators = Iterator[Iterator(tuple.(axe)) for axe in axes]
    return IteratorValues.(Ref(iterators), IteratorIndex.(eachindex(axes)), 1)
end

iterator(axe) = iterators([axe])[]

# The following is intentionally kept close to JuMP/src/nlp_expr.jl
const _ScalarWithIterator = Union{ExprTemplate,IteratorValues}

function _univariate(f, op, x)
    V = something(
        _variable_ref_type(x),
        JuMP.VariableRef, # FIXME needed if `x` is an iterator
    )
    nl = JuMP.GenericNonlinearExpr{V}(op, _expr(x))
    E = JuMP._MA.promote_operation(f, _type(x))
    return ExprTemplate{E}(nl, _iterators(x))
end

# Univariate operators
for f in MOI.Nonlinear.DEFAULT_UNIVARIATE_OPERATORS
    op = Meta.quot(f)
    if isdefined(Base, f)
        @eval function Base.$(f)(x::IteratorValues)
            return _univariate($f, $op, x)
        end
        @eval function Base.$(f)(x::ExprTemplate)
            return _univariate($f, $op, x)
        end
    end
end

function prepare(it::IteratorValues)
    return JuMP.GenericNonlinearExpr{JuMP.VariableRef}(:getindex, Any[it.index, it.value_index])
end

_expr(f::JuMP.AbstractJuMPScalar) = f
_expr(it::IteratorValues) = prepare(it)
_expr(f::ExprTemplate) = f.expr
_expr(f::Number) = f

_variable_ref_type(f::JuMP.AbstractJuMPScalar) = JuMP.variable_ref_type(f)
_variable_ref_type(::IteratorValues) = nothing
_variable_ref_type(::Number) = nothing

_iterators(::JuMP.AbstractJuMPScalar) = nothing
_iterators(it::_ScalarWithIterator) = it.iterators
_iterators(::Number) = nothing

_type(it::IteratorValues) = eltype(it.iterators[it.index.value].values[it.value_index])
_type(::ExprTemplate{E}) where {E} = E
_type(f::JuMP.AbstractJuMPScalar) = typeof(f)
_type(f::Number) = typeof(f)

_check_equal(it::Iterators, ::Nothing) = it
_check_equal(::Nothing, it::Iterators) = it
function _check_equal(a::Iterators, b::Iterators)
    #@assert a === b # reenable, workaroudn for promote_operation
    return a
end

function _multivariate(f, op, x, y)
    V = something(
        _variable_ref_type(x),
        _variable_ref_type(y),
        JuMP.VariableRef, # FIXME needed if both are iterators
    )
    nl = JuMP.GenericNonlinearExpr{V}(op, _expr(x), _expr(y))
    E = JuMP._MA.promote_operation(f, _type(x), _type(y))
    return ExprTemplate{E}(nl, _check_equal(_iterators(x), _iterators(y)))
end

# TODO move to JuMP
function JuMP._MA.promote_operation(::typeof(/), ::Type{JuMP.NonlinearExpr}, ::Type{JuMP.NonlinearExpr})
    return JuMP.NonlinearExpr
end

# Multivariate operators
for f in [:+, :-, :*, :^, :/, :atan, :min, :max]
    op = Meta.quot(f)
    @eval begin
        function Base.$(f)(x::IteratorValues, y::Number)
            return _multivariate($f, $op, x, y)
        end
        function Base.$(f)(x::Number, y::IteratorValues)
            return _multivariate($f, $op, x, y)
        end
        function Base.$(f)(x::ExprTemplate, y::Number)
            return _multivariate($f, $op, x, y)
        end
        function Base.$(f)(x::Number, y::ExprTemplate)
            return _multivariate($f, $op, x, y)
        end
        function Base.$(f)(x::_ScalarWithIterator, y::JuMP.AbstractJuMPScalar)
            return _multivariate($f, $op, x, y)
        end
        function Base.$(f)(x::JuMP.AbstractJuMPScalar, y::_ScalarWithIterator)
            return _multivariate($f, $op, x, y)
        end
        function Base.$(f)(x::_ScalarWithIterator, y::_ScalarWithIterator)
            return _multivariate($f, $op, x, y)
        end
    end
end

only_iterator(::Number) = nothing
function only_iterator(expr::JuMP.GenericNonlinearExpr)
    its = unique(filter(!isnothing, only_iterator.(expr.args)))
    if isempty(its)
        return
    else
        return its[]
    end
end

_force_value(α::Number) = α
_force_value(::IteratorIndex, v) = v

function _force_value(expr::JuMP.GenericNonlinearExpr)
    op(expr.head)(
        (force_value(e) for e in expr.args)...
    )
end

function force_value(t::ExprTemplate)
    it = only_iterator(t.expr)
    return _new_values(Base.Fix1(_force_value, t.expr), t.iterators, it)
end

function _new_values(f, iterators, index)
    iterator = iterators[index.value]
    iterators[index.value] = Iterator(map(iterator.values) do val
        (val..., f(val))
    end)
    return IteratorValues(iterators, index, length(first(iterators[index.value].values)))
end

function _getindex(d, it::IteratorValues)
    return _new_values(val -> d[val[it.value_index]], it.iterators, it.index)
end

Base.getindex(d::Dict, i::IteratorValues) = _getindex(d, i)
Base.getindex(v::Array, i::IteratorValues) = _getindex(v, i)

function Base.getindex(v::Array{V}, i::Integer, j::_ScalarWithIterator) where {V<:JuMP.AbstractVariableRef}
    nl = JuMP.GenericNonlinearExpr{V}(:getindex, to_generator(v), i, _expr(j))
    return ExprTemplate{V}(nl, _iterators(j))
end

function Base.getindex(v::Array{V}, i::_ScalarWithIterator, j::Integer) where {V<:JuMP.AbstractVariableRef}
    nl = JuMP.GenericNonlinearExpr{V}(:getindex, to_generator(v), _expr(i), j)
    return ExprTemplate{V}(nl, _iterators(i))
end
