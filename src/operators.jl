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
"""
struct ExprTemplate{E,V<:JuMP.AbstractVariableRef} <: JuMP.AbstractJuMPScalar
    expr::JuMP.GenericNonlinearExpr{V}
    iterators::Iterators
end

function ExprTemplate{E}(expr::JuMP.GenericNonlinearExpr{V}, iterators::Iterators) where {E,V}
    return ExprTemplate{E,V}(expr, iterators)
end

JuMP.variable_ref_type(::Type{ExprTemplate{E,V}}) where {E,V} = V

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
struct IteratorValues{I}
    iterators::Iterators
    index::Int
    values::I
end

function Base.show(io::IO, i::IteratorValues)
    print(io, "iterator(")
    print(io, i.values)
    print(io, ")")
    return
end

function iterators(axes)
    iterators = [Iterator(length(axe)) for axe in axes]
    return IteratorValues.(Ref(iterators), eachindex(axes), axes)
end

iterator(axe) = iterators([axe])[]

function Base.getindex(d::Dict, i::IteratorValues)
    return IteratorValues(i.iterators, i.index, [d[val] for val in i.values])
end

# The following is intentionally kept close to JuMP/src/nlp_expr.jl
const _ScalarWithIterator = Union{ExprTemplate,IteratorValues}

# Univariate operators
for f in MOI.Nonlinear.DEFAULT_UNIVARIATE_OPERATORS
    if isdefined(Base, f)
        @eval function Base.$(f)(x::IteratorValues)
            return IteratorValues(x.iterators, x.index, $(f).(x.values))
        end
        @eval function Base.$(f)(x::ExprTemplate)
            return ExprTemplate($f(x.expr), x.iterators)
        end
    end
end

function prepare(it::IteratorValues)
    append!(it.iterators[it.index].values, it.values)
    index = IteratorIndex(it.index, num_values(it.iterators[it.index]))
    return IteratorInExpr(it.iterators, index)
end

_expr(f::JuMP.AbstractJuMPScalar) = f
_expr(it::IteratorValues) = prepare(it)
_expr(f::ExprTemplate) = f.expr

_variable_ref_type(f::JuMP.AbstractJuMPScalar) = JuMP.variable_ref_type(f)
_variable_ref_type(::IteratorValues) = nothing

_iterators(::JuMP.AbstractJuMPScalar) = nothing
_iterators(it::_ScalarWithIterator) = it.iterators

_type(it::_ScalarWithIterator) = eltype(it.values)
_type(::ExprTemplate{E}) where {E} = E
_type(f::JuMP.AbstractJuMPScalar) = typeof(f)

_check_equal(it::Iterators, ::Nothing) = it
_check_equal(::Nothing, it::Iterators) = it
function _check_equal(a::Iterators, b::Iterators)
    @assert a === b
    return a
end

function _multivariate(f, op, x, y)
    V = something(
        _variable_ref_type(x),
        _variable_ref_type(y),
        JuMP.VariableRef, # FIXME needed if both are iterators
    )
    nl = JuMP.GenericNonlinearExpr{V}(
        op,
        _expr(x),
        _expr(y),
    )
    E = JuMP._MA.promote_operation(f, _type(x), _type(y))
    return ExprTemplate{E}(
        nl,
        _check_equal(_iterators(x), _iterators(y))
    )
end

# Multivariate operators
for f in [:+, :-, :*, :^, :/, :atan, :min, :max]
    op = Meta.quot(f)
    @eval begin
        function Base.$(f)(x::IteratorValues, y::Number)
            return IteratorValues(x.iterators, x.index, $(f).(x.values, y))
        end
        function Base.$(f)(x::Number, y::IteratorValues)
            return IteratorValues(y.iterators, y.index, $(f).(x, y.values))
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
