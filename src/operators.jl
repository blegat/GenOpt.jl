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
struct IteratorValues{I}
    iterators::Iterators
    index::IteratorIndex
    values::I
end

function Base.show(io::IO, i::IteratorValues)
    print(io, "iterator(")
    print(io, i.values)
    print(io, ")")
    return
end

function iterators(axes)
    iterators = Iterator[Iterator(axe) for axe in axes]
    return IteratorValues.(Ref(iterators), IteratorIndex.(eachindex(axes)), axes)
end

iterator(axe) = iterators([axe])[]

function Base.getindex(d::Dict, i::IteratorValues)
    new_values = [d[val] for val in i.values]
    i.iterators[i.index.value] = Iterator(new_values)
    return IteratorValues(i.iterators, i.index, new_values)
end

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
    @assert it.values == it.iterators[it.index.value].values
    return IteratorInExpr(it.iterators, it.index)
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

_type(it::_ScalarWithIterator) = eltype(it.values)
_type(::ExprTemplate{E}) where {E} = E
_type(f::JuMP.AbstractJuMPScalar) = typeof(f)
_type(f::Number) = typeof(f)

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
    nl = JuMP.GenericNonlinearExpr{V}(op, _expr(x), _expr(y))
    E = JuMP._MA.promote_operation(f, _type(x), _type(y))
    return ExprTemplate{E}(nl, _check_equal(_iterators(x), _iterators(y)))
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
