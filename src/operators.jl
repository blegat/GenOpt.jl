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

function Base.convert(
    ::Type{ExprTemplate{E,V}},
    expr::ExprTemplate{F,V},
) where {E,F,V}
    return ExprTemplate{E,V}(expr.expr, expr.iterators)
end

# Workaround for MA.promote_operation
function Base.zero(::Type{ExprTemplate{E,V}}) where {E,V}
    return ExprTemplate{E}(zero(JuMP.GenericNonlinearExpr{V}), Iterator[])
end

JuMP.variable_ref_type(::Type{ExprTemplate{E,V}}) where {E,V} = V

function JuMP.check_belongs_to_model(f::ExprTemplate, model)
    return JuMP.check_belongs_to_model(f.expr, model)
end

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

function _tuple(axe)
    if axe[1] isa Tuple
        return axe
    else
        return tuple.(axe)
    end
end

function iterators(axes)
    iterators = Iterator[Iterator(_tuple(axe)) for axe in axes]
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
    E = MA.promote_operation(f, _type(x))
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
    return JuMP.GenericNonlinearExpr{JuMP.VariableRef}(
        :getindex,
        Any[it.index, it.value_index],
    )
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

function _type(it::IteratorValues)
    return typeof(first(it.iterators[it.index.value].values)[it.value_index])
end
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
    if op == :^ && y == 1
        E = _type(x)
    elseif op == :^ && y == 2
        E = MA.promote_operation(*, _type(x), _type(x))
    else
        E = MA.promote_operation(f, _type(x), _type(y))
    end
    return ExprTemplate{E}(nl, _check_equal(_iterators(x), _iterators(y)))
end

# TODO move to JuMP
function MA.promote_operation(
    ::typeof(/),
    ::Type{JuMP.NonlinearExpr},
    ::Type{JuMP.NonlinearExpr},
)
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

#only_iterator(::Number) = nothing
#function only_iterator(expr::JuMP.GenericNonlinearExpr)
#    its = unique(filter(!isnothing, only_iterator.(expr.args)))
#    if isempty(its)
#        return
#    else
#        return its[]
#    end
#end
#
#_force_value(α::Number) = α
#_force_value(::IteratorIndex, v) = v
#
#function _force_value(expr::JuMP.GenericNonlinearExpr)
#    op(expr.head)(
#        (force_value(e) for e in expr.args)...
#    )
#end
#
#function force_value(t::ExprTemplate)
#    it = only_iterator(t.expr)
#    return _new_values(Base.Fix1(_force_value, t.expr), t.iterators, it)
#end

struct LazySum{E,V<:JuMP.AbstractVariableRef} <: JuMP.AbstractJuMPScalar
    expr::JuMP.GenericNonlinearExpr{V}
    iterators::Iterators
end

function LazySum(template::ExprTemplate{E,V}) where {E,V}
    return LazySum{E,V}(template.expr, template.iterators)
end

struct FilteredLazySum{E,V<:JuMP.AbstractVariableRef} <: JuMP.AbstractJuMPScalar
    expr::JuMP.GenericNonlinearExpr{V}
    iterators::Iterators
    filter::FilterExpression
end

function FilteredLazySum(template::ExprTemplate{E,V}, filter::FilterExpression) where {E,V}
    return FilteredLazySum{E,V}(template.expr, template.iterators, filter)
end

const _AnyLazySum{E,V} = Union{LazySum{E,V},FilteredLazySum{E,V}}

JuMP._is_real(::_AnyLazySum) = true
JuMP.variable_ref_type(s::_AnyLazySum) = JuMP.variable_ref_type(s.expr)
function JuMP.check_belongs_to_model(s::_AnyLazySum, model::JuMP.AbstractModel)
    return JuMP.check_belongs_to_model(s.expr, model)
end

function MA.promote_operation(
    ::Union{typeof(+),typeof(-),typeof(*),MA.AddSubMul},
    ::Type{<:JuMP._GenericAffOrQuadExpr{T,V}},
    ::Type{<:_AnyLazySum{E,V}},
) where {T,E,V}
    return JuMP.GenericNonlinearExpr{V}
end

function JuMP.moi_function(s::LazySum{E}) where {E}
    return SumGenerator{JuMP.moi_function_type(E)}(
        JuMP.moi_function(s.expr),
        s.iterators,
    )
end

function JuMP.moi_function(s::FilteredLazySum{E}) where {E}
    return FilteredSumGenerator{JuMP.moi_function_type(E)}(
        JuMP.moi_function(s.expr),
        s.iterators,
        s.filter,
    )
end

# From the code:
# `lazy_sum(... for j in 1:n if j == i)`
# we want to transform the filter into an expression graph `==(i, j)`
# We don't want to define a method `Base.:(==)(::IteratorValues, ::IteratorValues)`
# as that method might be used in other places. So we create this wrapper type
# and we allow ourself to define `Base.:(==)(::_Filtered, ::Any)`
struct _Filtered{I}
    iterator::I
end

Base.getindex(v::Array, i::_Filtered) = _Filtered(_getindex(v, i.iterator))
Base.getindex(d::Dict, i::_Filtered) = _Filtered(_getindex(d, i.iterator))

function Base.:(==)(i::_Filtered, j)
    return FilterExpression(:(==), Any[i.iterator, j])
end

# Base.Generator is slightly inconsistent:
# If there is just one iterator, f and flt are univariate and take the unique iterator
# (f(i) for i in 1:n if flt(i))
# Otherwise, it is also univariate but takes a tuple
# (f((i, j)) for i in 1:n, j in 1:m if flt((i, j)))
function _untuple_product(is_product::Bool, it)
    return is_product ? it : only(it)
end

# lazy_sum(f(i) for i in 1:n)
function _generator_iterators(it)
    return false, iterators((it,)), nothing
end
# lazy_sum(f(i, j) for i in 1:n, j in 1:m)
function _generator_iterators(it::Base.Iterators.ProductIterator)
    return true, iterators(it.iterators), nothing
end
# lazy_sum(f(i) for i in ... if flt(i))
function _generator_iterators(it::Base.Iterators.Filter)
    # We assert `::Nothing` to avoid nested filters,
    # we'll only implement it if needed
    is_product, its, _::Nothing = _generator_iterators(it.itr)
    return is_product, its, it.flt(_untuple_product(is_product, _Filtered.(its)))
end

function lazy_sum(gen::Base.Generator)
    is_product, its, filter = _generator_iterators(gen.iter)
    template = gen.f(_untuple_product(is_product, its))
    @assert template.iterators === first(its).iterators
    if isnothing(filter)
        return LazySum(template)
    else
        return FilteredLazySum(template, filter)
    end
end

function _new_values(f, iterators, index)
    iterator = iterators[index.value]
    iterators[index.value] = Iterator(map(iterator.values) do val
        return (val..., f(val))
    end)
    return IteratorValues(
        iterators,
        index,
        length(first(iterators[index.value].values)),
    )
end

function _getindex(d, it::IteratorValues)
    return _new_values(val -> d[val[it.value_index]], it.iterators, it.index)
end

Base.getindex(d::Dict, i::_ScalarWithIterator) = _getindex(d, i)
Base.getindex(v::Array, i::_ScalarWithIterator) = _getindex(v, i)

function Base.getindex(it::IteratorValues, i)
    @assert it.value_index == 1 # FIXME
    return IteratorValues(it.iterators, it.index, i)
end

function Base.getindex(
    v::Array{V},
    i::Integer,
    j::_ScalarWithIterator,
) where {V<:JuMP.AbstractVariableRef}
    nl = JuMP.GenericNonlinearExpr{V}(:getindex, to_generator(v), i, _expr(j))
    return ExprTemplate{V}(nl, _iterators(j))
end

function Base.getindex(
    v::Array{V},
    i::_ScalarWithIterator,
) where {V<:JuMP.AbstractVariableRef}
    nl = JuMP.GenericNonlinearExpr{V}(:getindex, to_generator(v), _expr(i))
    return ExprTemplate{V}(nl, _iterators(i))
end

function Base.getindex(
    v::Array{V},
    i::_ScalarWithIterator,
    j::Integer,
) where {V<:JuMP.AbstractVariableRef}
    nl = JuMP.GenericNonlinearExpr{V}(:getindex, to_generator(v), _expr(i), j)
    return ExprTemplate{V}(nl, _iterators(i))
end

function Base.getindex(
    v::Array{V},
    i::_ScalarWithIterator,
    j::_ScalarWithIterator,
) where {V<:JuMP.AbstractVariableRef}
    nl = JuMP.GenericNonlinearExpr{V}(
        :getindex,
        to_generator(v),
        _expr(i),
        _expr(j),
    )
    return ExprTemplate{V}(nl, _check_equal(_iterators(i), _iterators(j)))
end
