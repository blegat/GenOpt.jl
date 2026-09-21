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
    # axe can also be `keys(dict)`
    if !isempty(axe) && first(axe) isa Tuple
        return axe
    end
    return tuple.(axe)
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

_reindex(expr, _) = expr
function _reindex(index::IteratorIndex, positions)
    return IteratorIndex(positions[index.value])
end
function _reindex(expr::JuMP.GenericNonlinearExpr{V}, positions) where {V}
    return JuMP.GenericNonlinearExpr{V}(
        expr.head,
        Any[_reindex(arg, positions) for arg in expr.args],
    )
end

function _compatible_mappings(a::Iterator, b::Iterator)
    return length(a) == length(b) && all(zip(a.values, b.values)) do (x, y)
        return all(k -> isequal(x[k], y[k]), 1:min(length(x), length(y)))
    end
end

function _merge_exprs(x, y)
    a, b = _iterators(x), _iterators(y)
    if isnothing(a) || a === b
        return _expr(x), _expr(y), b
    elseif isnothing(b)
        return _expr(x), _expr(y), a
    end
    iterators = Iterator[]
    positions = IdDict{Base.RefValue{Nothing},Int}()
    function append_iterator(iterator)
        position = get!(positions, iterator.identity) do
            push!(iterators, iterator)
            return length(iterators)
        end
        previous = iterators[position]
        if previous !== iterator &&
           previous.values !== iterator.values &&
           !_compatible_mappings(previous, iterator)
            throw(
                ArgumentError(
                    "Cannot combine divergent mappings of the same iterator",
                ),
            )
        end
        if iterator.generation > iterators[position].generation
            iterators[position] = iterator
        end
        return position
    end
    a_positions = map(append_iterator, a)
    b_positions = map(append_iterator, b)
    return _reindex(_expr(x), a_positions),
    _reindex(_expr(y), b_positions),
    iterators
end

function _multivariate(f, op, x, y)
    V = something(
        _variable_ref_type(x),
        _variable_ref_type(y),
        JuMP.VariableRef, # FIXME needed if both are iterators
    )
    x_expr, y_expr, iterators = _merge_exprs(x, y)
    if op == :^ && y == 1
        E = _type(x)
    elseif op == :^ && y == 2
        E = MA.promote_operation(*, _type(x), _type(x))
    else
        E = MA.promote_operation(f, _type(x), _type(y))
    end
    if E <: JuMP.AbstractJuMPScalar
        T = JuMP.value_type(V)
        x_expr = x_expr isa Real ? convert(T, x_expr) : x_expr
        # Preserve integer exponents, including the quadratic case above.
        y_expr = y_expr isa Real && op != :^ ? convert(T, y_expr) : y_expr
    end
    nl = JuMP.GenericNonlinearExpr{V}(op, x_expr, y_expr)
    return ExprTemplate{E}(nl, iterators)
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

function FilteredLazySum(
    template::ExprTemplate{E,V},
    filter::FilterExpression,
) where {E,V}
    return FilteredLazySum{E,V}(template.expr, template.iterators, filter)
end

const _AnyLazySum{E,V} = Union{LazySum{E,V},FilteredLazySum{E,V}}

JuMP._is_real(::_AnyLazySum) = true
JuMP.variable_ref_type(s::_AnyLazySum) = JuMP.variable_ref_type(s.expr)

# `JuMP.:+(::AbstractJuMPScalar, ::AbstractJuMPScalar)` calls `iszero` which
# defaults to `x == zero(x)`. A `LazySum` is never simplified away so we
# short-circuit it here, this avoids needing `zero(::Type{<:LazySum})`.
Base.iszero(::_AnyLazySum) = false
JuMP.owner_model(s::_AnyLazySum) = JuMP.owner_model(s.expr)

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

function JuMP.jump_function_type(
    model::JuMP.AbstractModel,
    ::Type{SumGenerator{F}},
) where {F}
    return LazySum{
        JuMP.jump_function_type(model, F),
        JuMP.variable_ref_type(model),
    }
end

function JuMP.moi_function(s::FilteredLazySum{E}) where {E}
    return FilteredSumGenerator{JuMP.moi_function_type(E)}(
        JuMP.moi_function(s.expr),
        s.iterators,
        s.filter,
    )
end

function JuMP.jump_function_type(
    model::JuMP.AbstractModel,
    ::Type{FilteredSumGenerator{F}},
) where {F}
    return FilteredLazySum{
        JuMP.jump_function_type(model, F),
        JuMP.variable_ref_type(model),
    }
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
    # (typed `_` destructuring would need Julia 1.12; we support 1.10)
    is_product, its, inner_filter = _generator_iterators(it.itr)
    inner_filter::Nothing
    return is_product,
    its,
    it.flt(_untuple_product(is_product, _Filtered.(its)))
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
    values = map(iterator.values) do val
        return (val..., f(val))
    end
    iterators[index.value] =
        Iterator(values, iterator.identity, iterator.generation + 1)
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

_push_indices!(indices, _) = indices
_push_indices!(indices, i::IteratorIndex) = push!(indices, i)
function _push_indices!(indices, e::JuMP.GenericNonlinearExpr)
    for arg in e.args
        _push_indices!(indices, arg)
    end
    return indices
end

# `eval_*_function` evaluates to a `Float64`, so an arithmetic index like `i + 1`
# comes back as `4.0`: convert it back for array indexing (`Dict` lookup works
# by `hash`, for which `4.0` and `4` are equal, so no conversion is needed there)
_data_index(::Array, key) = _to_index(key)
_data_index(::Dict, key) = key

# Indexing a data collection by a computed iterator expression (e.g. `v[i + 1]`):
# the expression contains no decision variable, so it is evaluated at each value
# of the iterator and the result is looked up eagerly, appending it to the
# iterator values like `getindex(::Array, ::IteratorValues)` does.
function _getindex(d, t::ExprTemplate)
    indices = unique!(_push_indices!(IteratorIndex[], t.expr))
    if length(indices) > 1
        return _deferred_getindex(d, t)
    end
    index = only(indices)
    return _new_values(t.iterators, index) do val
        values = ntuple(k -> k == index.value ? val : (), length(t.iterators))
        return d[_data_index(d, index_iterators(t.expr, values))]
    end
end

struct _DataArray{T,N,A<:AbstractArray{T,N}} <: AbstractArray{T,N}
    data::A
end

Base.size(array::_DataArray) = size(array.data)
Base.getindex(array::_DataArray, indices...) = getindex(array.data, indices...)
JuMP._is_real(::_DataArray) = true
JuMP.moi_function(array::_DataArray) = array
JuMP.jump_function(_, array::_DataArray) = array

# A lookup involving several domains cannot be appended to one iterator's
# values. Keep it symbolic until the Cartesian product is expanded instead.
function _deferred_getindex(
    array::Array{T},
    index::ExprTemplate{<:Real,V},
) where {T<:Real,V}
    expr =
        JuMP.GenericNonlinearExpr{V}(:getindex, _DataArray(array), _expr(index))
    return ExprTemplate{T}(expr, _iterators(index))
end

function _getindex_variable_array(
    v::Array{V},
    i::_ScalarWithIterator,
) where {V<:JuMP.AbstractVariableRef}
    nl = JuMP.GenericNonlinearExpr{V}(:getindex, to_generator(v), _expr(i))
    return ExprTemplate{V}(nl, _iterators(i))
end

# Resolve the one-dimensional variable-array case before the generic
# `Array`-of-data method above. Without this specialization, `x[i]` is stored
# as another iterator value instead of becoming an expression template.
function Base.getindex(
    v::Array{V},
    i::IteratorValues,
) where {V<:JuMP.AbstractVariableRef}
    return _getindex_variable_array(v, i)
end

function Base.getindex(
    v::Array{V},
    i::ExprTemplate,
) where {V<:JuMP.AbstractVariableRef}
    return _getindex_variable_array(v, i)
end

function Base.getindex(it::IteratorValues, i)
    @assert it.value_index == 1 # FIXME
    return IteratorValues(it.iterators, it.index, i)
end

function _getindex_expr(v::AbstractArray{V}, args...) where {V}
    return JuMP.GenericNonlinearExpr{V}(:getindex, to_generator(v), args...)
end

function Base.getindex(
    v::Array{V},
    i::_ScalarWithIterator,
) where {V<:JuMP.AbstractVariableRef}
    return ExprTemplate{V}(_getindex_expr(v, _expr(i)), _iterators(i))
end

function Base.getindex(
    v::Array{V},
    i::Integer,
    j::_ScalarWithIterator,
) where {V<:JuMP.AbstractVariableRef}
    return ExprTemplate{V}(_getindex_expr(v, i, _expr(j)), _iterators(j))
end

function Base.getindex(
    v::Array{V},
    i::_ScalarWithIterator,
    j::Integer,
) where {V<:JuMP.AbstractVariableRef}
    return ExprTemplate{V}(_getindex_expr(v, _expr(i), j), _iterators(i))
end

function Base.getindex(
    v::Array{V},
    i::_ScalarWithIterator,
    j::_ScalarWithIterator,
) where {V<:JuMP.AbstractVariableRef}
    i_expr, j_expr, iterators = _merge_exprs(i, j)
    nl = _getindex_expr(v, i_expr, j_expr)
    return ExprTemplate{V}(nl, iterators)
end

# Support indexing a `DenseAxisArray` of variables (e.g. `@variable(model, vm[keys(ref[:bus])])`,
# as PowerModels builds its models) by an iterator. The variables live contiguously in `v.data`,
# and `v.lookup` maps each axis key to its position; so `v[i]` is `v.data[position_of(i)]`. We
# reuse the existing `Dict`/`Array{VariableRef}` iterator-indexing methods for each half.
function Base.getindex(
    v::JuMP.Containers.DenseAxisArray{V,1},
    i::_ScalarWithIterator,
) where {V<:JuMP.AbstractVariableRef}
    return v.data[_axis_position(v.lookup[1], i)]
end
# `_AxisLookup` wraps either a `Dict` (general axis: map key -> position) or a `Base.OneTo`
# (integer `1:n` axis, where the key already is the position).
_axis_position(l::JuMP.Containers._AxisLookup{<:AbstractDict}, i) = l.data[i]
_axis_position(::JuMP.Containers._AxisLookup, i) = i
