# Copyright (c) 2024: Benoît Legat and contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

# Inspect a polynomial template once, then fill one term across all rows at a
# time. Planning can dispatch on Any; the row kernels receive concrete columns
# and never inspect ScalarNonlinearFunction nodes or boxed iterator tuples.
struct _BatchAxis{T} <: AbstractVector{T}
    values::Vector{T}
    stride::Int
    rows::Int
end
Base.size(axis::_BatchAxis) = (axis.rows,)
Base.IndexStyle(::Type{<:_BatchAxis}) = IndexLinear()
@inline function Base.getindex(axis::_BatchAxis, row::Int)
    @boundscheck checkbounds(axis, row)
    index = mod(div(row - 1, axis.stride), length(axis.values)) + 1
    return @inbounds axis.values[index]
end

struct _BatchContext
    iterators::Vector{Iterator}
    strides::Vector{Int}
    rows::Int
    columns::Dict{Tuple{Int,Int},Any}
end
function _BatchContext(iterators::Vector{Iterator})
    sizes = length.(iterators)
    strides = Int[]
    rows = 1
    for size in sizes
        push!(strides, rows)
        rows *= size
    end
    return _BatchContext(iterators, strides, rows, Dict{Tuple{Int,Int},Any}())
end

Base.@nospecializeinfer function _batch_column_values(
    @nospecialize(values::Vector),
    column::Int,
)
    if column == 0
        return values
    end
    V = typeof(values).parameters[1]
    if !(V <: Tuple && isconcretetype(V))
        return nothing
    end
    T = fieldtype(V, column)
    # Keep the supported column types explicit so a trimmed build can retain
    # their typed kernels even though the template stores erased values.
    if T === Int
        return _batch_extract_column(values, column, Int)
    elseif T === Float64
        return _batch_extract_column(values, column, Float64)
    elseif T === Float32
        return _batch_extract_column(values, column, Float32)
    elseif T === BigFloat
        return _batch_extract_column(values, column, BigFloat)
    elseif T === MOI.VariableIndex
        return _batch_extract_column(values, column, MOI.VariableIndex)
    elseif T === Symbol
        return _batch_extract_column(values, column, Symbol)
    elseif T === String
        return _batch_extract_column(values, column, String)
    end
    return nothing
end
Base.@nospecializeinfer function _batch_extract_column(
    @nospecialize(values::Vector),
    column::Int,
    ::Type{T},
) where {T}
    result = Vector{T}(undef, length(values))
    for i in eachindex(result)
        value = @inbounds @inline getindex(values, i)
        result[i] = (@inline _iterator_component(value, column))::T
    end
    return result
end
Base.@nospecializeinfer function _batch_make_axis(
    @nospecialize(values::Vector),
    stride::Int,
    rows::Int,
)
    if values isa Vector{Int}
        return _BatchAxis{Int}(values, stride, rows)
    elseif values isa Vector{Float64}
        return _BatchAxis{Float64}(values, stride, rows)
    elseif values isa Vector{Float32}
        return _BatchAxis{Float32}(values, stride, rows)
    elseif values isa Vector{BigFloat}
        return _BatchAxis{BigFloat}(values, stride, rows)
    elseif values isa Vector{MOI.VariableIndex}
        return _BatchAxis{MOI.VariableIndex}(values, stride, rows)
    elseif values isa Vector{Symbol}
        return _BatchAxis{Symbol}(values, stride, rows)
    elseif values isa Vector{String}
        return _BatchAxis{String}(values, stride, rows)
    end
    return nothing
end
function _batch_column(context::_BatchContext, index::Int, column::Int)
    return get!(context.columns, (index, column)) do
        values = _batch_column_values(context.iterators[index].values, column)
        if values === nothing
            return nothing
        end
        return _batch_make_axis(values, context.strides[index], context.rows)
    end
end

# Scalars use Ref so the same typed kernels can also consume iterator columns
# and computed vectors. No expression shape is encoded in a closure's type.
@inline _batch_at(value::Base.RefValue, _) = value[]
@inline _batch_at(value::AbstractVector, row) = value[row]
# A uniform column representation keeps the compiled row kernels closed over
# the coefficient type, without a separate specialization for every pairing
# of scalar, repeated-axis, and fully materialized operands.
struct _BatchKernelColumn{T}
    values::Vector{T}
    stride::Int
    period::Int
end
Base.@nospecializeinfer function _batch_kernel_column(
    ::Type{T},
    @nospecialize(value),
)::_BatchKernelColumn{T} where {T}
    if value isa Base.RefValue{T}
        return _BatchKernelColumn(T[value[]], 0, 0)
    elseif value isa Vector{T}
        return _BatchKernelColumn(value, 1, 0)
    end
    axis = value::_BatchAxis{T}
    return _BatchKernelColumn(axis.values, axis.stride, length(axis.values))
end
@inline function _batch_at(column::_BatchKernelColumn, row)
    if column.stride == 0
        return column.values[1]
    elseif column.period == 0
        return column.values[row]
    end
    return column.values[mod(div(row-1, column.stride), column.period)+1]
end

Base.@nospecializeinfer function _batch_convert(
    ::Type{T},
    @nospecialize(value),
)::Union{Base.RefValue{T},Vector{T},_BatchAxis{T}} where {T}
    if value isa Base.RefValue{T}
        return value
    elseif value isa Base.RefValue
        raw = value[]
        if raw isa Int
            return Ref(T(raw))
        elseif raw isa Float64
            return Ref(T(raw))
        elseif raw isa Float32
            return Ref(T(raw))
        elseif raw isa BigFloat
            return Ref(T(raw))
        end
        return Ref(T(raw))
    elseif value isa _BatchAxis{T}
        return value
    elseif value isa _BatchAxis{Int}
        return _BatchAxis(
            _batch_copy_column(T, value.values),
            value.stride,
            value.rows,
        )
    elseif value isa _BatchAxis{Float64}
        return _BatchAxis(
            _batch_copy_column(T, value.values),
            value.stride,
            value.rows,
        )
    elseif value isa _BatchAxis{Float32}
        return _BatchAxis(
            _batch_copy_column(T, value.values),
            value.stride,
            value.rows,
        )
    elseif value isa _BatchAxis{BigFloat}
        return _BatchAxis(
            _batch_copy_column(T, value.values),
            value.stride,
            value.rows,
        )
    elseif value isa Vector{T}
        return value
    elseif value isa Vector{Int}
        return _batch_copy_column(T, value)
    elseif value isa Vector{Float64}
        return _batch_copy_column(T, value)
    elseif value isa Vector{Float32}
        return _batch_copy_column(T, value)
    elseif value isa Vector{BigFloat}
        return _batch_copy_column(T, value)
    end
    return T.(value)
end
function _batch_copy_column(::Type{T}, values::Vector{S}) where {T,S}
    result = Vector{T}(undef, length(values))
    for row in eachindex(result)
        result[row] = T(values[row])
    end
    return result
end
Base.@nospecializeinfer function _batch_rows(
    ::Type{T},
    @nospecialize(value)
)::Int where {T}
    if value isa Base.RefValue{T}
        return 0
    elseif value isa Vector{T}
        return length(value)
    end
    return (value::_BatchAxis{T}).rows
end
Base.@nospecializeinfer function _batch_input_type(@nospecialize(value))
    if value isa Union{Base.RefValue{Int},Vector{Int},_BatchAxis{Int}}
        return Int
    elseif value isa
           Union{Base.RefValue{Float64},Vector{Float64},_BatchAxis{Float64}}
        return Float64
    elseif value isa
           Union{Base.RefValue{Float32},Vector{Float32},_BatchAxis{Float32}}
        return Float32
    elseif value isa
           Union{Base.RefValue{BigFloat},Vector{BigFloat},_BatchAxis{BigFloat}}
        return BigFloat
    end
    return Any
end
@inline function _batch_binary_scalar(op::Symbol, a::T, b::T)::T where {T}
    if op === :+
        return a + b
    elseif op === :-
        return a - b
    elseif op === :*
        return a * b
    elseif op === :/
        return T(a / b)
    end
    return T(a^b)
end
@inline function _batch_unary_scalar(op::Symbol, a::T)::T where {T}
    if op === :+
        return a
    elseif op === :-
        return -a
    end
    return abs(a)
end
Base.@nospecializeinfer function _batch_binary_values(
    ::Type{T},
    op::Symbol,
    @nospecialize(a),
    @nospecialize(b),
)::Union{Base.RefValue{T},Vector{T}} where {T}
    aa, bb = _batch_convert(T, a), _batch_convert(T, b)
    rows_a, rows_b = _batch_rows(T, aa), _batch_rows(T, bb)
    first, second = _batch_kernel_column(T, aa), _batch_kernel_column(T, bb)
    if rows_a == 0 && rows_b == 0
        return Ref(
            _batch_binary_scalar(op, _batch_at(first, 1), _batch_at(second, 1)),
        )
    elseif rows_a != 0 && rows_b != 0 && rows_a != rows_b
        throw(DimensionMismatch("Prepared columns have different row counts"))
    end
    return _batch_binary_rows(op, first, second, max(rows_a, rows_b))
end
function _batch_binary_rows(
    op::Symbol,
    first::_BatchKernelColumn{T},
    second::_BatchKernelColumn{T},
    rows,
) where {T}
    if op === :+
        return _batch_binary_rows(+, first, second, rows)
    elseif op === :-
        return _batch_binary_rows(-, first, second, rows)
    elseif op === :*
        return _batch_binary_rows(*, first, second, rows)
    elseif op === :/
        return _batch_binary_rows(/, first, second, rows)
    end
    return _batch_binary_rows(^, first, second, rows)
end
function _batch_binary_rows(
    op::F,
    first::_BatchKernelColumn{T},
    second::_BatchKernelColumn{T},
    rows,
) where {F,T}
    result = Vector{T}(undef, rows)
    for row in eachindex(result)
        result[row] = T(op(_batch_at(first, row), _batch_at(second, row)))
    end
    return result
end
Base.@nospecializeinfer function _batch_unary_values(
    ::Type{T},
    op::Symbol,
    @nospecialize(value),
)::Union{Base.RefValue{T},Vector{T}} where {T}
    converted = _batch_convert(T, value)
    rows = _batch_rows(T, converted)
    column = _batch_kernel_column(T, converted)
    if rows == 0
        return Ref(_batch_unary_scalar(op, _batch_at(column, 1)))
    end
    return _batch_unary_rows(op, column, rows)
end
function _batch_unary_rows(
    op::Symbol,
    column::_BatchKernelColumn{T},
    rows,
) where {T}
    if op === :+
        return _batch_unary_rows(+, column, rows)
    elseif op === :-
        return _batch_unary_rows(-, column, rows)
    end
    return _batch_unary_rows(abs, column, rows)
end
function _batch_unary_rows(
    op::F,
    column::_BatchKernelColumn{T},
    rows,
) where {F,T}
    result = Vector{T}(undef, rows)
    for row in eachindex(result)
        result[row] = T(op(_batch_at(column, row)))
    end
    return result
end
Base.@nospecializeinfer function _batch_apply(
    @nospecialize(f),
    @nospecialize(a),
    @nospecialize(b)
)
    A, B = _batch_input_type(a), _batch_input_type(b)
    if A === Any || B === Any
        return nothing
    end
    op = f === (+) ? :+ : f === (-) ? :- : f === (*) ? :* : f === (/) ? :/ : :^
    T =
        A === BigFloat || B === BigFloat ? BigFloat :
        A === Float64 || B === Float64 ? Float64 :
        A === Float32 || B === Float32 ? Float32 : Int
    # Match the scalar evaluator: promote operands, then float numeric data.
    if T === Int
        T = Float64
    end
    if T === Float64
        return _batch_binary_values(Float64, op, a, b)
    elseif T === Float32
        return _batch_binary_values(Float32, op, a, b)
    end
    return _batch_binary_values(BigFloat, op, a, b)
end
Base.@nospecializeinfer function _batch_apply(
    @nospecialize(f),
    @nospecialize(a)
)
    if f === _to_index
        return _batch_convert(Int, a)
    end
    T = _batch_input_type(a)
    if T === Any
        return nothing
    end
    op = f === (+) ? :+ : f === (-) ? :- : :abs
    if T === Int
        T = Float64
    end
    if T === Float64
        return _batch_unary_values(Float64, op, a)
    elseif T === Float32
        return _batch_unary_values(Float32, op, a)
    end
    return _batch_unary_values(BigFloat, op, a)
end
Base.@nospecializeinfer function _batch_iszero(@nospecialize(value))::Bool
    if value isa Base.RefValue{Float64}
        return iszero(value[])
    elseif value isa Base.RefValue{Float32}
        return iszero(value[])
    elseif value isa Base.RefValue{BigFloat}
        return iszero(value[])
    elseif value isa Base.RefValue{Int}
        return iszero(value[])
    end
    return false
end
Base.@nospecializeinfer function _batch_numeric(@nospecialize(value))::Bool
    return value isa Base.RefValue{<:Real} || value isa AbstractVector{<:Real}
end
Base.@nospecializeinfer function _batch_variable(@nospecialize(value))::Bool
    return value isa Base.RefValue{MOI.VariableIndex} ||
           value isa AbstractVector{MOI.VariableIndex}
end

function _batch_operator(head::Symbol, arity::Int)
    if head === :+
        return +
    elseif head === :- && arity in (1, 2)
        return -
    elseif head === :* && arity >= 2
        return *
    elseif head === :/ && arity == 2
        return /
    elseif head === :^ && arity == 2
        return ^
    elseif head === :abs && arity == 1
        return abs
    end
    return nothing
end

# This evaluator is used for numeric data and indices, once per template node.
# It never evaluates a variable-containing polynomial with generic arithmetic.
Base.@nospecializeinfer function _batch_value(
    @nospecialize(value),
    context::_BatchContext,
)
    if value isa Int
        return Ref(value)
    elseif value isa Float64
        return Ref(value)
    elseif value isa Float32
        return Ref(value)
    elseif value isa BigFloat
        return Ref(value)
    elseif value isa MOI.VariableIndex
        return Ref(value)
    elseif value isa Real
        return Ref(value)
    elseif value isa IteratorIndex
        return _batch_column(context, value.value, 0)
    elseif value isa MOI.ScalarNonlinearFunction
        return _batch_value_node(value, context)
    end
    return nothing
end
Base.@nospecializeinfer function _batch_lookup_one(
    ::Type{T},
    collection::C,
    @nospecialize(index),
)::Union{Base.RefValue{T},Vector{T}} where {T,C}
    converted = _batch_convert(Int, index)
    column = _batch_kernel_column(Int, converted)
    rows = _batch_rows(Int, converted)
    if rows == 0
        return Ref(getindex(collection, _batch_at(column, 1)))
    end
    return _batch_lookup_one_rows(collection, column, T, rows)
end
function _batch_lookup_one_rows(
    collection::C,
    column::_BatchKernelColumn{Int},
    ::Type{T},
    rows,
) where {C,T}
    result = Vector{T}(undef, rows)
    for row in eachindex(result)
        result[row] = getindex(collection, _batch_at(column, row))
    end
    return result
end
Base.@nospecializeinfer function _batch_lookup_two(
    ::Type{T},
    collection::C,
    @nospecialize(first),
    @nospecialize(second),
)::Union{Base.RefValue{T},Vector{T}} where {T,C}
    aa, bb = _batch_convert(Int, first), _batch_convert(Int, second)
    rows = max(_batch_rows(Int, aa), _batch_rows(Int, bb))
    a, b = _batch_kernel_column(Int, aa), _batch_kernel_column(Int, bb)
    if rows == 0
        return Ref(getindex(collection, _batch_at(a, 1), _batch_at(b, 1)))
    end
    return _batch_lookup_two_rows(collection, a, b, T, rows)
end
function _batch_lookup_two_rows(
    collection::C,
    first::_BatchKernelColumn{Int},
    second::_BatchKernelColumn{Int},
    ::Type{T},
    rows,
) where {C,T}
    result = Vector{T}(undef, rows)
    for row in eachindex(result)
        result[row] =
            getindex(collection, _batch_at(first, row), _batch_at(second, row))
    end
    return result
end
macro _batch_lookup_array(T)
    collection, indices = esc(:collection), esc(:indices)
    T = esc(T)
    return quote
        if $collection isa Vector{$T} && length($indices) == 1
            return _batch_lookup_one($T, $collection, $indices[1])
        elseif $collection isa Matrix{$T} && length($indices) == 2
            return _batch_lookup_two($T, $collection, $indices[1], $indices[2])
        elseif $collection isa Array{$T,0} && isempty($indices)
            return Ref(getindex($collection))
        end
    end
end
Base.@nospecializeinfer function _batch_lookup(
    @nospecialize(collection),
    indices::Vector{Any},
)
    if collection isa ContiguousArrayOfVariables{1} && length(indices) == 1
        return _batch_lookup_one(MOI.VariableIndex, collection, indices[1])
    elseif collection isa ContiguousArrayOfVariables{2} && length(indices) == 2
        return _batch_lookup_two(
            MOI.VariableIndex,
            collection,
            indices[1],
            indices[2],
        )
    elseif collection isa ContiguousArrayOfVariables{0} && isempty(indices)
        return Ref(getindex(collection))
    end
    @_batch_lookup_array Float64
    @_batch_lookup_array Float32
    @_batch_lookup_array Int
    @_batch_lookup_array BigFloat
    @_batch_lookup_array MOI.VariableIndex
    return nothing
end
function _batch_value_node(
    expr::MOI.ScalarNonlinearFunction,
    context::_BatchContext,
)
    args = expr.args
    if expr.head === :getindex
        collection = args[1]
        if collection isa IteratorIndex && length(args) == 2 && args[2] isa Int
            return _batch_column(context, collection.value, args[2])
        elseif !(collection isa AbstractArray || collection isa AbstractDict)
            return nothing
        end
        indices = Any[]
        for arg in args[2:end]
            index = _batch_value(arg, context)
            if index === nothing
                return nothing
            end
            if collection isa AbstractArray
                index = _batch_apply(_to_index, index)
            end
            push!(indices, index)
        end
        return _batch_lookup(collection, indices)
    end
    # The scalar folder uses MOI's numeric semantics (including NaN handling).
    # Prepare only literal unit and square powers; other powers and nonlinear
    # operators stay on that path instead of silently using Base semantics.
    if expr.head === :^ &&
       length(args) == 2 &&
       !(_is_unit_exponent(args[2]) || _is_square_exponent(args[2]))
        return nothing
    end
    operator = _batch_operator(expr.head, length(args))
    if operator === nothing || isempty(args)
        return nothing
    end
    values = Any[_batch_value(arg, context) for arg in args]
    if !all(_batch_numeric, values)
        return nothing
    elseif length(values) == 1
        return _batch_apply(operator, values[1])
    end
    if length(values) > 2
        big, wide, single = false, false, false
        for value in values
            type = _batch_input_type(value)
            if type === Any
                return nothing
            end
            big |= type === BigFloat
            wide |= type === Float64
            single |= type === Float32
        end
        if big
            return _batch_nary_values(BigFloat, operator, values)
        elseif wide || !single
            return _batch_nary_values(Float64, operator, values)
        end
        return _batch_nary_values(Float32, operator, values)
    end
    return _batch_apply(operator, values[1], values[2])
end

# Match scalar promotion before doing any arithmetic, including when an
# intermediate index expression has floated an otherwise integer column.
Base.@nospecializeinfer function _batch_nary_values(
    ::Type{T},
    @nospecialize(operator),
    values::Vector{Any},
)::Union{Base.RefValue{T},Vector{T},_BatchAxis{T}} where {T}
    value = _batch_convert(T, values[1])
    for next in values[2:end]
        value = _batch_apply(operator, value, _batch_convert(T, next))
    end
    return value
end

const _BatchValue{T} = Union{Base.RefValue{T},Vector{T},_BatchAxis{T}}

struct _BatchAffineTerm{T}
    coefficient::_BatchValue{T}
    variable::_BatchValue{MOI.VariableIndex}
end
struct _BatchQuadraticTerm{T}
    coefficient::_BatchValue{T}
    variable_1::_BatchValue{MOI.VariableIndex}
    variable_2::_BatchValue{MOI.VariableIndex}
end
struct _BatchPolynomial{T}
    constant::_BatchValue{T}
    affine::Vector{_BatchAffineTerm{T}}
    quadratic::Vector{_BatchQuadraticTerm{T}}
end
Base.@nospecializeinfer function _batch_constant(
    ::Type{T},
    @nospecialize(value)
)::_BatchPolynomial{T} where {T}
    return _BatchPolynomial{T}(
        _batch_convert(T, value),
        _BatchAffineTerm{T}[],
        _BatchQuadraticTerm{T}[],
    )
end
Base.@nospecializeinfer function _batch_scale(
    p::_BatchPolynomial{T},
    @nospecialize(scale)
)::_BatchPolynomial{T} where {T}
    return _BatchPolynomial{T}(
        _batch_apply(*, p.constant, scale),
        [
            _BatchAffineTerm{T}(
                _batch_apply(*, t.coefficient, scale),
                t.variable,
            ) for t in p.affine
        ],
        [
            _BatchQuadraticTerm{T}(
                _batch_apply(*, t.coefficient, scale),
                t.variable_1,
                t.variable_2,
            ) for t in p.quadratic
        ],
    )
end
function _batch_add(p::_BatchPolynomial{T}, q::_BatchPolynomial{T}) where {T}
    return _BatchPolynomial{T}(
        _batch_apply(+, p.constant, q.constant),
        vcat(p.affine, q.affine),
        vcat(p.quadratic, q.quadratic),
    )
end
function _batch_degree(p::_BatchPolynomial)
    return !isempty(p.quadratic) ? 2 : !isempty(p.affine) ? 1 : 0
end
function _batch_product(
    p::_BatchPolynomial{T},
    q::_BatchPolynomial{T},
) where {T}
    if _batch_degree(p) == 0
        return _batch_scale(q, p.constant)
    elseif _batch_degree(q) == 0
        return _batch_scale(p, q.constant)
    elseif _batch_degree(p) + _batch_degree(q) > 2
        return nothing
    end
    out = _batch_constant(T, Ref(zero(T)))
    if !_batch_iszero(q.constant)
        out = _batch_add(out, _batch_scale(p, q.constant))
    end
    if !_batch_iszero(p.constant)
        out = _batch_add(out, _batch_scale(q, p.constant))
    end
    # The two scaled polynomials counted the constant product twice.
    out = _BatchPolynomial{T}(
        _batch_apply(*, p.constant, q.constant),
        out.affine,
        out.quadratic,
    )
    for a in p.affine, b in q.affine
        push!(
            out.quadratic,
            _BatchQuadraticTerm{T}(
                _batch_apply(*, a.coefficient, b.coefficient),
                a.variable,
                b.variable,
            ),
        )
    end
    return out
end

Base.@nospecializeinfer function _batch_polynomial(
    ::Type{T},
    @nospecialize(expr),
    context::_BatchContext,
)::Union{_BatchPolynomial{T},Nothing} where {T}
    if expr isa PolynomialFunction
        return _batch_polynomial(T, expr.func, context)
    elseif expr isa _AnySumGenerator
        return _batch_polynomial(
            T,
            _build_function(MOI.ScalarQuadraticFunction{T}, expr, Any[]),
            context,
        )
    elseif expr isa MOI.ScalarAffineFunction ||
           expr isa MOI.ScalarQuadraticFunction
        affine =
            expr isa MOI.ScalarAffineFunction ? expr.terms : expr.affine_terms
        quadratic =
            expr isa MOI.ScalarQuadraticFunction ? expr.quadratic_terms : ()
        # Plans store ordinary monomials. MOI halves diagonal coefficients
        # when evaluating a quadratic function; the fill kernel restores them.
        return _BatchPolynomial{T}(
            Ref(T(expr.constant)),
            [
                _BatchAffineTerm{T}(Ref(T(t.coefficient)), Ref(t.variable)) for
                t in affine
            ],
            [
                _BatchQuadraticTerm{T}(
                    Ref(
                        T(t.coefficient) /
                        (t.variable_1 == t.variable_2 ? T(2) : one(T)),
                    ),
                    Ref(t.variable_1),
                    Ref(t.variable_2),
                ) for t in quadratic
            ],
        )
    elseif expr isa MOI.ScalarNonlinearFunction && expr.head !== :getindex
        samples = Any[]
        for iterator in context.iterators
            push!(samples, @inbounds @inline getindex(iterator.values, 1))
        end
        if !_contains_variable(expr, samples)
            numeric = _batch_value(expr, context)
            return _batch_numeric(numeric) ? _batch_constant(T, numeric) :
                   nothing
        end
        return _batch_polynomial_node(T, expr, context)
    end
    value = _batch_value(expr, context)
    if _batch_numeric(value)
        return _batch_constant(T, value)
    elseif _batch_variable(value)
        return _BatchPolynomial{T}(
            Ref(zero(T)),
            [_BatchAffineTerm{T}(Ref(one(T)), value)],
            _BatchQuadraticTerm{T}[],
        )
    end
    return nothing
end
function _batch_polynomial_node(::Type{T}, expr, context) where {T}
    head, args = expr.head, expr.args
    if head === :+ || head === :- || head === :*
        if isempty(args) || (head === :- && !(length(args) in (1, 2)))
            return nothing
        end
        result = _batch_polynomial(T, args[1], context)
        if result === nothing
            return nothing
        elseif head === :- && length(args) == 1
            return _batch_scale(result, Ref(-one(T)))
        end
        for arg in args[2:end]
            next = _batch_polynomial(T, arg, context)
            if next === nothing
                return nothing
            end
            result = if head === :*
                _batch_product(result, next)
            else
                _batch_add(
                    result,
                    head === :- ? _batch_scale(next, Ref(-one(T))) : next,
                )
            end
            if result === nothing
                return nothing
            end
        end
        return result
    elseif head === :/ && length(args) == 2
        numerator = _batch_polynomial(T, args[1], context)
        denominator = _batch_value(args[2], context)
        if numerator !== nothing && _batch_numeric(denominator)
            scale = _batch_apply(/, Ref(one(T)), _batch_convert(T, denominator))
            return _batch_scale(numerator, scale)
        end
        return nothing
    elseif head === :^ &&
           length(args) == 2 &&
           (_is_unit_exponent(args[2]) || _is_square_exponent(args[2]))
        base = _batch_polynomial(T, args[1], context)
        if base === nothing || _is_unit_exponent(args[2])
            return base
        end
        return _batch_product(base, base)
    end
    value = _batch_value(expr, context)
    return _batch_numeric(value) ? _batch_constant(T, value) : nothing
end

function _batch_functions(
    ::Type{MOI.ScalarAffineFunction{T}},
    rows,
    na,
    _,
) where {T}
    return [
        MOI.ScalarAffineFunction(
            Vector{MOI.ScalarAffineTerm{T}}(undef, na),
            zero(T),
        ) for _ in 1:rows
    ]
end
function _batch_functions(
    ::Type{MOI.ScalarQuadraticFunction{T}},
    rows,
    na,
    nq,
) where {T}
    return [
        MOI.ScalarQuadraticFunction(
            Vector{MOI.ScalarQuadraticTerm{T}}(undef, nq),
            Vector{MOI.ScalarAffineTerm{T}}(undef, na),
            zero(T),
        ) for _ in 1:rows
    ]
end
_batch_affine_terms(f::MOI.ScalarAffineFunction) = f.terms
_batch_affine_terms(f::MOI.ScalarQuadraticFunction) = f.affine_terms

function _batch_fill_constants!(functions, values::V, offset::Int = 0) where {V}
    for row in eachindex(functions)
        functions[row].constant = _batch_at(values, row + offset)
    end
    return
end
function _batch_fill_affine!(
    functions,
    slot,
    coefficients::C,
    variables::V,
    offset::Int = 0,
) where {C,V}
    for row in eachindex(functions)
        coefficient = _batch_at(coefficients, row + offset)
        variable = _batch_at(variables, row + offset)
        _batch_affine_terms(functions[row])[slot] =
            MOI.ScalarAffineTerm(coefficient, variable)
    end
    return
end
function _batch_fill_quadratic!(
    functions,
    slot,
    coefficients::C,
    first::A,
    second::B,
    offset::Int = 0,
) where {C,A,B}
    for row in eachindex(functions)
        a, b = _batch_at(first, row + offset), _batch_at(second, row + offset)
        coefficient = _batch_at(coefficients, row + offset)
        if a == b
            coefficient *= 2
        end
        functions[row].quadratic_terms[slot] =
            MOI.ScalarQuadraticTerm(coefficient, a, b)
    end
    return
end

_try_batch_constraints(_, ::FunctionGenerator, _) = nothing
function _try_batch_constraints(
    model,
    func::FunctionGenerator{F},
    set::S,
) where {
    T,
    F<:Union{MOI.ScalarAffineFunction{T},MOI.ScalarQuadraticFunction{T}},
    S,
}
    if !(T === Float32 || T === Float64 || T === BigFloat)
        return nothing
    end
    context = _BatchContext(func.iterators)
    if context.rows == 0
        return MOI.ConstraintIndex{F,S}[]
    end
    plan = _batch_polynomial(T, func.func, context)
    if plan === nothing ||
       (F <: MOI.ScalarAffineFunction && !isempty(plan.quadratic))
        return nothing
    end
    return _batch_add_constraints(model, plan, F, set, context.rows)
end

# Bound temporary function storage while retaining a single prepared plan.
const _BATCH_ROWS = 65_536

function _batch_add_constraints(
    model,
    plan::_BatchPolynomial{T},
    ::Type{F},
    set::S,
    rows,
) where {T,F,S}
    constraints = Vector{MOI.ConstraintIndex{F,S}}(undef, rows)
    for first_row in 1:_BATCH_ROWS:rows
        count = min(_BATCH_ROWS, rows - first_row + 1)
        offset = first_row - 1
        functions = _batch_functions(
            F,
            count,
            length(plan.affine),
            length(plan.quadratic),
        )
        _batch_fill_constants!(
            functions,
            _batch_kernel_column(T, plan.constant),
            offset,
        )
        for (slot, term) in enumerate(plan.affine)
            _batch_fill_affine!(
                functions,
                slot,
                _batch_kernel_column(T, term.coefficient),
                _batch_kernel_column(MOI.VariableIndex, term.variable),
                offset,
            )
        end
        for (slot, term) in enumerate(plan.quadratic)
            _batch_fill_quadratic!(
                functions,
                slot,
                _batch_kernel_column(T, term.coefficient),
                _batch_kernel_column(MOI.VariableIndex, term.variable_1),
                _batch_kernel_column(MOI.VariableIndex, term.variable_2),
                offset,
            )
        end
        for row in eachindex(functions)
            # Each function is fresh and owned by this batch, so normalization
            # can remove its constant without copying all its term vectors.
            constraints[row+offset] =
                MOI.Utilities.normalize_and_add_constraint(
                    model,
                    functions[row],
                    set;
                    allow_modify_function = true,
                )
        end
    end
    return constraints
end
