"""
    struct IteratorValues{I}
        iterator::Iterator
        values::I
    end

Mapped values for `iterator`. The values at `i`th position
of the iterator after the mapping is `values[i]`.
In general, given an iterator `it::IteratorValues`, for any operator
`op`, `op(it)` is `IteratorValues(it, op.(it.values))`
"""
struct IteratorValues{I}
    iterator::Iterator
    values::I
end

function iterator(values)
    return IteratorValues(Iterator(length(values)), values)
end

function Base.getindex(d::Dict, i::IteratorValues)
    return IteratorValues(i.iterator, [d[val] for val in i.values])
end

# The following is intentionally kept close to JuMP/src/nlp_expr.jl

# Univariate operators
for f in MOI.Nonlinear.DEFAULT_UNIVARIATE_OPERATORS
    if isdefined(Base, f)
        @eval function Base.$(f)(x::IteratorValues)
            return IteratorValues(x.iterator, $(f).(x.values))
        end
    end
end

const _MULTIVARIATE_OPERATORS = [:+, :-, :*, :^, :/, :atan, :min, :max]

# Multivariate operators
for f in _MULTIVARIATE_OPERATORS
    @eval begin
        function Base.$(f)(x::IteratorValues, y::Number)
            return IteratorValues(x.iterator, $(f).(x.values, y))
        end
        function Base.$(f)(x::Number, y::IteratorValues)
            return IteratorValues(x.iterator, $(f).(x, y.values))
        end
        function Base.$(f)(::IteratorValues, ::IteratorValues)
            error("Operation between two iterators is not implemented yet")
        end
    end
end
