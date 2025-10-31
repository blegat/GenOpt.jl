# Copyright (c) 2024: Benoît Legat and contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

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
    iterator::Iterator
    values::I
end

function Base.show(io::IO, i::IteratorValues)
    print(io, "iterator(")
    print(io, i.values)
    print(io, ")")
    return
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
            return IteratorValues(y.iterator, $(f).(x, y.values))
        end
        function Base.$(f)(::IteratorValues, ::IteratorValues)
            error("Operation between two iterators is not implemented yet")
        end
    end
end
