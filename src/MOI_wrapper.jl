# Copyright (c) 2024: Benoît Legat and contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

import MathOptInterface as MOI

"""
    struct Iterator
        length::Int
        values::Vector{Float64}
    end
"""
struct Iterator
    length::Int
    values::Vector{Float64}
    function Iterator(length::Int)
        return new(length, Float64[])
    end
end

function num_values(it::Iterator)
    return div(length(it.values), it.length)
end

function values_at(it::Iterator, i)
    return it.values[(1+it.length*(i-1)):(it.length*i)]
end

struct IteratorIndex
    iterator_index::Int
    value_index::Int
end

Base.copy(i::IteratorIndex) = i

struct FunctionGenerator{F} <: MOI.AbstractVectorFunction
    func::MOI.ScalarNonlinearFunction
    iterators::Vector{Iterator}
end

function Base.copy(f::FunctionGenerator{F}) where {F}
    return FunctionGenerator{F}(copy(f.func), f.iterators)
end
function MOI.Utilities.is_canonical(f::FunctionGenerator)
    return MOI.Utilities.is_canonical(f.func)
end
