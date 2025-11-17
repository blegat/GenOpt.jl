# Copyright (c) 2024: Benoît Legat and contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

import MathOptInterface as MOI

struct ContiguousArrayOfVariables{N} <: AbstractArray{MOI.VariableIndex,N}
    offset::Int64
    size::NTuple{N,Int64}
end

Base.copy(array::ContiguousArrayOfVariables) = array
Base.size(array::ContiguousArrayOfVariables) = array.size

"""
    struct Iterator{T}
        values::Vector{T}
    end
"""
struct Iterator{T}
    values::Vector{T}
end

Iterator(values::AbstractArray) = Iterator(collect(values))

struct IteratorIndex
    value::Int
end

Base.copy(i::IteratorIndex) = i

struct FunctionGenerator{F} <: MOI.AbstractVectorFunction
    func::MOI.ScalarNonlinearFunction
    iterators::Vector{Iterator} # Slight type instability, we don't have `Iterator{T}`
end

function Base.copy(f::FunctionGenerator{F}) where {F}
    return FunctionGenerator{F}(copy(f.func), f.iterators)
end
function MOI.Utilities.is_canonical(f::FunctionGenerator)
    return MOI.Utilities.is_canonical(f.func)
end
function MOI.Utilities.map_indices(::MOI.Utilities.IndexMap, func::FunctionGenerator)
    # TODO check it's identity
    return func
end
function MOI.Utilities.is_coefficient_type(::Type{FunctionGenerator{E}}, ::Type{T}) where {E,T}
    return MOI.Utilities.is_coefficient_type(E, T)
end
