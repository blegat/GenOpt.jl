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

function Base.getindex(A::ContiguousArrayOfVariables, I::Integer...)
    index = A.offset + Base._to_linear_index(CartesianIndices(A.size), I...)
    return MOI.VariableIndex(index)
end

"""
    struct Iterator{T}
        values::Vector{T}
    end
"""
struct Iterator{T}
    values::Vector{T}
end

Iterator(values::AbstractArray) = Iterator(vec(collect(values)))

Base.length(it::Iterator) = length(it.values)

struct IteratorIndex
    value::Int
end

Base.copy(i::IteratorIndex) = i
function Base.isapprox(a::IteratorIndex, b::IteratorIndex; kwargs...)
    return a.value == b.value
end

struct FunctionGenerator{F} <: MOI.AbstractVectorFunction
    func::MOI.ScalarNonlinearFunction
    iterators::Vector{Iterator} # Slight type instability, we don't have `Iterator{T}`
end

function Base.copy(f::FunctionGenerator{F}) where {F}
    return FunctionGenerator{F}(copy(f.func), f.iterators)
end

function Base.isapprox(a::FunctionGenerator, b::FunctionGenerator; kwargs...)
    return isapprox(a.func, b.func; kwargs...) &&
           length(a.iterators) == length(b.iterators) &&
           all(
               isapprox(ai.values, bi.values; kwargs...) for
               (ai, bi) in zip(a.iterators, b.iterators)
           )
end
function MOI.Utilities.is_canonical(f::FunctionGenerator)
    return MOI.Utilities.is_canonical(f.func)
end

function MOI.output_dimension(f::FunctionGenerator)
    return prod(length, f.iterators)
end

function MOI.Utilities.is_coefficient_type(
    ::Type{<:FunctionGenerator},
    ::Type{T},
) where {T}
    # Return false so standard MOI bridges (ScalarizeBridge, FlipSignBridge, etc.)
    # don't try to handle FunctionGenerator. Only FunctionGeneratorBridge should.
    return false
end

struct SumGenerator{F} <: MOI.AbstractScalarFunction
    func::MOI.ScalarNonlinearFunction
    iterators::Vector{Iterator} # Slight type instability, we don't have `Iterator{T}`
end

function Base.copy(f::SumGenerator{F}) where {F}
    return SumGenerator{F}(copy(f.func), f.iterators)
end

function MOI.Utilities.map_indices(
    ::MOI.Utilities.IndexMap,
    func::Union{FunctionGenerator,SumGenerator},
)
    # TODO check it's identity
    return func
end

function MOI.Utilities.map_indices(
    ::Function,
    func::Union{FunctionGenerator,SumGenerator},
)
    # TODO check it's identity
    return func
end
