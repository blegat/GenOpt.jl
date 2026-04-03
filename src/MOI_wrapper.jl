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
function MOI.Utilities.is_coefficient_type(
    ::Type{FunctionGenerator{E}},
    ::Type{T},
) where {E,T}
    return MOI.Utilities.is_coefficient_type(E, T)
end

# Methods needed for the LazyBridgeOptimizer to explore bridge paths
# involving FunctionGenerator without erroring on promote_operation calls
# from other bridges (e.g., FlipSignBridge, VectorSlackBridge).
for op in (-, +)
    @eval function MOI.Utilities.promote_operation(
        ::typeof($op),
        ::Type{T},
        ::Type{FunctionGenerator{F}},
    ) where {T<:Number,F}
        return FunctionGenerator{F}
    end
    @eval function MOI.Utilities.promote_operation(
        ::typeof($op),
        ::Type{T},
        ::Type{FunctionGenerator{F}},
        ::Type{<:MOI.AbstractVectorFunction},
    ) where {T<:Number,F}
        return FunctionGenerator{F}
    end
    @eval function MOI.Utilities.promote_operation(
        ::typeof($op),
        ::Type{T},
        ::Type{<:MOI.AbstractVectorFunction},
        ::Type{FunctionGenerator{F}},
    ) where {T<:Number,F}
        return FunctionGenerator{F}
    end
end

function MOI.Utilities.scalar_type(::Type{FunctionGenerator{F}}) where {F}
    return F
end

function MOI.Utilities.operate(
    ::typeof(-),
    ::Type{T},
    f::FunctionGenerator{F},
) where {T,F}
    return FunctionGenerator{F}(
        MOI.ScalarNonlinearFunction(:-, Any[f.func]),
        f.iterators,
    )
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
