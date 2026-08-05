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

"""
    struct IteratorRef
        iterator::Iterator
    end

Reference to an [`Iterator`](@ref) by identity, usable in a template
expression before any generator exists. Unlike the positional
[`IteratorIndex`](@ref), it does not depend on a generator's iterator list,
so templates can be built eagerly and the same iterator can be reused across
generators. The [`FunctionGenerator`](@ref) constructor taking only a
template discovers the distinct iterators (in first-encounter order) and
rewrites each `IteratorRef` into the corresponding `IteratorIndex`.
"""
struct IteratorRef
    iterator::Iterator
end

struct FunctionGenerator{F} <: MOI.AbstractVectorFunction
    func::MOI.ScalarNonlinearFunction
    iterators::Vector{Iterator} # Slight type instability, we don't have `Iterator{T}`
end

function _index_refs(
    func::MOI.ScalarNonlinearFunction,
    positions::IdDict{Iterator,Int},
    iterators::Vector{Iterator},
)
    args = Any[_index_refs(arg, positions, iterators) for arg in func.args]
    return MOI.ScalarNonlinearFunction(func.head, args)
end

function _index_refs(
    ref::IteratorRef,
    positions::IdDict{Iterator,Int},
    iterators::Vector{Iterator},
)
    position = get!(positions, ref.iterator) do
        push!(iterators, ref.iterator)
        return length(iterators)
    end
    return IteratorIndex(position)
end

_index_refs(arg, _, _) = arg

function collect_iterator_refs(func::MOI.ScalarNonlinearFunction) where {F}
    iterators = Iterator[]
    func = _index_refs(func, IdDict{Iterator,Int}(), iterators)
    return func, iterators
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

"""
    VectorInterval{T} <: MOI.AbstractVectorSet

Vector set `{x : lower[k] ≤ x[k] ≤ upper[k]}` carrying *per-element* bounds. Used as the set
of a [`FunctionGenerator`](@ref) built from an interval constraint `lb[i] ≤ f(i) ≤ ub[i]`
(with `container = ParametrizedArray`), where the bounds `lb`/`ub` vary with the iterator `i`.
"""
struct VectorInterval{T} <: MOI.AbstractVectorSet
    lower::Vector{T}
    upper::Vector{T}
end

MOI.dimension(s::VectorInterval) = length(s.lower)
Base.copy(s::VectorInterval) = VectorInterval(copy(s.lower), copy(s.upper))
