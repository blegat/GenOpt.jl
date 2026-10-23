# Copyright (c) 2024: Benoît Legat and contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    PolynomialFunction{F}(func::MOI.ScalarNonlinearFunction)

A structured nonlinear expression declared convertible to polynomial type `F`,
either `MOI.ScalarAffineFunction{T}` or `MOI.ScalarQuadraticFunction{T}`. The
expression, including lazy sums, is retained until a receiving optimizer needs
the polynomial representation. Conversion validates the declaration.
"""
struct PolynomialFunction{
    F<:Union{MOI.ScalarAffineFunction,MOI.ScalarQuadraticFunction},
} <: MOI.AbstractScalarFunction
    func::MOI.ScalarNonlinearFunction
end

function Base.copy(f::PolynomialFunction{F}) where {F}
    return PolynomialFunction{F}(copy(f.func))
end

function Base.isapprox(a::PolynomialFunction, b::PolynomialFunction; kwargs...)
    return isapprox(a.func, b.func; kwargs...)
end

function MOI.Utilities.map_indices(
    index_map::I,
    f::PolynomialFunction{F},
) where {I<:Function,F}
    return PolynomialFunction{F}(MOI.Utilities.map_indices(index_map, f.func))
end

function MOI.Utilities.is_canonical(f::PolynomialFunction)
    return MOI.Utilities.is_canonical(f.func)
end

function MOI.Utilities.canonicalize!(f::PolynomialFunction)
    MOI.Utilities.canonicalize!(f.func)
    return f
end

MOI.constant(::PolynomialFunction, ::Type{T}) where {T} = zero(T)
function MOI.constant(
    ::PolynomialFunction{MOI.ScalarAffineFunction{T}},
) where {T}
    return zero(T)
end
function MOI.constant(
    ::PolynomialFunction{MOI.ScalarQuadraticFunction{T}},
) where {T}
    return zero(T)
end

function Base.isapprox(a::SumGenerator, b::SumGenerator; kwargs...)
    return isapprox(a.func, b.func; kwargs...) &&
           length(a.iterators) == length(b.iterators) &&
           all(
               ai.values == bi.values for
               (ai, bi) in zip(a.iterators, b.iterators)
           )
end

function Base.isapprox(
    a::FilteredSumGenerator,
    b::FilteredSumGenerator;
    kwargs...,
)
    return a.filter === b.filter &&
           isapprox(a.func, b.func; kwargs...) &&
           length(a.iterators) == length(b.iterators) &&
           all(
               ai.values == bi.values for
               (ai, bi) in zip(a.iterators, b.iterators)
           )
end
