# Copyright (c) 2024: Benoît Legat and contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

const _PolynomialTarget =
    Union{MOI.ScalarAffineFunction,MOI.ScalarQuadraticFunction}

function Base.convert(
    ::Type{F},
    func::PolynomialFunction{F},
) where {F<:_PolynomialTarget}
    return _build_function(F, func.func, Any[])
end

function Base.convert(
    ::Type{PolynomialFunction{F}},
    func::F,
) where {F<:_PolynomialTarget}
    return PolynomialFunction{F}(convert(MOI.ScalarNonlinearFunction, func))
end

function Base.convert(
    ::Type{PolynomialFunction{F}},
    func::MOI.ScalarNonlinearFunction,
) where {F<:_PolynomialTarget}
    return PolynomialFunction{F}(func)
end

# Unrelated MOI bridges cannot operate directly on this structured wrapper.
# The concrete-target objective conversion below handles it explicitly.
function MOI.Utilities.is_coefficient_type(::Type{<:PolynomialFunction}, ::Type)
    return false
end

function MOI.Bridges.Constraint.conversion_cost(
    ::Type{F},
    ::Type{PolynomialFunction{F}},
) where {F<:_PolynomialTarget}
    return 1.0
end

function MOI.Bridges.Objective.supports_objective_function(
    ::Type{<:MOI.Bridges.Objective.FunctionConversionBridge{T,F}},
    ::Type{PolynomialFunction{G}},
) where {T,F<:MOI.AbstractFunction,G<:_PolynomialTarget}
    return F === G && MOI.Utilities.is_coefficient_type(G, T)
end
