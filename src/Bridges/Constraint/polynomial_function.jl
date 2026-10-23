# Copyright (c) 2024: Benoît Legat and contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    PolynomialFunctionBridge{T,F,S}

Lower a `PolynomialFunction{F}` constraint to its concrete polynomial type `F`.
Constants are moved into the scalar set, as required by solvers such as HiGHS.
Only the moved constant is retained: function queries reconstruct an equivalent
wrapper from the current inner function, including after variable deletion.
"""
mutable struct PolynomialFunctionBridge{T,F,S} <:
               MOI.Bridges.Constraint.AbstractFunctionConversionBridge{F,S}
    constraint::MOI.ConstraintIndex{F,S}
    constant::T
end

function MOI.supports_constraint(
    ::Type{PolynomialFunctionBridge{T}},
    ::Type{PolynomialFunction{F}},
    ::Type{<:MOI.AbstractScalarSet},
) where {T,F}
    return MOI.Utilities.is_coefficient_type(F, T)
end

function MOI.Bridges.Constraint.concrete_bridge_type(
    ::Type{PolynomialFunctionBridge{T}},
    ::Type{PolynomialFunction{F}},
    ::Type{S},
) where {T,F,S<:MOI.AbstractScalarSet}
    return PolynomialFunctionBridge{T,F,S}
end

function MOI.Bridges.Constraint.bridge_constraint(
    ::Type{PolynomialFunctionBridge{T,F,S}},
    model::MOI.ModelLike,
    func::PolynomialFunction{F},
    set::S,
) where {T,F,S}
    polynomial = convert(F, func)
    constant =
        MOI.Utilities.supports_shift_constant(S) ? polynomial.constant : zero(T)
    constraint = MOI.Utilities.normalize_and_add_constraint(
        model,
        polynomial,
        set;
        allow_modify_function = true,
    )
    return PolynomialFunctionBridge{T,F,S}(constraint, constant)
end

function MOI.get(
    model::MOI.ModelLike,
    ::MOI.ConstraintFunction,
    bridge::PolynomialFunctionBridge{T,F},
) where {T,F}
    func = copy(MOI.get(model, MOI.ConstraintFunction(), bridge.constraint))
    func.constant += bridge.constant
    return convert(PolynomialFunction{F}, func)
end

function MOI.get(
    model::MOI.ModelLike,
    ::MOI.ConstraintSet,
    bridge::PolynomialFunctionBridge{T,F,S},
) where {T,F,S}
    set = MOI.get(model, MOI.ConstraintSet(), bridge.constraint)
    if MOI.Utilities.supports_shift_constant(S)
        return MOI.Utilities.shift_constant(set, bridge.constant)
    end
    return set
end

function MOI.set(
    model::MOI.ModelLike,
    ::MOI.ConstraintFunction,
    bridge::PolynomialFunctionBridge{T,F,S},
    func::PolynomialFunction{F},
) where {T,F,S}
    set = MOI.get(model, MOI.ConstraintSet(), bridge)
    polynomial = convert(F, func)
    constant =
        MOI.Utilities.supports_shift_constant(S) ? polynomial.constant : zero(T)
    polynomial, set = MOI.Utilities.normalize_constant(
        polynomial,
        set;
        allow_modify_function = true,
    )
    MOI.set(model, MOI.ConstraintFunction(), bridge.constraint, polynomial)
    MOI.set(model, MOI.ConstraintSet(), bridge.constraint, set)
    bridge.constant = constant
    return
end

function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.ConstraintSet,
    bridge::PolynomialFunctionBridge{T,F,S},
    set::S,
) where {T,F,S}
    if MOI.Utilities.supports_shift_constant(S)
        set = MOI.Utilities.shift_constant(set, -bridge.constant)
    end
    MOI.set(model, attr, bridge.constraint, set)
    return
end

function MOI.get(
    model::MOI.ModelLike,
    attr::Union{MOI.ConstraintPrimal,MOI.ConstraintPrimalStart},
    bridge::PolynomialFunctionBridge,
)
    value = MOI.get(model, attr, bridge.constraint)
    return value === nothing ? nothing : value + bridge.constant
end

function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.ConstraintPrimalStart,
    bridge::PolynomialFunctionBridge,
    value,
)
    shifted = value === nothing ? nothing : value - bridge.constant
    MOI.set(model, attr, bridge.constraint, shifted)
    return
end

"""
    PolynomialFunctions{T}(model)

Wrap `model` in a constraint `SingleBridgeOptimizer` that lowers typed polynomial
functions. To retain ordinary bridge selection, register
`PolynomialFunctionBridge{T}` with `MOI.Bridges.add_bridge` instead.
"""
const PolynomialFunctions{T,OT<:MOI.ModelLike} =
    MOI.Bridges.Constraint.SingleBridgeOptimizer{PolynomialFunctionBridge{T},OT}
