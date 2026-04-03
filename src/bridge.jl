# Copyright (c) 2024: Benoît Legat and contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    FunctionGeneratorBridge{T,F,S}

Bridge that expands a `FunctionGenerator{F}` constraint into individual
scalar `F`-in-`S` constraints.

The `FunctionGenerator` contains a template `MOI.ScalarNonlinearFunction`
with `IteratorIndex` placeholders. This bridge substitutes concrete values
from the iterators for each combination and converts each expanded expression
to the type `F` declared by the `FunctionGenerator{F}` type parameter.
If the expanded expression cannot be converted to `F`, an error is thrown.
"""
struct FunctionGeneratorBridge{T,F,S,VS<:MOI.Utilities.VectorLinearSet} <:
       MOI.Bridges.Constraint.AbstractBridge
    constraints::Vector{MOI.ConstraintIndex{F,S}}
    func::FunctionGenerator{F}
    set::VS
end

function MOI.Bridges.Constraint.bridge_constraint(
    ::Type{FunctionGeneratorBridge{T,F,S,VS}},
    model::MOI.ModelLike,
    func::FunctionGenerator{F},
    set::VS,
) where {T,F,S,VS<:MOI.Utilities.VectorLinearSet}
    scalar_set = S(zero(T))
    constraints = MOI.ConstraintIndex{F,S}[]
    sizes = Tuple(length.(func.iterators))
    for idx in CartesianIndices(sizes)
        values = [
            func.iterators[k].values[idx[k]] for k in eachindex(func.iterators)
        ]
        expanded = _expand(func.func, values)
        scalar_func = convert(F, expanded)
        ci = MOI.Utilities.normalize_and_add_constraint(
            model,
            scalar_func,
            scalar_set,
        )
        push!(constraints, ci)
    end
    return FunctionGeneratorBridge{T,F,S,VS}(constraints, func, set)
end

function MOI.supports_constraint(
    ::Type{FunctionGeneratorBridge{T}},
    ::Type{<:FunctionGenerator},
    ::Type{<:MOI.Utilities.VectorLinearSet},
) where {T}
    return true
end

function MOI.Bridges.Constraint.concrete_bridge_type(
    ::Type{FunctionGeneratorBridge{T}},
    ::Type{FunctionGenerator{F}},
    VS::Type{<:MOI.Utilities.VectorLinearSet},
) where {T,F}
    S = MOI.Utilities.scalar_set_type(VS, T)
    return FunctionGeneratorBridge{T,F,S,VS}
end

function MOI.Bridges.added_constraint_types(
    ::Type{<:FunctionGeneratorBridge{T,F,S}},
) where {T,F,S}
    return Tuple{Type,Type}[(F, S)]
end

function MOI.Bridges.added_constrained_variable_types(
    ::Type{<:FunctionGeneratorBridge},
)
    return Tuple{Type}[]
end

function MOI.get(
    bridge::FunctionGeneratorBridge{T,F,S},
    ::MOI.NumberOfConstraints{F,S},
) where {T,F,S}
    return length(bridge.constraints)
end

function MOI.get(
    bridge::FunctionGeneratorBridge{T,F,S},
    ::MOI.ListOfConstraintIndices{F,S},
) where {T,F,S}
    return copy(bridge.constraints)
end

function MOI.get(
    ::MOI.ModelLike,
    ::MOI.ConstraintFunction,
    bridge::FunctionGeneratorBridge,
)
    return copy(bridge.func)
end

function MOI.get(
    ::MOI.ModelLike,
    ::MOI.ConstraintSet,
    bridge::FunctionGeneratorBridge,
)
    return bridge.set
end

function MOI.delete(model::MOI.ModelLike, bridge::FunctionGeneratorBridge)
    for ci in bridge.constraints
        MOI.delete(model, ci)
    end
    return
end

# --- Expression expansion ---

"""
    _expand(func, values)

Recursively expand an `MOI.ScalarNonlinearFunction` template by substituting
`IteratorIndex` placeholders with concrete `values`.

Evaluates `:getindex` nodes (for variable and data lookups) and simplifies
pure-numeric subexpressions.
"""
function _expand(func::MOI.ScalarNonlinearFunction, values)
    args = [_expand(arg, values) for arg in func.args]
    if func.head == :getindex
        collection = args[1]
        indices = [_to_index(a) for a in args[2:end]]
        return getindex(collection, indices...)
    end
    if all(_is_numeric, args)
        return _eval_op(func.head, args)
    end
    return MOI.ScalarNonlinearFunction(func.head, args)
end

_expand(idx::IteratorIndex, values) = values[idx.value]
_expand(x, _) = x  # constants, MOI.VariableIndex, etc.

_is_numeric(::Number) = true
_is_numeric(_) = false

_to_index(x::Integer) = Int(x)
_to_index(x::AbstractFloat) = Int(x)
_to_index(x) = x

function _eval_op(head::Symbol, args::Vector)
    registry = MOI.Nonlinear.OperatorRegistry()
    float_args = Float64.(args)
    if length(float_args) == 1
        return MOI.Nonlinear.eval_univariate_function(
            registry,
            head,
            float_args[1],
        )
    else
        return MOI.Nonlinear.eval_multivariate_function(
            registry,
            head,
            float_args,
        )
    end
end
