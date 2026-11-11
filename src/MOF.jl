# Copyright (c) 2024: Benoît Legat and contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

# Experimental MathOptFormat serialization. MathOptInterface's MOF reader and
# writer dispatch through open generic functions, so GenOpt can extend them
# without replacing either implementation.

const _MOF = MOI.FileFormats.MOF

function _mof_function_type(::Type{MOI.ScalarAffineFunction{T}}) where {T}
    return "ScalarAffineFunction"
end

function _mof_function_type(::Type{MOI.ScalarQuadraticFunction{T}}) where {T}
    return "ScalarQuadraticFunction"
end

function _mof_function_type(::Type{MOI.ScalarNonlinearFunction})
    return "ScalarNonlinearFunction"
end

function _moi_function_type(::Val{:ScalarAffineFunction}, ::Type{T}) where {T}
    return MOI.ScalarAffineFunction{T}
end

function _moi_function_type(
    ::Val{:ScalarQuadraticFunction},
    ::Type{T},
) where {T}
    return MOI.ScalarQuadraticFunction{T}
end

function _moi_function_type(
    ::Val{:ScalarNonlinearFunction},
    ::Type{T},
) where {T}
    return MOI.ScalarNonlinearFunction
end

function _moi_function_type(name::String, ::Type{T}) where {T}
    return _moi_function_type(Val(Symbol(name)), T)
end

# Encode GenOpt leaves as nonlinear operator nodes, so that the template is an
# ordinary ScalarNonlinearFunction for MOF's writer and reader. Decoding is done
# by `_decode_mof_template`.
function _encode_mof_template(f::MOI.ScalarNonlinearFunction)
    return MOI.ScalarNonlinearFunction(
        f.head,
        Any[_encode_mof_template(arg) for arg in f.args],
    )
end

function _encode_mof_template(index::IteratorIndex)
    return MOI.ScalarNonlinearFunction(:GenOptIteratorIndex, Any[index.value])
end

function _encode_mof_template(array::ContiguousArrayOfVariables)
    return MOI.ScalarNonlinearFunction(
        :GenOptContiguousArrayOfVariables,
        Any[array.offset, array.size...],
    )
end

# A vector of arbitrary variables, for instance when the variables of a model
# are not contiguous.
function _encode_mof_template(array::Vector{MOI.VariableIndex})
    return MOI.ScalarNonlinearFunction(:GenOptVariableArray, Vector{Any}(array))
end

_encode_mof_template(x) = x

function _mof_generator_to_object(
    generator::Union{FunctionGenerator{F},SumGenerator{F}},
    type::String,
    name_map::Dict{MOI.VariableIndex,String},
) where {F}
    template =
        _MOF.moi_to_object(_encode_mof_template(generator.func), name_map)
    return (
        type = type,
        function_type = _mof_function_type(F),
        root = template.root,
        node_list = template.node_list,
        iterators = _mof_iterator_to_object.(generator.iterators),
    )
end

function _MOF.moi_to_object(
    generator::FunctionGenerator,
    name_map::Dict{MOI.VariableIndex,String},
)
    return _mof_generator_to_object(
        generator,
        "GenOptFunctionGenerator",
        name_map,
    )
end

function _MOF.moi_to_object(
    generator::SumGenerator,
    name_map::Dict{MOI.VariableIndex,String},
)
    return _mof_generator_to_object(generator, "GenOptSumGenerator", name_map)
end

function _mof_iterator_value_to_object(value::Tuple)
    return (
        type = "GenOptTuple",
        values = _mof_iterator_value_to_object.(value),
    )
end
function _mof_iterator_value_to_object(value::Symbol)
    return (type = "GenOptSymbol", value = string(value))
end
_mof_iterator_value_to_object(value) = value

function _mof_iterator_to_object(iterator::Iterator{T}) where {T<:Integer}
    values = iterator.values
    if length(values) >= 2
        step = values[2] - values[1]
        if all(i -> values[i] - values[i-1] == step, 2:length(values))
            return (
                type = "GenOptRangeIterator",
                start = first(values),
                step = step,
                length = length(values),
            )
        end
    end
    return _mof_explicit_iterator_to_object(iterator)
end

function _mof_iterator_to_object(iterator::Iterator)
    return _mof_explicit_iterator_to_object(iterator)
end

function _mof_explicit_iterator_to_object(iterator::Iterator)
    return (
        type = "GenOptExplicitIterator",
        values = _mof_iterator_value_to_object.(iterator.values),
    )
end

function _mof_iterator_value_to_moi(value::Dict)
    type = value["type"]
    if type == "GenOptTuple"
        return Tuple(_mof_iterator_value_to_moi.(value["values"]))
    elseif type == "GenOptSymbol"
        return Symbol(value["value"])
    end
    return error("Unsupported GenOpt iterator value: $type")
end

_mof_iterator_value_to_moi(value) = value

function _mof_iterator_to_moi(object::Dict)
    type = object["type"]
    if type == "GenOptRangeIterator"
        values = range(
            object["start"];
            step = object["step"],
            length = object["length"],
        )
        return Iterator(values)
    elseif type == "GenOptExplicitIterator"
        return Iterator(_mof_iterator_value_to_moi.(object["values"]))
    end
    return error("Unsupported GenOpt iterator: $type")
end

function _decode_mof_template(f::MOI.ScalarNonlinearFunction)
    args = Any[_decode_mof_template(arg) for arg in f.args]
    if f.head == :GenOptIteratorIndex
        return IteratorIndex(only(args))
    elseif f.head == :GenOptContiguousArrayOfVariables
        return ContiguousArrayOfVariables(args[1], Tuple(args[2:end]))
    elseif f.head == :GenOptVariableArray
        return MOI.VariableIndex[arg for arg in args]
    end
    return MOI.ScalarNonlinearFunction(f.head, args)
end

_decode_mof_template(x) = x

function _generator_from_mof(
    ::Type{T},
    object::Dict,
    name_map::Dict{String,MOI.VariableIndex},
) where {T}
    # `object` has the `root` and `node_list` of the encoded template.
    template =
        _MOF.function_to_moi(Val(:ScalarNonlinearFunction), T, object, name_map)
    F = _moi_function_type(object["function_type"], T)
    iterators = Iterator[_mof_iterator_to_moi(it) for it in object["iterators"]]
    return F, _decode_mof_template(template), iterators
end

function _MOF.function_to_moi(
    ::Val{:GenOptFunctionGenerator},
    ::Type{T},
    object::Dict,
    name_map::Dict{String,MOI.VariableIndex},
) where {T}
    F, template, iterators = _generator_from_mof(T, object, name_map)
    return FunctionGenerator{F}(template, iterators)
end

function _MOF.function_to_moi(
    ::Val{:GenOptSumGenerator},
    ::Type{T},
    object::Dict,
    name_map::Dict{String,MOI.VariableIndex},
) where {T}
    F, template, iterators = _generator_from_mof(T, object, name_map)
    return SumGenerator{F}(template, iterators)
end
