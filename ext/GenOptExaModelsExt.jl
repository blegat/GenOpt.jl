# Copyright (c) 2024: Benoît Legat and contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module GenOptExaModelsExt

import ExaModels
import GenOpt:
    FunctionGenerator, SumGenerator, ContiguousArrayOfVariables, IteratorIndex
import MathOptInterface as MOI

# The triggers of this extension are a strict superset of the ones of
# `ExaModelsMOI` so `ExaModelsMOI` is precompiled and loaded before this one,
# see https://github.com/JuliaLang/julia/pull/56368 and
# https://github.com/JuliaLang/julia/pull/49891
const ExaModelsMOI = Base.get_extension(ExaModels, :ExaModelsMOI)

# Objective

function MOI.supports(
    ::ExaModelsMOI.Optimizer,
    ::MOI.ObjectiveFunction{<:SumGenerator},
)
    return true
end

function MOI.set(
    model::ExaModelsMOI.Optimizer,
    ::MOI.ObjectiveFunction{F},
    f::F,
) where {F<:SumGenerator}
    empty!(model.objs)
    ExaModelsMOI.update_bin!(model.objs, ExaModelsMOI.ObjectiveBin(), f)
    return
end

# This is also called for each `SumGenerator` of a `+` in the objective
function ExaModelsMOI.update_bin!(
    bins::Vector{ExaModelsMOI.Bin},
    ::ExaModelsMOI.ObjectiveBin,
    f::SumGenerator,
)
    lengths = map(it -> length(first(it.values)), f.iterators)
    if length(lengths) == 1 && lengths[] == 1
        bin = ExaModelsMOI.Bin(
            exagen(f.func, nothing),
            only.(f.iterators[].values),
        )
    else
        bin = ExaModelsMOI.Bin(
            exagen(f.func, _offsets(lengths)),
            _data(f.iterators),
        )
    end
    push!(bins, bin)
    return bins
end

# Constraints

const _Sets = Union{MOI.Zeros,MOI.Nonnegatives,MOI.Nonpositives}

function MOI.supports_constraint(
    ::ExaModelsMOI.Optimizer,
    ::Type{<:FunctionGenerator},
    ::Type{<:_Sets},
)
    return true
end

_bounds(::MOI.Zeros, ::Type{T}) where {T} = (zero(T), zero(T))
_bounds(::MOI.Nonnegatives, ::Type{T}) where {T} = (zero(T), typemax(T))
_bounds(::MOI.Nonpositives, ::Type{T}) where {T} = (typemin(T), zero(T))

function MOI.add_constraint(
    model::ExaModelsMOI.Optimizer{T},
    f::FunctionGenerator,
    s::_Sets,
) where {T}
    row = length(model.lcon) + 1
    lengths = map(it -> length(first(it.values)), f.iterators)
    # The row index is appended to each element of the data
    row_expr = ExaModels.DataIndexed(ExaModels.DataSource(), sum(lengths) + 1)
    data = [(d..., row + i - 1) for (i, d) in enumerate(_data(f.iterators))]
    head = row_expr => exagen(f.func, _offsets(lengths))
    push!(model.cons, ExaModelsMOI.Bin(head, data))
    l, u = _bounds(s, T)
    append!(model.lcon, fill(l, MOI.dimension(s)))
    append!(model.ucon, fill(u, MOI.dimension(s)))
    return MOI.ConstraintIndex{typeof(f),typeof(s)}(row)
end

# Convert GenOpt expression trees to ExaModels format

_offsets(lengths) = [0; cumsum(lengths)[1:(end-1)]]

function _data(iterators)
    values = ntuple(i -> iterators[i].values, length(iterators))
    return vec(map(Base.Iterators.ProductIterator(values)) do I
        return reduce((i, j) -> tuple(i..., j...), I)
    end)
end

exagen(α::Number, _) = α

function exagen(f::MOI.ScalarNonlinearFunction, offsets)
    if f.head != :getindex
        # This assumes that we support only the default functions in
        # `MOI.Nonlinear`, like `ExaModelsMOI._exafy`
        op = getfield(MOI.Nonlinear, f.head)
        return op((exagen(e, offsets) for e in f.args)...)
    end
    v = f.args[1]
    if v isa ContiguousArrayOfVariables
        # `ExaModelsMOI` uses the MOI variable index as ExaModels index
        idx = exagen(f.args[2], offsets)
        if !iszero(v.offset)
            idx = v.offset + idx
        end
        cp = cumprod(v.size)
        for i in 3:length(f.args)
            idx += cp[i-2] * (exagen(f.args[i], offsets) - 1)
        end
        return ExaModels.Var(idx)
    elseif v isa IteratorIndex
        @assert length(f.args) == 2
        @assert f.args[2] isa Integer
        if isnothing(offsets)
            @assert isone(f.args[2])
            return ExaModels.DataSource()
        end
        return ExaModels.DataIndexed(
            ExaModels.DataSource(),
            offsets[v.value] + f.args[2],
        )
    end
    return error(
        "Unexpected the first operand of `getindex` to be of type " *
        "`$(typeof(v))`",
    )
end

end # module
