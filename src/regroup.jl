# Copyright (c) 2024: Benoît Legat and contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

# A group of scalar constraints sharing the same expression structure, in
# construction. `f` is the symbolic template of the first member (see
# `MOI.Nonlinear.SymbolicAD._to_symbolic_form`): `MOI.VariableIndex(m)` with
# `m > 0` is the `m`-th variable slot and `MOI.VariableIndex(-i)` the `i`-th
# data slot of each row tuple. `nx` is the number of variable slots. `lower`
# and `upper` are only filled for `kind == :interval`.
mutable struct _Group{T}
    kind::Symbol
    f::MOI.ScalarNonlinearFunction
    nx::Int
    rows::Vector{Any}
    lower::Vector{T}
    upper::Vector{T}
end

# Move the right-hand side into the function so that it becomes part of the
# per-row data: rows differing only in their right-hand side still group.
_shifted(func, set::MOI.EqualTo) =
    MOI.ScalarNonlinearFunction(:-, Any[func, set.value]), :zeros
_shifted(func, set::MOI.LessThan) =
    MOI.ScalarNonlinearFunction(:-, Any[func, set.upper]), :nonpositives
_shifted(func, set::MOI.GreaterThan) =
    MOI.ScalarNonlinearFunction(:-, Any[func, set.lower]), :nonnegatives
# The bounds of an `Interval` go to a `VectorInterval` instead.
_shifted(func, ::MOI.Interval) = func, :interval

function _push_row!(
    groups::Vector{_Group{T}},
    group_index,
    nlp,
    variable_to_column,
    func,
    set,
) where {T}
    shifted, kind = _shifted(func, set)
    expr = MOI.Nonlinear.parse_expression(nlp, shifted)
    sym = MOI.Nonlinear.SymbolicAD._to_symbolic_form(nlp, expr, variable_to_column)
    row = (sym.ordered_variables..., sym.data...)
    k = get!(group_index, (kind, sym.hash)) do
        push!(
            groups,
            _Group{T}(kind, sym.f, length(sym.ordered_variables), Any[], T[], T[]),
        )
        return length(groups)
    end
    push!(groups[k].rows, row)
    if kind == :interval
        push!(groups[k].lower, set.lower)
        push!(groups[k].upper, set.upper)
    end
    return
end

# Translate a symbolic template into the `FunctionGenerator` encoding produced
# by the JuMP interface with `container = ParametrizedArray`: variable slot `m`
# becomes `array[row[m]]` and data slot `i` becomes `row[nx + i]`, where `row`
# is the tuple of the (single) iterator.
function _template(f::MOI.ScalarNonlinearFunction, array, nx)
    return MOI.ScalarNonlinearFunction(
        f.head,
        Any[_template(arg, array, nx) for arg in f.args],
    )
end

function _template(v::MOI.VariableIndex, array, nx)
    if v.value > 0
        idx = MOI.ScalarNonlinearFunction(:getindex, Any[IteratorIndex(1), v.value])
        return MOI.ScalarNonlinearFunction(:getindex, Any[array, idx])
    else
        return MOI.ScalarNonlinearFunction(
            :getindex,
            Any[IteratorIndex(1), nx - v.value],
        )
    end
end

function _vector_set(group::_Group)
    n = length(group.rows)
    if group.kind == :zeros
        return MOI.Zeros(n)
    elseif group.kind == :nonpositives
        return MOI.Nonpositives(n)
    elseif group.kind == :nonnegatives
        return MOI.Nonnegatives(n)
    else
        return VectorInterval(group.lower, group.upper)
    end
end

const _GROUPED_SET_TYPES{T} =
    Union{MOI.EqualTo{T},MOI.LessThan{T},MOI.GreaterThan{T},MOI.Interval{T}}

"""
    regroup(src::MOI.ModelLike; T::Type = Float64)

Return a `MOI.Utilities.UniversalFallback` model equivalent to `src` in which
the scalar function constraints (`ScalarAffineFunction`,
`ScalarQuadraticFunction` or `ScalarNonlinearFunction` in `EqualTo`,
`LessThan`, `GreaterThan` or `Interval`) are grouped into
[`FunctionGenerator`](@ref) constraints: one vector constraint per family of
scalar constraints sharing the same expression structure, with the per-row
variables and coefficients stored as tuples in the [`Iterator`](@ref) data.
The result is the model that the JuMP interface would have produced had the
constraints been written with `container = ParametrizedArray`.

The structure detection reuses `MOI.Nonlinear.SymbolicAD._to_symbolic_form`
(the symbolic templating that MathOptSymbolicAD.jl was built on): each
function is parsed into a `MOI.Nonlinear` expression and converted to a
template whose leaves are variable and data slots, together with a hash of
the structure. Constraints with the same hash and set kind form one group.
Right-hand sides are moved into the function (becoming data), so rows
differing only in their bounds group together; `Interval` bounds are kept in
a per-row [`VectorInterval`](@ref).

Variable bound and integrality constraints (`MOI.VariableIndex` constraints),
constraints in other set types, the objective and the variable starting
values are copied over unchanged.
"""
function regroup(src::MOI.ModelLike; T::Type = Float64)
    dest = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{T}())
    vis_src = MOI.get(src, MOI.ListOfVariableIndices())
    vis_dest = MOI.add_variables(dest, length(vis_src))
    varmap = Dict(zip(vis_src, vis_dest))
    if MOI.supports(src, MOI.VariablePrimalStart(), MOI.VariableIndex)
        for vi in vis_src
            start = MOI.get(src, MOI.VariablePrimalStart(), vi)
            if !isnothing(start)
                MOI.set(dest, MOI.VariablePrimalStart(), varmap[vi], start)
            end
        end
    end
    sense = MOI.get(src, MOI.ObjectiveSense())
    MOI.set(dest, MOI.ObjectiveSense(), sense)
    if sense != MOI.FEASIBILITY_SENSE
        F = MOI.get(src, MOI.ObjectiveFunctionType())
        obj = MOI.get(src, MOI.ObjectiveFunction{F}())
        MOI.set(
            dest,
            MOI.ObjectiveFunction{F}(),
            MOI.Utilities.map_indices(Base.Fix1(getindex, varmap), obj),
        )
    end
    nlp = MOI.Nonlinear.Model()
    variable_to_column =
        Dict{Int64,Int}(vi.value => varmap[vi].value for vi in vis_src)
    groups = _Group{T}[]
    group_index = Dict{Tuple{Symbol,UInt64},Int}()
    for (F, S) in MOI.get(src, MOI.ListOfConstraintTypesPresent())
        grouped = F != MOI.VariableIndex && S <: _GROUPED_SET_TYPES{T}
        for ci in MOI.get(src, MOI.ListOfConstraintIndices{F,S}())
            func = MOI.get(src, MOI.ConstraintFunction(), ci)
            set = MOI.get(src, MOI.ConstraintSet(), ci)
            if grouped
                _push_row!(groups, group_index, nlp, variable_to_column, func, set)
            else
                MOI.add_constraint(
                    dest,
                    MOI.Utilities.map_indices(Base.Fix1(getindex, varmap), func),
                    copy(set),
                )
            end
        end
    end
    array = ContiguousArrayOfVariables(0, (length(vis_dest),))
    for group in groups
        generator = FunctionGenerator{MOI.ScalarNonlinearFunction}(
            _template(group.f, array, group.nx),
            Iterator[Iterator(map(identity, group.rows))],
        )
        MOI.add_constraint(dest, generator, _vector_set(group))
    end
    return dest
end
