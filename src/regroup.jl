# Copyright (c) 2024: Benoît Legat and contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

# The symbolic form of one additive term of a constraint function, as returned
# by `MOI.Nonlinear.SymbolicAD._to_symbolic_form`: in the template `f`,
# `MOI.VariableIndex(m)` with `m > 0` is the `m`-th entry of `x` (a variable
# column) and `MOI.VariableIndex(-i)` the `i`-th entry of `data`.
struct _TermInstance
    f::Any
    x::Vector{Int}
    data::Vector{Float64}
end

# A constraint decomposed into its additive terms, keyed by the sign and the
# structure hash of the term. The vector holds the occurrences of that term
# shape in encounter order.
struct _Row{T}
    terms::Dict{Tuple{Bool,UInt64},Vector{_TermInstance}}
    lower::T
    upper::T
end

# A family of constraints whose decompositions have the same set of distinct
# term shapes (up to multiplicity), to be emitted as one `FunctionGenerator`.
struct _Family{T}
    kind::Symbol
    keys::Set{Tuple{Bool,UInt64}}
    rows::Vector{_Row{T}}
end

# Additive decomposition: flatten `+`/`-` (through nested affine and quadratic
# functions) into a list of `(sign, term)`. Affine and quadratic terms are
# emitted in the canonical forms `coef * x` and `coef * x * y` so that the
# same term shape is obtained regardless of the function type it came from.
function _flatten!(
    list::Vector{Tuple{Bool,Any}},
    sign::Bool,
    f::MOI.ScalarAffineFunction,
)
    for t in f.terms
        push!(
            list,
            (
                sign,
                MOI.ScalarNonlinearFunction(:*, Any[t.coefficient, t.variable]),
            ),
        )
    end
    push!(list, (sign, f.constant))
    return
end

function _flatten!(
    list::Vector{Tuple{Bool,Any}},
    sign::Bool,
    f::MOI.ScalarQuadraticFunction,
)
    for t in f.affine_terms
        push!(
            list,
            (
                sign,
                MOI.ScalarNonlinearFunction(:*, Any[t.coefficient, t.variable]),
            ),
        )
    end
    for t in f.quadratic_terms
        # MOI convention: a diagonal `ScalarQuadraticTerm(q, x, x)` contributes
        # `q / 2 * x^2` to the function value
        coef = t.variable_1 == t.variable_2 ? t.coefficient / 2 : t.coefficient
        push!(
            list,
            (
                sign,
                MOI.ScalarNonlinearFunction(
                    :*,
                    Any[coef, t.variable_1, t.variable_2],
                ),
            ),
        )
    end
    push!(list, (sign, f.constant))
    return
end

function _flatten!(
    list::Vector{Tuple{Bool,Any}},
    sign::Bool,
    f::MOI.ScalarNonlinearFunction,
)
    if f.head == :+
        for arg in f.args
            _flatten!(list, sign, arg)
        end
    elseif f.head == :-
        _flatten!(list, length(f.args) == 1 ? !sign : sign, f.args[1])
        for k in 2:length(f.args)
            _flatten!(list, !sign, f.args[k])
        end
    else
        push!(list, (sign, f))
    end
    return
end

_flatten!(list::Vector{Tuple{Bool,Any}}, sign::Bool, f) = push!(list, (sign, f))

# The right-hand side is flattened into the function as an extra `-rhs` term
# so that it becomes part of the per-row data: rows differing only in their
# right-hand side still group. The bounds of an `Interval` are kept in a
# `VectorInterval` instead.
_rhs(set::MOI.EqualTo) = set.value, :zeros
_rhs(set::MOI.LessThan) = set.upper, :nonpositives
_rhs(set::MOI.GreaterThan) = set.lower, :nonnegatives

function _decompose(nlp, variable_to_column, func, set::MOI.Interval, T)
    return _row(
        nlp,
        variable_to_column,
        Tuple{Bool,Any}[(true, func)],
        set.lower,
        set.upper,
        T,
    ),
    :interval
end

function _decompose(nlp, variable_to_column, func, set, T)
    rhs, kind = _rhs(set)
    list = Tuple{Bool,Any}[(true, func), (false, rhs)]
    return _row(nlp, variable_to_column, list, T(-Inf), T(Inf), T), kind
end

function _row(nlp, variable_to_column, list, lower, upper, ::Type{T}) where {T}
    terms = Dict{Tuple{Bool,UInt64},Vector{_TermInstance}}()
    flat = Tuple{Bool,Any}[]
    for (sign, f) in list
        _flatten!(flat, sign, f)
    end
    for (sign, f) in flat
        expr = MOI.Nonlinear.parse_expression(nlp, f)
        sym = MOI.Nonlinear.SymbolicAD._to_symbolic_form(
            nlp,
            expr,
            variable_to_column,
        )
        instance = _TermInstance(sym.f, sym.ordered_variables, sym.data)
        push!(get!(() -> _TermInstance[], terms, (sign, sym.hash)), instance)
    end
    return _Row{T}(terms, lower, upper)
end

# Merge each family into the first family (in decreasing key-set size order)
# whose key set contains it: the terms present in the larger family but not in
# the smaller one simply have zero occurrences in the merged-in rows, which
# makes them varying-multiplicity terms handled by a `FilteredSumGenerator`
# (e.g. the shunt or generator terms of a power balance, absent on some buses).
function _merge_families(families::Vector{_Family{T}}) where {T}
    order =
        sortperm(families; by = f -> (-length(f.keys), sort!(collect(f.keys))))
    merged = _Family{T}[]
    for f in families[order]
        k = findfirst(g -> g.kind == f.kind && f.keys ⊆ g.keys, merged)
        if isnothing(k)
            push!(merged, f)
        else
            append!(merged[k].rows, f.rows)
        end
    end
    return merged
end

# Translate the symbolic template of a term into the `FunctionGenerator`
# encoding produced by the JuMP interface with `container = ParametrizedArray`:
# variable slot `m` becomes `array[row[xoffset + m]]` and data slot `i` becomes
# `row[doffset + i]`, where `row` is the tuple of the (single) iterator.
function _template(f::MOI.ScalarNonlinearFunction, array, xoffset, doffset)
    return MOI.ScalarNonlinearFunction(
        f.head,
        Any[_template(arg, array, xoffset, doffset) for arg in f.args],
    )
end

function _template(v::MOI.VariableIndex, array, xoffset, doffset)
    if v.value > 0
        idx = MOI.ScalarNonlinearFunction(
            :getindex,
            Any[IteratorIndex(1), xoffset+v.value],
        )
        return MOI.ScalarNonlinearFunction(:getindex, Any[array, idx])
    else
        return MOI.ScalarNonlinearFunction(
            :getindex,
            Any[IteratorIndex(1), doffset-v.value],
        )
    end
end

function _signed(template, sign::Bool)
    return sign ? template : MOI.ScalarNonlinearFunction(:-, Any[template])
end

function _generator(family, key, array, outer_iterators, rowid_slot)
    sign, _ = key
    first_instance = nothing
    inner = Any[]
    for (r, row) in enumerate(family.rows)
        for instance in get(() -> _TermInstance[], row.terms, key)
            first_instance = instance
            push!(inner, (instance.x..., instance.data..., r))
        end
    end
    nx = length(first_instance.x)
    template = _signed(_template(first_instance.f, array, 0, nx), sign)
    inner_iterators = Iterator[Iterator(map(identity, inner))]
    inner_rowid = nx + length(first_instance.data) + 1
    filter = FilterExpression(
        :(==),
        Any[
            IteratorValues(inner_iterators, IteratorIndex(1), inner_rowid),
            IteratorValues(outer_iterators, IteratorIndex(1), rowid_slot),
        ],
    )
    return FilteredSumGenerator{MOI.ScalarNonlinearFunction}(
        template,
        inner_iterators,
        filter,
    )
end

function _vector_set(family::_Family{T}) where {T}
    n = length(family.rows)
    if family.kind == :zeros
        return MOI.Zeros(n)
    elseif family.kind == :nonpositives
        return MOI.Nonpositives(n)
    elseif family.kind == :nonnegatives
        return MOI.Nonnegatives(n)
    else
        return VectorInterval(
            [row.lower for row in family.rows],
            [row.upper for row in family.rows],
        )
    end
end

function _add_family(dest, family::_Family, array)
    keys_sorted = sort!(collect(family.keys))
    counts = [
        length(get(() -> _TermInstance[], first(family.rows).terms, key))
        for key in keys_sorted
    ]
    is_base = [
        all(
            length(get(() -> _TermInstance[], row.terms, key)) == counts[k]
            for row in family.rows
        ) for (k, key) in enumerate(keys_sorted)
    ]
    # rows: the slot blocks of the base occurrences, then the row id used by
    # the generator filters
    offsets = Int[]
    offset = 0
    for (k, key) in enumerate(keys_sorted)
        is_base[k] || continue
        instance = first(first(family.rows).terms[key])
        for _ in 1:counts[k]
            push!(offsets, offset)
            offset += length(instance.x) + length(instance.data)
        end
    end
    rowid_slot = offset + 1
    rows = Any[]
    for row in family.rows
        tup = ()
        for (k, key) in enumerate(keys_sorted)
            is_base[k] || continue
            for instance in row.terms[key]
                tup = (tup..., instance.x..., instance.data...)
            end
        end
        push!(rows, (tup..., length(rows) + 1))
    end
    outer_iterators = Iterator[Iterator(map(identity, rows))]
    args = Any[]
    i = 0
    for (k, key) in enumerate(keys_sorted)
        sign, _ = key
        if is_base[k]
            for _ in 1:counts[k]
                i += 1
                instance = first(first(family.rows).terms[key])
                xoffset = offsets[i]
                doffset = offsets[i] + length(instance.x)
                push!(
                    args,
                    _signed(
                        _template(instance.f, array, xoffset, doffset),
                        sign,
                    ),
                )
            end
        else
            push!(
                args,
                _generator(family, key, array, outer_iterators, rowid_slot),
            )
        end
    end
    generator = FunctionGenerator{MOI.ScalarNonlinearFunction}(
        MOI.ScalarNonlinearFunction(:+, args),
        outer_iterators,
    )
    return MOI.add_constraint(dest, generator, _vector_set(family))
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
scalar constraints sharing the same set of additive term shapes, with the
per-row variables and coefficients stored as tuples in the
[`Iterator`](@ref) data. The result is the model that the JuMP interface
would have produced had the constraints been written with
`container = ParametrizedArray` and `lazy_sum`.

Each function is split into its additive terms and each term is converted to
a symbolic template with `MOI.Nonlinear.SymbolicAD._to_symbolic_form` (the
symbolic templating that MathOptSymbolicAD.jl was built on): the leaves of
the template are variable and data slots, and a hash identifies the term
structure. Constraints with the same set of distinct term shapes (and set
kind) form one family; a family whose term-shape set is contained in the set
of another family is merged into it. Within a family, term shapes occurring
the same number of times in every row are inlined into the base template,
while term shapes with a row-dependent number of occurrences (e.g. the
`sum(p[a] for a in arcs(i))` of a power balance, or terms that JuMP dropped
on the rows where their coefficient is zero) become
[`FilteredSumGenerator`](@ref) arguments whose filter matches the inner rows
to the constraint row, exactly like `lazy_sum(... for j in ... if bus[j] == i)`.

Right-hand sides are moved into the function (becoming data), so rows
differing only in their bounds group together; `Interval` bounds are kept in
a per-row [`VectorInterval`](@ref). Variable bound and integrality
constraints (`MOI.VariableIndex` constraints), constraints in other set
types, the objective and the variable starting values are copied over
unchanged.
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
    families = _Family{T}[]
    family_index = Dict{Tuple{Symbol,Vector{Tuple{Bool,UInt64}}},Int}()
    for (F, S) in MOI.get(src, MOI.ListOfConstraintTypesPresent())
        grouped = F != MOI.VariableIndex && S <: _GROUPED_SET_TYPES{T}
        for ci in MOI.get(src, MOI.ListOfConstraintIndices{F,S}())
            func = MOI.get(src, MOI.ConstraintFunction(), ci)
            set = MOI.get(src, MOI.ConstraintSet(), ci)
            if grouped
                row, kind = _decompose(nlp, variable_to_column, func, set, T)
                key = (kind, sort!(collect(keys(row.terms))))
                k = get!(family_index, key) do
                    push!(families, _Family{T}(kind, Set(key[2]), _Row{T}[]))
                    return length(families)
                end
                push!(families[k].rows, row)
            else
                MOI.add_constraint(
                    dest,
                    MOI.Utilities.map_indices(
                        Base.Fix1(getindex, varmap),
                        func,
                    ),
                    copy(set),
                )
            end
        end
    end
    array = ContiguousArrayOfVariables(0, (length(vis_dest),))
    for family in _merge_families(families)
        _add_family(dest, family, array)
    end
    return dest
end
