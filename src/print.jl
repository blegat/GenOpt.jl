# Copyright (c) 2024: Benoît Legat and contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

function Base.show(io::IO, i::IteratorInExpr)
    print(io, i.iterators[i.index.value].values)
    print(io, "[i]")
    return
end

function Base.show(io::IO, f::Union{ExprGenerator,ExprTemplate,LazySum})
    return print(io, JuMP.function_string(MIME("text/plain"), f))
end

function Base.show(
    io::IO,
    mime::MIME"text/latex",
    f::Union{ExprGenerator,ExprTemplate,LazySum},
)
    str = JuMP.function_string(mime, f)
    str = JuMP._wrap_in_inline_math_mode(str)
    return print(io, str)
end

function Base.show(
    io::IO,
    mime::MIME"text/plain",
    f::Union{ExprGenerator,ExprTemplate,LazySum},
)
    str = JuMP.function_string(mime, f)
    return print(io, str)
end

function Base.show(io::IO, ::MIME, f::Union{ExprTemplate,ExprGenerator,LazySum})
    return show(io, MIME"text/plain"(), f)
end

function JuMP.function_string(mime, a::ExprTemplate)
    str = JuMP.function_string(mime, a.expr)
    for iter in a.iterators
        str *= ", "
        str *= string(iter)
    end
    return str
end

function JuMP.function_string(mime, a::LazySum)
    str = "sum("
    str *= JuMP.function_string(mime, a.expr)
    for iter in a.iterators
        str *= ", "
        str *= string(iter)
    end
    str *= ")"
    return str
end

function JuMP.function_string(mime, a::ExprGenerator)
    return JuMP.function_string(mime, a.expr)
end

function Base.show(io::IO, ::MIME"text/latex", a::ParametrizedArray)
    return show(io, a)
end

function Base.show(io::IO, ::MIME"text/plain", v::ArrayOfVariables)
    return println(io, Base.summary(v), " with offset ", v.offset)
end

function Base.show(io::IO, v::ArrayOfVariables)
    return show(io, MIME"text/plain"(), v)
end

# ---------------------------------------------------------------------------
# MOI-level printing of a whole family at once
#
# A `FunctionGenerator` (a family of constraints) and a `SumGenerator` (a sum)
# each store a single *template* plus the iterators it ranges over. MOI's
# generic printing of a vector function calls `scalarize`, i.e. it prints the
# expanded family, one row per index. The methods below print the template
# once instead, followed by the domains it is quantified over.
# ---------------------------------------------------------------------------

# Names given to the iterators of a generator, in order.
const _INDEX_NAMES = ["i", "j", "k", "l", "m", "n"]

_default_index_name(k::Integer) = get(_INDEX_NAMES, k, "i_$(k)")

# Names for the `n` iterators of a generator. A nested generator passes the
# names already in scope as `taken` so that it cannot shadow an enclosing one.
function _index_names(n::Integer, taken = String[])
    names = String[]
    k = 0
    while length(names) < n
        k += 1
        name = _default_index_name(k)
        if !(name in taken)
            push!(names, name)
        end
    end
    return names
end

const _SUBSCRIPT_DIGITS = ('₀', '₁', '₂', '₃', '₄', '₅', '₆', '₇', '₈', '₉')

_subscript(n::Integer) = join(_SUBSCRIPT_DIGITS[d-'0'+1] for d in string(n))

_is_zero(x) = x isa Real && iszero(x)
_is_one(x) = x isa Real && isone(x)

# Drop the `- 0` and `* 1` that building a template leaves behind, so that the
# printed form matches what the user wrote.
_simplify(x) = x
function _simplify(f::MOI.ScalarNonlinearFunction)
    args = Any[_simplify(arg) for arg in f.args]
    if length(args) == 2
        if f.head == :+
            _is_zero(args[1]) && return args[2]
            _is_zero(args[2]) && return args[1]
        elseif f.head == :-
            _is_zero(args[2]) && return args[1]
        elseif f.head == :*
            _is_one(args[1]) && return args[2]
            _is_one(args[2]) && return args[1]
        end
    end
    return MOI.ScalarNonlinearFunction(f.head, args)
end

const _INFIX = Dict(:+ => " + ", :- => " - ", :* => "*", :/ => "/", :^ => "^")

# Binding strength, used to add parentheses only where they are needed.
_precedence(::Any) = 4
_precedence(::SumGenerator) = 1
function _precedence(f::MOI.ScalarNonlinearFunction)
    if f.head in (:+, :-)
        return 1
    elseif f.head in (:*, :/)
        return 2
    elseif f.head == :^
        return 3
    end
    return 4
end

# `names[k]` is the name printed for `IteratorIndex(k)` in the current scope.
function _template_string(options, model, x, names)
    return MOI.Utilities._to_string(options, model, x)
end

_template_string(options, model, x::Bool, names) = string(Int(x))

# An array index or an exponent reads better bare than through MOI's coefficient
# shortening, which parenthesizes anything that is not a `Float64`.
_template_string(options, model, x::Integer, names) = string(x)

function _template_string(options, model, index::IteratorIndex, names)
    return get(names, index.value, _default_index_name(index.value))
end

function _template_string(options, model, f::SumGenerator, names)
    inner = _index_names(length(f.iterators), names)
    body = _template_string(options, model, _simplify(f.func), inner)
    domains = map(zip(inner, f.iterators)) do (name, iterator)
        return string(name, " ", _in_string(options), " ", _domain_string(iterator))
    end
    return string("∑_{", join(domains, ", "), "} ", body)
end

function _template_string(options, model, f::MOI.ScalarNonlinearFunction, names)
    if f.head == :getindex
        return _getindex_string(options, model, f.args, names)
    elseif length(f.args) == 2 && haskey(_INFIX, f.head)
        precedence = _precedence(f)
        return string(
            _operand_string(
                options,
                model,
                f.args[1],
                names,
                precedence,
                false,
            ),
            _INFIX[f.head],
            # `-`, `/` and `^` are not associative, so an operand of the same
            # strength on the right still needs parentheses: `a - (b - c)`.
            _operand_string(
                options,
                model,
                f.args[2],
                names,
                precedence,
                f.head in (:-, :/, :^),
            ),
        )
    elseif length(f.args) == 1 && f.head in (:+, :-)
        operand = _operand_string(options, model, f.args[1], names, 3, false)
        return string(f.head, operand)
    end
    args = [_template_string(options, model, arg, names) for arg in f.args]
    return string(f.head, "(", join(args, ", "), ")")
end

function _operand_string(
    options,
    model,
    arg,
    names,
    precedence,
    non_associative,
)
    str = _template_string(options, model, arg, names)
    arg_precedence = _precedence(arg)
    if arg_precedence < precedence ||
       (non_associative && arg_precedence == precedence)
        return string("(", str, ")")
    end
    return str
end

function _getindex_string(options, model, args, names)
    collection = args[1]
    if collection isa IteratorIndex
        name = _template_string(options, model, collection, names)
        column = args[2]
        if column isa Integer
            # Column 1 is the iterator's own value; a later column holds data
            # mapped through it, whose original name is not recoverable.
            return column == 1 ? name :
                   string("p", _subscript(column), "[", name, "]")
        end
        index = _template_string(options, model, column, names)
        return string(name, "[", index, "]")
    end
    indices = map(args[2:end]) do arg
        return _template_string(options, model, arg, names)
    end
    return string(_array_name(model, collection), "[", join(indices, ","), "]")
end

# The array's own name, recovered from the name of its first variable
# (`x[1,1]` gives `x`). Printing a function on its own goes through a model
# with no names, in which case the array stays an anonymous `X`.
function _array_name(model, array::ContiguousArrayOfVariables)
    index = MOI.VariableIndex(array.offset + 1)
    name = MOI.get(model, MOI.VariableName(), index)
    matched = match(r"^(.+)\[.*\]$", name)
    return matched === nothing ? "X" : matched.captures[1]
end

_in_string(options) = MOI.Utilities._to_string(options, in)

_forall_string(::MOI.Utilities._PrintOptions) = "∀ "
_forall_string(::MOI.Utilities._PrintOptions{MIME"text/latex"}) = "\\forall "

# The values an iterator takes, i.e. the first entry of each of its tuples.
# Long domains are elided; the whole point is to stay on one line.
function _domain_string(iterator::Iterator)
    values = [string(first(value)) for value in iterator.values]
    if length(values) > 6
        values = [values[1:3]; "…"; values[end]]
    end
    return string("{", join(values, ", "), "}")
end

function _quantifier_string(options, iterators, names)
    domains = map(zip(names, iterators)) do (name, iterator)
        return string(name, " ", _in_string(options), " ", _domain_string(iterator))
    end
    return string(_forall_string(options), join(domains, ", "))
end

function MOI.Utilities._to_string(
    options::MOI.Utilities._PrintOptions,
    model::MOI.ModelLike,
    array::ContiguousArrayOfVariables,
)
    return _array_name(model, array)
end

function MOI.Utilities._to_string(
    options::MOI.Utilities._PrintOptions,
    model::MOI.ModelLike,
    index::IteratorIndex,
)
    return _default_index_name(index.value)
end

function MOI.Utilities._to_string(
    options::MOI.Utilities._PrintOptions,
    model::MOI.ModelLike,
    f::SumGenerator,
)
    names = _index_names(length(f.iterators))
    return _template_string(options, model, f, names)
end

function MOI.Utilities._to_string(
    options::MOI.Utilities._PrintOptions,
    model::MOI.ModelLike,
    f::FunctionGenerator,
)
    names = _index_names(length(f.iterators))
    return string(
        _template_string(options, model, _simplify(f.func), names),
        "  ",
        _quantifier_string(options, f.iterators, names),
    )
end

# Every element of the family is constrained the same way, so the group prints
# with the scalar comparison its rows share rather than with the vector set.
function _elementwise_set_string(options, set::MOI.AbstractSet)
    return MOI.Utilities._to_string(options, set)
end

function _elementwise_set_string(options, ::MOI.Nonnegatives)
    return MOI.Utilities._to_string(options, MOI.GreaterThan(0.0))
end

function _elementwise_set_string(options, ::MOI.Nonpositives)
    return MOI.Utilities._to_string(options, MOI.LessThan(0.0))
end

function _elementwise_set_string(options, ::MOI.Zeros)
    return MOI.Utilities._to_string(options, MOI.EqualTo(0.0))
end

# The set has to come before the `∀` clause, so the whole constraint is
# rendered here instead of letting MOI place the function and the set.
function MOI.Utilities._to_string(
    options::MOI.Utilities._PrintOptions,
    model::MOI.ModelLike,
    cref::MOI.ConstraintIndex{<:FunctionGenerator},
)
    f = MOI.get(model, MOI.ConstraintFunction(), cref)
    set = MOI.get(model, MOI.ConstraintSet(), cref)
    names = _index_names(length(f.iterators))
    return string(
        _template_string(options, model, _simplify(f.func), names),
        " ",
        _elementwise_set_string(options, set),
        "  ",
        _quantifier_string(options, f.iterators, names),
        MOI.Utilities._name_suffix(options, model, cref),
    )
end
