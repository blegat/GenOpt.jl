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
    println(io, Base.summary(v), " with offset ", v.offset)
end

function Base.show(io::IO, v::ArrayOfVariables)
    return show(io, MIME"text/plain"(), v)
end
