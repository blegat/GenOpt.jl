function Base.show(io::IO, f::Union{ExprGenerator,ExprTemplate})
    return print(io, JuMP.function_string(MIME("text/plain"), f))
end

function Base.show(io::IO, mime::MIME"text/latex", f::Union{ExprGenerator,ExprTemplate})
    str = JuMP.function_string(mime, f)
    str = JuMP._wrap_in_inline_math_mode(str)
    return print(io, str)
end

function Base.show(io::IO, mime::MIME"text/plain", f::Union{ExprGenerator,ExprTemplate})
    str = JuMP.function_string(mime, f)
    return print(io, str)
end

function Base.show(io::IO, ::MIME, f::Union{ExprTemplate,ExprGenerator})
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

function JuMP.function_string(mime, a::ExprGenerator)
    return JuMP.function_string(mime, a.expr)
end

function Base.show(io::IO, ::MIME"text/latex", a::ParametrizedArray)
    return show(io, a)
end
