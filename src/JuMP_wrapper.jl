import JuMP

"""
    struct IteratorInExpr
        iterator::Iterator
        index::Int
    end


"""
struct IteratorInExpr
    iterator::Iterator
    index::Int
end

function Base.show(io::IO, i::IteratorInExpr)
    print(io, values_at(i.iterator, i.index))
    print(io, "[i]")
    return
end

JuMP._is_real(::Union{IteratorInExpr,IteratorIndex}) = true
JuMP.moi_function(i::Union{IteratorInExpr,IteratorIndex}) = i
JuMP.jump_function(_, i::Union{IteratorInExpr,IteratorIndex}) = i

function prepare(it::IteratorValues)
    append!(it.iterator.values, it.values)
    return IteratorInExpr(it.iterator, num_values(it.iterator))
end

for f in _MULTIVARIATE_OPERATORS
    op = Meta.quot(f)
    @eval begin
        function Base.$(f)(it::IteratorValues, y::JuMP.AbstractJuMPScalar)
            return JuMP.GenericNonlinearExpr{JuMP.variable_ref_type(x)}(
                $op,
                prepare(it),
                y,
            )
        end
        function Base.$(f)(x::JuMP.AbstractJuMPScalar, it::IteratorValues)
            return JuMP.GenericNonlinearExpr{JuMP.variable_ref_type(x)}(
                $op,
                x,
                prepare(it),
            )
        end
    end
end

struct IteratedExpr{V<:JuMP.AbstractVariableRef} <: AbstractVector{JuMP.GenericNonlinearExpr{V}}
    expr::JuMP.GenericNonlinearExpr{V}
    iterators::Vector{Iterator}
end

function Base.show(io::IO, f::IteratedExpr)
    return print(io, JuMP.function_string(MIME("text/plain"), f))
end

function Base.show(io::IO, mime::MIME"text/latex", f::IteratedExpr)
    str = JuMP.function_string(mime, f)
    str = JuMP._wrap_in_inline_math_mode(str)
    return print(io, str)
end

function Base.show(io::IO, mime::MIME"text/plain", f::IteratedExpr)
    str = JuMP.function_string(mime, f)
    return print(io, str)
end

function Base.show(io::IO, ::MIME, f::IteratedExpr)
    return show(io, MIME"text/plain"(), f)
end

function JuMP.function_string(mime, a::IteratedExpr)
    str = JuMP.function_string(mime, a.expr)
    for iter in a.iterators
        str *= ", "
        str *= string(iter)
    end
    return str
end

function JuMP.moi_function(f::IteratedExpr)
    return IteratedFunction(
        JuMP.moi_function(f.expr),
        f.iterators,
    )
end

function JuMP.jump_function(model, f::IteratedFunction)
    return IteratedExpr(
        JuMP.jump_function(model, f.func),
        f.iterators,
    )
end

function Base.:-(expr::IteratedExpr, α::Number)
    return IteratedExpr(expr.expr - α, expr.iterators)
end

_size(expr::IteratedExpr) = getfield.(expr.iterators, :length)

index_iterators(::Vector, func, _) = func

function index_iterators(iterators::Vector, func::IteratorInExpr, index)
    return values_at(iterators[func.iterator_index], func.value_index)[index[func.iterator_index]]
end

function index_iterators(iterators::Vector, func::JuMP.GenericNonlinearExpr, index)
    return GenericNonlinearExpr(
        func.head,
        map(Base.Fix1(index_iterators, iterators), func.args)
    )
end

function Base.getindex(expr::IteratedExpr, i::Integer)
    idx = CartesianIndices(Base.OneTo.(_size(expr)))[i]
    return index_iterators(expr.iterators, expr.expr, idx)
end

Base.length(expr::IteratedExpr) = prod(_size(expr))

struct ParametrizedArray
    constraint
    iterators
end

function Base.show(io::IO, mime::MIME"text/latex", a::ParametrizedArray)
    return show(io, a)
end

function JuMP.Containers.container(
    f::Function,
    indices::JuMP.Containers.VectorizedProductIterator,
    ::Type{ParametrizedArray},
)
    its = iterator.(indices.prod.iterators)
    ParametrizedArray(f(its...), its)
end

collect_iterators!(_::Vector, func) = func

function collect_iterators!(iterators::Vector, func::IteratorInExpr)
    push!(iterators, func.iterator)
    return IteratorIndex(length(iterators), func.index)
end

function collect_iterators!(iterators::Vector, func::JuMP.GenericNonlinearExpr)
    return JuMP.GenericNonlinearExpr(
        func.head,
        map(Base.Fix1(collect_iterators!, iterators), func.args)
    )
end

function collect_iterators(func::JuMP.GenericNonlinearExpr)
    iterators = Iterator[]
    new_func = collect_iterators!(iterators, func)
    if isempty(iterators)
        return func
    else
        return IteratedExpr(new_func, iterators)
    end
end

function JuMP.build_constraint(
    error_fn::Function,
    func::JuMP.GenericNonlinearExpr,
    set::MOI.Utilities.ScalarLinearSet,
)
    new_func = collect_iterators(func)
    if new_func isa IteratedExpr
        new_func -= MOI.constant(set)
        S = MOI.Utilities.vector_set_type(typeof(set))
        vector_set = S(length(new_func))
        return JuMP.build_constraint(error_fn, new_func, vector_set)
    else
        return JuMP.build_constraint(error_fn, func, set)
    end
end

struct IteratedConstraint{
    V<:JuMP.GenericVariableRef,
    S<:MOI.AbstractVectorSet,
} <: JuMP.AbstractConstraint
    func::IteratedExpr{V}
    set::S
end

JuMP.shape(::IteratedConstraint) = JuMP.VectorShape()

JuMP.reshape_vector(f::IteratedExpr, ::JuMP.VectorShape) = f

function JuMP.check_belongs_to_model(con::IteratedConstraint, model)
    return JuMP.check_belongs_to_model(con.func.expr, model)
end

function JuMP.build_constraint(::Function, expr::IteratedExpr, set::MOI.AbstractVectorSet)
    return IteratedConstraint(expr, set)
end

function JuMP.constraint_object(
    con_ref::JuMP.ConstraintRef{
        <:JuMP.AbstractModel,
        MOI.ConstraintIndex{FuncType,SetType},
    },
) where {FuncType<:IteratedFunction,SetType<:MOI.AbstractVectorSet}
    model = con_ref.model
    f = MOI.get(model, MOI.ConstraintFunction(), con_ref)::FuncType
    s = MOI.get(model, MOI.ConstraintSet(), con_ref)::SetType
    return IteratedConstraint(JuMP.jump_function(model, f), s)
end
