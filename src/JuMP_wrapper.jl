# Copyright (c) 2024: Benoît Legat and contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

# TODO move to a package extension depending on JuMP
import JuMP

include("operators.jl")

"""
    struct IteratorInExpr
        iterators::Iterators
        index::IteratorIndex
    end

Iterator `iterators[index.value]`.
"""
struct IteratorInExpr
    iterators::Iterators
    index::IteratorIndex
end

Base.copy(it::IteratorInExpr) = it

JuMP._is_real(::Union{IteratorInExpr,IteratorIndex}) = true
JuMP.moi_function(i::Union{IteratorInExpr,IteratorIndex}) = i
JuMP.jump_function(_, i::Union{IteratorInExpr,IteratorIndex}) = i

struct ExprGenerator{E,V<:JuMP.AbstractVariableRef} <:
       AbstractVector{JuMP.GenericNonlinearExpr{V}}
    expr::ExprTemplate{E,V}
end

function JuMP.moi_function(f::ExprGenerator{E}) where {E}
    return FunctionGenerator{JuMP.moi_function_type(E)}(
        JuMP.moi_function(f.expr.expr),
        f.expr.iterators,
    )
end

function JuMP.jump_function(model, f::FunctionGenerator{F}) where {F}
    return ExprGenerator(
        ExprTemplate{JuMP.jump_function_type(model, F)}(
            JuMP.jump_function(model, f.func),
            f.iterators,
        ),
    )
end

_size(expr::ExprGenerator) = length.(getfield.(expr.expr.iterators, :values))

index_iterators(func, _) = func

function index_iterators(func::IteratorInExpr, index)
    idx = func.index
    return func.iterators[idx.value].values[index[idx.value]]
end

function index_iterators(func::JuMP.GenericNonlinearExpr, index)
    args = map(Base.Fix2(index_iterators, index), func.args)
    if any(JuMP._has_variable_ref_type, args)
        return JuMP.GenericNonlinearExpr(func.head, args)
    else
        registry = MOI.Nonlinear.OperatorRegistry()
        if length(func.args) == 1
            MOI.Nonlinear.eval_univariate_function(registry, func.head, args[])
        else
            MOI.Nonlinear.eval_multivariate_function(registry, func.head, args)
        end
    end
end

function Base.getindex(expr::ExprGenerator, i::Integer)
    idx = CartesianIndices(Base.OneTo.(_size(expr)))[i]
    return index_iterators(expr.expr.expr, idx)
end

Base.length(expr::ExprGenerator) = prod(_size(expr))

struct ParametrizedArray
    constraint::Any
    iterators::Any
end

function JuMP.Containers.container(
    f::Function,
    indices::JuMP.Containers.VectorizedProductIterator,
    ::Type{ParametrizedArray},
)
    its = iterators(indices.prod.iterators)
    return ParametrizedArray(f(its...), its)
end

function JuMP.build_constraint(
    error_fn::Function,
    func::ExprTemplate,
    set::MOI.Utilities.ScalarLinearSet,
)
    new_func = ExprGenerator(func - MOI.constant(set))
    S = MOI.Utilities.vector_set_type(typeof(set))
    vector_set = S(length(new_func))
    return JuMP.build_constraint(error_fn, new_func, vector_set)
end

struct IteratedConstraint{
    E,
    V<:JuMP.GenericVariableRef,
    S<:MOI.AbstractVectorSet,
} <: JuMP.AbstractConstraint
    func::ExprGenerator{E,V}
    set::S
end

JuMP.shape(::IteratedConstraint) = JuMP.VectorShape()

JuMP.reshape_vector(f::ExprGenerator, ::JuMP.VectorShape) = f

function JuMP.check_belongs_to_model(con::IteratedConstraint, model)
    return JuMP.check_belongs_to_model(con.func.expr, model)
end

function JuMP.build_constraint(
    ::Function,
    expr::ExprGenerator,
    set::MOI.AbstractVectorSet,
)
    return IteratedConstraint(expr, set)
end

function JuMP.constraint_object(
    con_ref::JuMP.ConstraintRef{
        <:JuMP.AbstractModel,
        MOI.ConstraintIndex{FuncType,SetType},
    },
) where {FuncType<:FunctionGenerator,SetType<:MOI.AbstractVectorSet}
    model = con_ref.model
    f = MOI.get(model, MOI.ConstraintFunction(), con_ref)::FuncType
    s = MOI.get(model, MOI.ConstraintSet(), con_ref)::SetType
    return IteratedConstraint(JuMP.jump_function(model, f), s)
end

include("print.jl")
