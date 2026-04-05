struct ArrayOfVariables{T,N} <: AbstractArray{JuMP.GenericVariableRef{T},N}
    model::JuMP.GenericModel{T}
    offset::Int64
    size::NTuple{N,Int64}
end

Base.size(array::ArrayOfVariables) = array.size
function Base.getindex(A::ArrayOfVariables{T}, I...) where {T}
    index =
        A.offset + Base._to_linear_index(Base.CartesianIndices(A.size), I...)
    return JuMP.GenericVariableRef{T}(A.model, MOI.VariableIndex(index))
end

function JuMP.Containers.container(
    f::Function,
    indices::JuMP.Containers.VectorizedProductIterator{NTuple{N,Base.OneTo{Int}}},
    ::Type{ArrayOfVariables},
) where {N}
    return to_generator(JuMP.Containers.container(f, indices, Array))
end

JuMP._is_real(::ArrayOfVariables) = true
function JuMP.moi_function(array::ArrayOfVariables)
    return ContiguousArrayOfVariables(array.offset, array.size)
end
function JuMP.jump_function(
    model::JuMP.GenericModel{T},
    array::ContiguousArrayOfVariables{N},
) where {T,N}
    return ArrayOfVariables{T,N}(model, array.offset, array.size)
end

function Base.convert(
    ::Type{ArrayOfVariables{T,N}},
    array::Array{JuMP.GenericVariableRef{T},N},
) where {T,N}
    model = JuMP.owner_model(array[1])
    offset = JuMP.index(array[1]).value - 1
    for i in eachindex(array)
        @assert JuMP.owner_model(array[i]) === model
        @assert JuMP.index(array[i]).value == offset + i
    end
    return ArrayOfVariables{T,N}(model, offset, size(array))
end

function to_generator(array::Array{JuMP.GenericVariableRef{T},N}) where {T,N}
    return convert(ArrayOfVariables{T,N}, array)
end

function LinearAlgebra.mul(A::ArrayOfVariables, B::Array)
    return JuMP.NonlinearExpr(:*, Any[A, B])
end

function Base.show(io::IO, ::MIME"text/plain", v::ArrayOfVariables)
    return println(io, Base.summary(v), " with offset ", v.offset)
end

function Base.show(io::IO, v::ArrayOfVariables)
    return show(io, MIME"text/plain"(), v)
end
