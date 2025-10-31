import MathOptInterface as MOI

"""
    struct Iterator
        length::Int
        values::Vector{Float64}
    end
"""
struct Iterator
    length::Int
    values::Vector{Float64}
    function Iterator(length::Int)
        return new(length, Float64[])
    end
end

function num_values(it::Iterator)
    return div(length(it.values), it.length)
end

function values_at(it::Iterator, i)
    return it.values[(1 + it.length * (i - 1)):it.length * i]
end

struct IteratorIndex
    iterator_index::Int
    value_index::Int
end

struct IteratedFunction <: MOI.AbstractVectorFunction
    func::MOI.ScalarNonlinearFunction
    iterators::Vector{Iterator}
end

Base.copy(f::IteratedFunction) = IteratedFunction(copy(f.func), f.iterators)
MOI.Utilities.is_canonical(f::IteratedFunction) = MOI.Utilities.is_canonical(f.func)
