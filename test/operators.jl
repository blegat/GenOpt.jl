module TestOperators

using Test
using GenOpt

function runtests()
    for name in names(@__MODULE__; all = true)
        if startswith("$(name)", "test_")
            @testset "$(name)" begin
                getfield(@__MODULE__, name)()
            end
        end
    end
    return
end

function _test_iterator(it, values)
    @test it isa IteratorValues
    @test it.iterator.length == length(values)
    @test it.values == values
end

function test_getindex()
    d1 = Dict(:a => -1, :b => 1)
    d2 = Dict(:a => π, :b => 0.0)

    i = GenOpt.iterator([:a, :b])

    _test_iterator(d1[i], [-1, 1])
    _test_iterator(d2[i], Real[π, 0.0])
end

end  # module

TestOperators.runtests()
