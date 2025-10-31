# Copyright (c) 2024: Benoît Legat and contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

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

function test_univariate()
    i = GenOpt.iterator([2, -3])
    _test_iterator(+i, [2, -3])
    _test_iterator(-i, [-2, 3])
end

function test_multivariate()
    i = GenOpt.iterator([2, -3])
    _test_iterator(i + 1, [3, -2])
    _test_iterator(2 - i, [0, 5])
    j = GenOpt.iterator([1, -1])
    err = ErrorException("Operation between two iterators is not implemented yet")
    @test_throws err i + j
end

end  # module

TestOperators.runtests()
