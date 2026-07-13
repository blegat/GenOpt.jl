# Copyright (c) 2024: Benoît Legat and contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestIteratorRef

using Test
import MathOptInterface as MOI
import GenOpt
import HiGHS

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

_getindex(collection, index) =
    MOI.ScalarNonlinearFunction(:getindex, Any[collection, index])

function test_discovers_iterators_in_first_encounter_order()
    i = GenOpt.Iterator(1:3)
    j = GenOpt.Iterator(1:2)
    x = GenOpt.ContiguousArrayOfVariables(0, (6,))
    # x[2(j - 1) + i]
    template = _getindex(
        x,
        MOI.ScalarNonlinearFunction(
            :+,
            Any[
                MOI.ScalarNonlinearFunction(
                    :*,
                    Any[2.0, MOI.ScalarNonlinearFunction(:-, Any[GenOpt.IteratorRef(j), 1.0])],
                ),
                GenOpt.IteratorRef(i),
            ],
        ),
    )
    generator = GenOpt.FunctionGenerator{MOI.ScalarAffineFunction{Float64}}(template)
    @test generator.iterators == [j, i]
    @test MOI.output_dimension(generator) == 6
    # j was encountered first, i second
    plus = generator.func.args[2]
    @test plus.args[2] == GenOpt.IteratorIndex(2)
    @test plus.args[1].args[2].args[1] == GenOpt.IteratorIndex(1)
end

function test_same_iterator_twice_is_diagonal()
    i = GenOpt.Iterator(1:3)
    x = GenOpt.ContiguousArrayOfVariables(0, (3,))
    # x[i] + x[i]: the same iterator must map to the same index
    template = MOI.ScalarNonlinearFunction(
        :+,
        Any[_getindex(x, GenOpt.IteratorRef(i)), _getindex(x, GenOpt.IteratorRef(i))],
    )
    generator = GenOpt.FunctionGenerator{MOI.ScalarAffineFunction{Float64}}(template)
    @test generator.iterators == [i]
    @test MOI.output_dimension(generator) == 3
end

function test_solve_through_bridge()
    # min sum(x)  s.t.  x[i] >= i for i in 1..3; solution x = (1, 2, 3)
    inner = HiGHS.Optimizer()
    MOI.set(inner, MOI.Silent(), true)
    optimizer = MOI.Bridges.full_bridge_optimizer(inner, Float64)
    MOI.Bridges.add_bridge(optimizer, GenOpt.FunctionGeneratorBridge{Float64})
    x = MOI.add_variables(optimizer, 3)
    i = GenOpt.Iterator(1:3)
    block = GenOpt.ContiguousArrayOfVariables(0, (3,))
    template = MOI.ScalarNonlinearFunction(
        :-,
        Any[_getindex(block, GenOpt.IteratorRef(i)), GenOpt.IteratorRef(i)],
    )
    generator =
        GenOpt.FunctionGenerator{MOI.ScalarAffineFunction{Float64}}(template)
    MOI.add_constraint(optimizer, generator, MOI.Nonnegatives(3))
    obj = MOI.ScalarAffineFunction(
        [MOI.ScalarAffineTerm(1.0, xi) for xi in x],
        0.0,
    )
    MOI.set(optimizer, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(optimizer, MOI.ObjectiveFunction{typeof(obj)}(), obj)
    MOI.optimize!(optimizer)
    @test MOI.get(optimizer, MOI.TerminationStatus()) == MOI.OPTIMAL
    @test MOI.get(optimizer, MOI.VariablePrimal(), x) ≈ [1.0, 2.0, 3.0] atol = 1e-6
end

end  # module

TestIteratorRef.runtests()
