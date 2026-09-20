# Copyright (c) 2024: Benoît Legat and contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

# Scope: `IteratorRef` and `IteratorIndex` at the MOI level, end to end with a solver
# (HiGHS). Add new tests about iterator references or placeholders in a template here
# rather than in a new file.

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

function _getindex(collection, index)
    return MOI.ScalarNonlinearFunction(:getindex, Any[collection, index])
end

function _aff(func)
    f, it = GenOpt.collect_iterator_refs(func)
    return GenOpt.FunctionGenerator{MOI.ScalarAffineFunction{Float64}}(f, it)
end

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
                    Any[
                        2.0,
                        MOI.ScalarNonlinearFunction(
                            :-,
                            Any[GenOpt.IteratorRef(j), 1.0],
                        ),
                    ],
                ),
                GenOpt.IteratorRef(i),
            ],
        ),
    )
    generator = _aff(template)
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
        Any[
            _getindex(x, GenOpt.IteratorRef(i)),
            _getindex(x, GenOpt.IteratorRef(i)),
        ],
    )
    generator = _aff(template)
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
    generator = _aff(template)
    MOI.add_constraint(optimizer, generator, MOI.Nonnegatives(3))
    obj = MOI.ScalarAffineFunction(
        [MOI.ScalarAffineTerm(1.0, xi) for xi in x],
        0.0,
    )
    MOI.set(optimizer, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(optimizer, MOI.ObjectiveFunction{typeof(obj)}(), obj)
    MOI.optimize!(optimizer)
    @test MOI.get(optimizer, MOI.TerminationStatus()) == MOI.OPTIMAL
    @test MOI.get(optimizer, MOI.VariablePrimal(), x) ≈ [1.0, 2.0, 3.0] atol =
        1e-6
end

# `MOI.Utilities.map_indices` is called on every function of a model that is copied from one
# optimizer to another. That happens whenever the model is built before the solver is
# attached -- `Model()`, then `set_optimizer`, then `optimize!` -- which is how the OPF
# example is written.
#
# Why it is useful: `map_indices` walks every argument of a `ScalarNonlinearFunction`, so it
# reaches the `IteratorIndex` placeholders a generator template is made of. They are not MOI
# indices and nothing maps them, so without a method for them the copy fails with
# `MethodError: no method matching map_indices(::Base.Fix1{typeof(getindex), IndexMap},
# ::IteratorIndex)`. They must come back untouched while the variables around them are
# remapped.
function test_map_indices_keeps_iterator_index()
    block = GenOpt.ContiguousArrayOfVariables(0, (3,))
    index = GenOpt.IteratorIndex(1)
    template =
        MOI.ScalarNonlinearFunction(:-, Any[_getindex(block, index), index])
    # The index map a copy to another model would use: shift every variable.
    index_map = MOI.Utilities.IndexMap()
    for k in 1:3
        index_map[MOI.VariableIndex(k)] = MOI.VariableIndex(k + 10)
    end
    mapped = MOI.Utilities.map_indices(index_map, template)
    @test mapped.head == :-
    # The placeholders are left as they are, at both depths of the expression.
    @test mapped.args[2] === index
    @test mapped.args[1].head == :getindex
    @test mapped.args[1].args[2] === index
    # ... while the variables they index into are remapped.
    @test mapped.args[1].args[1] == MOI.VariableIndex.(11:13)
    return
end

end  # module

TestIteratorRef.runtests()
