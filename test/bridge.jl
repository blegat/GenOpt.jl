# Copyright (c) 2024: Benoît Legat and contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestBridge

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

function _create_optimizer()
    inner = HiGHS.Optimizer()
    MOI.set(inner, MOI.RawOptimizerAttribute("output_flag"), false)
    optimizer = MOI.Bridges.full_bridge_optimizer(inner, Float64)
    MOI.Bridges.add_bridge(optimizer, GenOpt.FunctionGeneratorBridge{Float64})
    return optimizer
end

function _affine_sum(vars)
    return MOI.ScalarAffineFunction(
        [MOI.ScalarAffineTerm(1.0, xi) for xi in vars],
        0.0,
    )
end

function _minimize!(optimizer, obj)
    MOI.set(optimizer, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    return MOI.set(optimizer, MOI.ObjectiveFunction{typeof(obj)}(), obj)
end

function test_runtests_simple()
    # x[i] - 1 >= 0 for i in 1..3
    # normalize_and_add moves constant to set: 1.0*x[i] + 0.0 >= 1.0
    return MOI.Bridges.runtests(
        GenOpt.FunctionGeneratorBridge,
        model -> begin
            x = MOI.add_variables(model, 3)
            x_block = GenOpt.ContiguousArrayOfVariables(0, (3,))
            template = MOI.ScalarNonlinearFunction(
                :-,
                Any[
                    MOI.ScalarNonlinearFunction(
                        :getindex,
                        Any[x_block, GenOpt.IteratorIndex(1)],
                    ),
                    1.0,
                ],
            )
            iterators = [GenOpt.Iterator([1, 2, 3])]
            func_gen =
                GenOpt.FunctionGenerator{MOI.ScalarNonlinearFunction}(
                    template,
                    iterators,
                )
            MOI.add_constraint(model, func_gen, MOI.Nonnegatives(3))
        end,
        model -> begin
            x = MOI.add_variables(model, 3)
            for xi in x
                MOI.add_constraint(
                    model,
                    MOI.ScalarAffineFunction(
                        [MOI.ScalarAffineTerm(1.0, xi)],
                        0.0,
                    ),
                    MOI.GreaterThan(1.0),
                )
            end
        end,
    )
end

function test_runtests_equality()
    # x[i] - 5 == 0 for i in 1..2
    # normalize_and_add moves constant to set: 1.0*x[i] + 0.0 == 5.0
    return MOI.Bridges.runtests(
        GenOpt.FunctionGeneratorBridge,
        model -> begin
            x = MOI.add_variables(model, 2)
            x_block = GenOpt.ContiguousArrayOfVariables(0, (2,))
            template = MOI.ScalarNonlinearFunction(
                :-,
                Any[
                    MOI.ScalarNonlinearFunction(
                        :getindex,
                        Any[x_block, GenOpt.IteratorIndex(1)],
                    ),
                    5.0,
                ],
            )
            iterators = [GenOpt.Iterator([1, 2])]
            func_gen =
                GenOpt.FunctionGenerator{MOI.ScalarNonlinearFunction}(
                    template,
                    iterators,
                )
            MOI.add_constraint(model, func_gen, MOI.Zeros(2))
        end,
        model -> begin
            x = MOI.add_variables(model, 2)
            for xi in x
                MOI.add_constraint(
                    model,
                    MOI.ScalarAffineFunction(
                        [MOI.ScalarAffineTerm(1.0, xi)],
                        0.0,
                    ),
                    MOI.EqualTo(5.0),
                )
            end
        end,
    )
end

function test_runtests_consecutive()
    # x[i] + x[i+1] - 2 >= 0 for i in 1..2 (3 variables)
    # normalize_and_add: 1.0*x[i] + 1.0*x[i+1] + 0.0 >= 2.0
    return MOI.Bridges.runtests(
        GenOpt.FunctionGeneratorBridge,
        model -> begin
            x = MOI.add_variables(model, 3)
            x_block = GenOpt.ContiguousArrayOfVariables(0, (3,))
            idx = GenOpt.IteratorIndex(1)
            idx_plus_1 = MOI.ScalarNonlinearFunction(:+, Any[idx, 1])
            template = MOI.ScalarNonlinearFunction(
                :-,
                Any[
                    MOI.ScalarNonlinearFunction(
                        :+,
                        Any[
                            MOI.ScalarNonlinearFunction(
                                :getindex,
                                Any[x_block, idx],
                            ),
                            MOI.ScalarNonlinearFunction(
                                :getindex,
                                Any[x_block, idx_plus_1],
                            ),
                        ],
                    ),
                    2.0,
                ],
            )
            iterators = [GenOpt.Iterator([1, 2])]
            func_gen =
                GenOpt.FunctionGenerator{MOI.ScalarNonlinearFunction}(
                    template,
                    iterators,
                )
            MOI.add_constraint(model, func_gen, MOI.Nonnegatives(2))
        end,
        model -> begin
            x = MOI.add_variables(model, 3)
            MOI.add_constraint(
                model,
                MOI.ScalarAffineFunction(
                    [
                        MOI.ScalarAffineTerm(1.0, x[1]),
                        MOI.ScalarAffineTerm(1.0, x[2]),
                    ],
                    0.0,
                ),
                MOI.GreaterThan(2.0),
            )
            MOI.add_constraint(
                model,
                MOI.ScalarAffineFunction(
                    [
                        MOI.ScalarAffineTerm(1.0, x[2]),
                        MOI.ScalarAffineTerm(1.0, x[3]),
                    ],
                    0.0,
                ),
                MOI.GreaterThan(2.0),
            )
        end,
    )
end

function test_expand_constant()
    func = MOI.ScalarNonlinearFunction(:+, Any[GenOpt.IteratorIndex(1), 1.0])
    result = GenOpt._expand(func, [5.0])
    @test result == 6.0
end

function test_expand_variable()
    x = GenOpt.ContiguousArrayOfVariables(0, (3,))
    index_expr =
        MOI.ScalarNonlinearFunction(:+, Any[GenOpt.IteratorIndex(1), 1])
    func = MOI.ScalarNonlinearFunction(:getindex, Any[x, index_expr])
    result = GenOpt._expand(func, [1])
    @test result == MOI.VariableIndex(2)
end

function test_expand_with_variable_in_expr()
    func = MOI.ScalarNonlinearFunction(
        :+,
        Any[MOI.VariableIndex(1), GenOpt.IteratorIndex(1)],
    )
    result = GenOpt._expand(func, [3.0])
    @test result isa MOI.ScalarNonlinearFunction
    @test result.head == :+
    @test result.args[1] == MOI.VariableIndex(1)
    @test result.args[2] == 3.0
end

function test_to_affine()
    x1 = MOI.VariableIndex(1)
    # x1 - 1.0 should simplify to ScalarAffineFunction
    func = MOI.ScalarNonlinearFunction(:-, Any[x1, 1.0])
    result = GenOpt._to_affine(Float64, func)
    @test result isa MOI.ScalarAffineFunction{Float64}
    @test length(result.terms) == 1
    @test result.terms[1].coefficient == 1.0
    @test result.terms[1].variable == x1
    @test result.constant == -1.0
end

function test_simple_constraint_group()
    # min sum(x)  s.t.  x[i] >= 1 for i in 1..3
    optimizer = _create_optimizer()
    x = MOI.add_variables(optimizer, 3)
    for xi in x
        MOI.add_constraint(optimizer, xi, MOI.GreaterThan(0.0))
    end

    x_block = GenOpt.ContiguousArrayOfVariables(0, (3,))
    template = MOI.ScalarNonlinearFunction(
        :-,
        Any[
            MOI.ScalarNonlinearFunction(
                :getindex,
                Any[x_block, GenOpt.IteratorIndex(1)],
            ),
            1.0,
        ],
    )
    iterators = [GenOpt.Iterator([1, 2, 3])]
    func_gen = GenOpt.FunctionGenerator{MOI.ScalarNonlinearFunction}(
        template,
        iterators,
    )
    MOI.add_constraint(optimizer, func_gen, MOI.Nonnegatives(3))

    _minimize!(optimizer, _affine_sum(x))
    MOI.optimize!(optimizer)
    @test MOI.get(optimizer, MOI.TerminationStatus()) == MOI.OPTIMAL

    for xi in x
        @test MOI.get(optimizer, MOI.VariablePrimal(), xi) ≈ 1.0 atol = 1e-6
    end
end

function test_consecutive_constraint_group()
    # min sum(x)  s.t.  x[i] + x[i+1] >= 2 for i in 1..4, x[i] >= 0
    optimizer = _create_optimizer()
    x = MOI.add_variables(optimizer, 5)
    for xi in x
        MOI.add_constraint(optimizer, xi, MOI.GreaterThan(0.0))
    end

    x_block = GenOpt.ContiguousArrayOfVariables(0, (5,))
    idx = GenOpt.IteratorIndex(1)
    idx_plus_1 = MOI.ScalarNonlinearFunction(:+, Any[idx, 1])
    template = MOI.ScalarNonlinearFunction(
        :-,
        Any[
            MOI.ScalarNonlinearFunction(
                :+,
                Any[
                    MOI.ScalarNonlinearFunction(:getindex, Any[x_block, idx]),
                    MOI.ScalarNonlinearFunction(
                        :getindex,
                        Any[x_block, idx_plus_1],
                    ),
                ],
            ),
            2.0,
        ],
    )
    iterators = [GenOpt.Iterator([1, 2, 3, 4])]
    func_gen = GenOpt.FunctionGenerator{MOI.ScalarNonlinearFunction}(
        template,
        iterators,
    )
    MOI.add_constraint(optimizer, func_gen, MOI.Nonnegatives(4))

    _minimize!(optimizer, _affine_sum(x))
    MOI.optimize!(optimizer)
    @test MOI.get(optimizer, MOI.TerminationStatus()) == MOI.OPTIMAL

    for i in 1:4
        xi = MOI.get(optimizer, MOI.VariablePrimal(), x[i])
        xi1 = MOI.get(optimizer, MOI.VariablePrimal(), x[i+1])
        @test xi + xi1 >= 2.0 - 1e-6
    end
end

function test_parameter_data_lookup()
    # min sum(x)  s.t.  x[i] >= demand[i] for i in 1..3
    # demand = [1.0, 2.0, 3.0]
    optimizer = _create_optimizer()
    x = MOI.add_variables(optimizer, 3)
    for xi in x
        MOI.add_constraint(optimizer, xi, MOI.GreaterThan(0.0))
    end

    demand = [1.0, 2.0, 3.0]
    x_block = GenOpt.ContiguousArrayOfVariables(0, (3,))
    idx = GenOpt.IteratorIndex(1)
    template = MOI.ScalarNonlinearFunction(
        :-,
        Any[
            MOI.ScalarNonlinearFunction(:getindex, Any[x_block, idx]),
            MOI.ScalarNonlinearFunction(:getindex, Any[demand, idx]),
        ],
    )
    iterators = [GenOpt.Iterator([1, 2, 3])]
    func_gen = GenOpt.FunctionGenerator{MOI.ScalarNonlinearFunction}(
        template,
        iterators,
    )
    MOI.add_constraint(optimizer, func_gen, MOI.Nonnegatives(3))

    _minimize!(optimizer, _affine_sum(x))
    MOI.optimize!(optimizer)
    @test MOI.get(optimizer, MOI.TerminationStatus()) == MOI.OPTIMAL

    for (i, d) in enumerate(demand)
        @test MOI.get(optimizer, MOI.VariablePrimal(), x[i]) ≈ d atol = 1e-6
    end
end

function test_multidim_constraint_group()
    # x[2*(i-1) + j] >= 1 for i in 1..2, j in 1..2  (4 constraints, 4 variables)
    optimizer = _create_optimizer()
    x = MOI.add_variables(optimizer, 4)
    for xi in x
        MOI.add_constraint(optimizer, xi, MOI.GreaterThan(0.0))
    end

    x_block = GenOpt.ContiguousArrayOfVariables(0, (4,))
    idx_expr = MOI.ScalarNonlinearFunction(
        :+,
        Any[
            MOI.ScalarNonlinearFunction(
                :*,
                Any[
                    2,
                    MOI.ScalarNonlinearFunction(
                        :-,
                        Any[GenOpt.IteratorIndex(1), 1],
                    ),
                ],
            ),
            GenOpt.IteratorIndex(2),
        ],
    )
    template = MOI.ScalarNonlinearFunction(
        :-,
        Any[
            MOI.ScalarNonlinearFunction(:getindex, Any[x_block, idx_expr]),
            1.0,
        ],
    )
    iterators = [GenOpt.Iterator([1, 2]), GenOpt.Iterator([1, 2])]
    func_gen = GenOpt.FunctionGenerator{MOI.ScalarNonlinearFunction}(
        template,
        iterators,
    )
    MOI.add_constraint(optimizer, func_gen, MOI.Nonnegatives(4))

    _minimize!(optimizer, _affine_sum(x))
    MOI.optimize!(optimizer)
    @test MOI.get(optimizer, MOI.TerminationStatus()) == MOI.OPTIMAL

    for xi in x
        @test MOI.get(optimizer, MOI.VariablePrimal(), xi) ≈ 1.0 atol = 1e-6
    end
end

function test_equality_constraint_group()
    # x[i] == 5 for i in 1..3
    optimizer = _create_optimizer()
    x = MOI.add_variables(optimizer, 3)

    x_block = GenOpt.ContiguousArrayOfVariables(0, (3,))
    template = MOI.ScalarNonlinearFunction(
        :-,
        Any[
            MOI.ScalarNonlinearFunction(
                :getindex,
                Any[x_block, GenOpt.IteratorIndex(1)],
            ),
            5.0,
        ],
    )
    iterators = [GenOpt.Iterator([1, 2, 3])]
    func_gen = GenOpt.FunctionGenerator{MOI.ScalarNonlinearFunction}(
        template,
        iterators,
    )
    MOI.add_constraint(optimizer, func_gen, MOI.Zeros(3))

    _minimize!(optimizer, _affine_sum(x))
    MOI.optimize!(optimizer)
    @test MOI.get(optimizer, MOI.TerminationStatus()) == MOI.OPTIMAL

    for xi in x
        @test MOI.get(optimizer, MOI.VariablePrimal(), xi) ≈ 5.0 atol = 1e-6
    end
end

end  # module

TestBridge.runtests()
