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
                GenOpt.FunctionGenerator{MOI.ScalarAffineFunction{Float64}}(
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
        cannot_unbridge = true,
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
                GenOpt.FunctionGenerator{MOI.ScalarAffineFunction{Float64}}(
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
        cannot_unbridge = true,
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
                GenOpt.FunctionGenerator{MOI.ScalarAffineFunction{Float64}}(
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
        cannot_unbridge = true,
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

function test_affine_jump_wrapped_iterator_index()
    # The JuMP wrapper's `prepare(it::IteratorValues)` produces SNFs of the form
    # `SNF(:getindex, [IteratorIndex(k), value_index])` because iterator values
    # are stored as tuples (so the value is `values[k][value_index]`). A
    # constraint like `x[k+1] + x[k] == c` produces the index expression
    # `SNF(:+, [SNF(:getindex, [IteratorIndex(1), 1]), 1])` inside a `:getindex`
    # on `x`. Exercise that path so `_eval_index` resolves it correctly.
    x_block = GenOpt.ContiguousArrayOfVariables(0, (5,))
    k_idx =
        MOI.ScalarNonlinearFunction(:getindex, Any[GenOpt.IteratorIndex(1), 1])
    k_plus_1 = MOI.ScalarNonlinearFunction(:+, Any[k_idx, 1])
    template = MOI.ScalarNonlinearFunction(
        :+,
        Any[
            MOI.ScalarNonlinearFunction(:getindex, Any[x_block, k_idx]),
            MOI.ScalarNonlinearFunction(:getindex, Any[x_block, k_plus_1]),
        ],
    )
    out = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm{Float64}[], 0.0)
    values = Any[(2,)]
    GenOpt._expand_affine!(out, template, values, 1.0)
    @test out.constant == 0.0
    @test length(out.terms) == 2
    @test out.terms[1].coefficient == 1.0
    @test out.terms[1].variable == MOI.VariableIndex(2)
    @test out.terms[2].coefficient == 1.0
    @test out.terms[2].variable == MOI.VariableIndex(3)
end

function test_eval_index_getindex_iterator_index_alloc()
    # JuMP-wrapped iterator-reference pattern (`SNF(:getindex, [IteratorIndex(k), j])`).
    # `_eval_index` is intentionally allowed small allocations — see the comment
    # at its definition: it is called only off the `:getindex` resolution path,
    # not the affine hot path, so the recursive `Float64` return type isn't
    # forced to be allocation-free. The bound below is the headroom around the
    # one box per `Vector{Any}` read (tuple value + arg slot). Tighten if the
    # implementation later avoids those.
    expr =
        MOI.ScalarNonlinearFunction(:getindex, Any[GenOpt.IteratorIndex(1), 1])
    values = Any[(7,)]
    @test GenOpt._eval_index(expr, values) === 7.0
    @test GenOpt._to_int(expr, values) === 7
    # Warm up before measuring so first-call compilation isn't counted.
    GenOpt._eval_index(expr, values)
    GenOpt._to_int(expr, values)
    @test @allocated(GenOpt._eval_index(expr, values)) <= 64
    @test @allocated(GenOpt._to_int(expr, values)) <= 64
end

function test_eval_index_getindex_data_collection()
    # Covers the `coll isa IteratorIndex ? ... : coll` else branch — used when
    # an inner index expression itself dereferences a constant data array,
    # e.g. `vpll_d[some_data[k], s]`.
    data = [10, 20, 30, 40]
    expr = MOI.ScalarNonlinearFunction(
        :getindex,
        Any[data, MOI.ScalarNonlinearFunction(:+, Any[GenOpt.IteratorIndex(1), 1])],
    )
    @test GenOpt._eval_index(expr, [1]) == 20.0
    @test GenOpt._to_int(expr, [2]) == 30
end

function test_eval_index_getindex_n3()
    # Covers the n == 3 (2-D `getindex`) branch of `_eval_index`.
    mat = [10 20; 30 40]
    expr = MOI.ScalarNonlinearFunction(
        :getindex,
        Any[mat, GenOpt.IteratorIndex(1), GenOpt.IteratorIndex(2)],
    )
    @test GenOpt._eval_index(expr, Any[2, 1]) == 30.0
end

function test_eval_index_getindex_nlarge()
    # Covers the ntuple fallback branch (n > 3). 3-rd order tensor — fallback
    # path that pays one box per call, so we don't check allocations here.
    arr = reshape(collect(1:8), (2, 2, 2))
    expr = MOI.ScalarNonlinearFunction(
        :getindex,
        Any[
            arr,
            GenOpt.IteratorIndex(1),
            GenOpt.IteratorIndex(2),
            GenOpt.IteratorIndex(3),
        ],
    )
    @test GenOpt._eval_index(expr, Any[2, 2, 2]) == 8.0
end

function test_eval_index_unsupported_head_error()
    # Covers the unsupported-SNF-head error branch. The error message must
    # NOT call `string(::SNF)` on an SNF containing `IteratorIndex` — that
    # would trigger an unrelated MOI `_to_string` MethodError and mask the
    # real "Cannot resolve" message.
    expr = MOI.ScalarNonlinearFunction(:sin, Any[GenOpt.IteratorIndex(1)])
    err = try
        GenOpt._eval_index(expr, Any[(2,)])
    catch e
        e
    end
    @test err isa ErrorException
    msg = sprint(showerror, err)
    @test occursin("Cannot resolve `getindex` index", msg)
    @test occursin(":sin", msg)
end

function test_eval_index_unsupported_type_error()
    # Covers the top-level unsupported-type fallback (anything that is not
    # Integer / AbstractFloat / IteratorIndex / SNF).
    err = try
        GenOpt._eval_index(:not_a_number, Any[])
    catch e
        e
    end
    @test err isa ErrorException
    @test occursin("Symbol", sprint(showerror, err))
end

function test_to_int_unsupported_type_error()
    # Same as above for `_to_int`'s top-level fallback.
    err = try
        GenOpt._to_int(:not_a_number, Any[])
    catch e
        e
    end
    @test err isa ErrorException
    @test occursin("Symbol", sprint(showerror, err))
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
    func_gen = GenOpt.FunctionGenerator{MOI.ScalarAffineFunction{Float64}}(
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
    func_gen = GenOpt.FunctionGenerator{MOI.ScalarAffineFunction{Float64}}(
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
    func_gen = GenOpt.FunctionGenerator{MOI.ScalarAffineFunction{Float64}}(
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
    func_gen = GenOpt.FunctionGenerator{MOI.ScalarAffineFunction{Float64}}(
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
    func_gen = GenOpt.FunctionGenerator{MOI.ScalarAffineFunction{Float64}}(
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

function test_expand_affine_allocation_free()
    # Template: 2 * x[1] + price[i] * x[i] - 1 for i in 1..3
    # Exercises all the relevant branches: `+`, `-`, `*` (literal-on-left,
    # data-lookup-on-left), `:getindex` on both `ContiguousArrayOfVariables`
    # and `Vector{Float64}`.
    x_block = GenOpt.ContiguousArrayOfVariables(0, (3,))
    price = [2.0, 3.0, 5.0]
    idx = GenOpt.IteratorIndex(1)
    template = MOI.ScalarNonlinearFunction(
        :-,
        Any[
            MOI.ScalarNonlinearFunction(
                :+,
                Any[
                    MOI.ScalarNonlinearFunction(
                        :*,
                        Any[
                            2.0,
                            MOI.ScalarNonlinearFunction(
                                :getindex,
                                Any[x_block, 1],
                            ),
                        ],
                    ),
                    MOI.ScalarNonlinearFunction(
                        :*,
                        Any[
                            MOI.ScalarNonlinearFunction(
                                :getindex,
                                Any[price, idx],
                            ),
                            MOI.ScalarNonlinearFunction(
                                :getindex,
                                Any[x_block, idx],
                            ),
                        ],
                    ),
                ],
            ),
            1.0,
        ],
    )
    values = Vector{Any}(undef, 1)
    values[1] = 2  # iterator value
    out = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm{Float64}[], 0.0)
    sizehint!(out.terms, 2) # final term count for this template
    # Warm up to force compilation.
    GenOpt._expand_affine!(out, template, values, 1.0)
    @test out.constant == -1.0
    @test length(out.terms) == 2
    @test out.terms[1].coefficient == 2.0
    @test out.terms[1].variable.value == 1
    @test out.terms[2].coefficient == 3.0
    @test out.terms[2].variable.value == 2
    # With the buffer pre-sized via `sizehint!` and `N ∈ {0, 1, 2}` enumerated
    # in `_expand_getindex_affine!`, the inner expansion is fully allocation-
    # free. Amortize over a loop to filter out one-shot @allocated overhead.
    function _loop!(out, expr, values, n)
        for _ in 1:n
            empty!(out.terms)
            out.constant = 0.0
            GenOpt._expand_affine!(out, expr, values, 1.0)
        end
    end
    _loop!(out, template, values, 1) # warm
    @test (@allocated _loop!(out, template, values, 1000)) == 0
end

end  # module

TestBridge.runtests()
