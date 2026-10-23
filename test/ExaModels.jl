# Copyright (c) 2024: Benoît Legat and contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

# Scope: the `GenOptExaModelsExt` extension, end to end with
# `ExaModels.Optimizer`.

module TestExaModels

using Test
using JuMP
import ExaModels
import GenOpt
import MadNLP

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

# Simplified quadrotor of the ExaModels tutorial. If `parameters` is true, MOI
# parameters are created before the variables so the MOI variable indices of
# the JuMP model differ from the ones of `ExaModels.Optimizer`.
function _quadrotor(; parameters::Bool = false)
    N, n, p, dt, g = 3, 9, 4, 0.1, 9.81
    xf = [1.0, 0, 0, 1, 0, 0, 0, 0, 0]
    Q = zeros(n)
    Q[1] = Q[4] = 1.0
    Qf = ones(n)
    R = ones(p)
    itr1 = [(i, j, xf[j]) for i in 1:N for j in 1:n if Q[j] != 0]
    itr2 = [(j, xf[j]) for j in 1:n]
    model = Model()
    if parameters
        @variable(model, par1 in MOI.Parameter(2.0))
        @variable(model, par2 in MOI.Parameter(3.0))
    end
    @variable(model, x[1:(N+1), 1:n])
    @variable(model, u[1:N, 1:p])
    c = GenOpt.ParametrizedArray
    @constraint(model, [i in 1:n], x[1, i] == 0, container = c)
    @constraint(
        model,
        [i in 1:N],
        x[i+1, 1] == x[i, 1] + x[i, 2] * dt,
        container = c
    )
    @constraint(
        model,
        [i in 1:N],
        x[i+1, 2] == x[i, 2] + u[i, 1] * dt,
        container = c
    )
    @constraint(
        model,
        [i in 1:N],
        x[i+1, 3] == x[i, 3] + x[i, 4] * dt,
        container = c
    )
    @constraint(
        model,
        [i in 1:N],
        x[i+1, 4] == x[i, 4] + u[i, 2] * dt,
        container = c
    )
    @constraint(
        model,
        [i in 1:N],
        x[i+1, 5] == x[i, 5] + x[i, 6] * dt,
        container = c
    )
    @constraint(
        model,
        [i in 1:N],
        x[i+1, 6] == x[i, 6] + (u[i, 3] - g) * dt,
        container = c,
    )
    @constraint(
        model,
        [i in 1:N],
        x[i+1, 7] == x[i, 7] + u[i, 1] * dt,
        container = c
    )
    @constraint(
        model,
        [i in 1:N],
        x[i+1, 8] == x[i, 8] + u[i, 2] * dt,
        container = c
    )
    @constraint(
        model,
        [i in 1:N],
        x[i+1, 9] == x[i, 9] + u[i, 4] * dt,
        container = c
    )
    @objective(
        model,
        Min,
        GenOpt.lazy_sum(0.5 * R[j] * (u[i, j]^2) for i in 1:N, j in 1:p) +
        GenOpt.lazy_sum(
            0.5 * Q[it[2]] * (x[it[1], it[2]] - it[3])^2 for it in itr1
        ) +
        GenOpt.lazy_sum(
            0.5 * Qf[it[1]] * (x[N+1, it[1]] - it[2])^2 for it in itr2
        ),
    )
    return model
end

function _optimize!(model)
    set_optimizer(model, () -> ExaModels.Optimizer(MadNLP.madnlp))
    set_attribute(model, "print_level", MadNLP.ERROR)
    optimize!(model)
    return
end

function test_quadrotor()
    model = _quadrotor()
    _optimize!(model)
    @test objective_value(model) ≈ 8.1797 atol = 1e-3
    return
end

function test_quadrotor_structure()
    # The 10 `@constraint` with `container = GenOpt.ParametrizedArray` are 1
    # initial condition over `n = 9` and 9 dynamics over `N = 3`. They should
    # not be scalarized: each one should be a block augmenting the 36 rows.
    m = ExaModels.ExaModel(_quadrotor())
    @test m.meta.ncon == 36
    @test sort([length(c.itr) for c in m.cons]) == [fill(3, 9); 9; 36]
    return
end

function test_quadrotor_parameters()
    model = _quadrotor(; parameters = true)
    _optimize!(model)
    @test objective_value(model) ≈ 8.1797 atol = 1e-3
    return
end

function test_single_sum_objective()
    N = 4
    model = Model()
    @variable(model, x[1:N])
    c = GenOpt.ParametrizedArray
    @constraint(model, [i in 1:N], x[i] >= 2, container = c)
    @constraint(model, [i in 1:N], x[i] <= 3, container = c)
    @objective(model, Min, GenOpt.lazy_sum((x[i] - i)^2 for i in 1:N))
    _optimize!(model)
    @test objective_value(model) ≈ 2 atol = 1e-6
    @test value.(x) ≈ [2, 2, 3, 3] atol = 1e-3
    return
end

function test_typed_sum_structure()
    for mixed in (false, true)
        model = Model()
        @variable(model, x[1:4])
        linear = GenOpt.lazy_sum(i * x[i] for i in 1:4)
        quadratic = GenOpt.lazy_sum(x[i]^2 for i in 1:4)
        expression = mixed ? linear + quadratic : quadratic
        @objective(model, Min, expression)
        func = MOI.get(
            backend(model),
            MOI.ObjectiveFunction{JuMP.moi_function_type(typeof(expression))}(),
        )
        @test func isa GenOpt.PolynomialFunction
        optimizer = ExaModels.Optimizer(nothing)
        set_optimizer(model, () -> optimizer)
        MOI.Utilities.attach_optimizer(backend(model))
        @test length(optimizer.objs) == (mixed ? 2 : 1)
        @test all(bin -> length(bin.data) == 4, optimizer.objs)
    end
    return
end

function test_polynomial_leaf_unwrapping()
    extension = Base.get_extension(GenOpt, :GenOptExaModelsExt)
    F = MOI.ScalarAffineFunction{Float64}
    lookup = MOI.ScalarNonlinearFunction(
        :getindex,
        Any[GenOpt.ContiguousArrayOfVariables(0, (1,)), 1],
    )
    template = MOI.ScalarNonlinearFunction(:+, Any[lookup, 2.0])
    wrapped = GenOpt.PolynomialFunction{F}(template)
    @test typeof(extension.exagen(wrapped, nothing)) ==
          typeof(extension.exagen(template, nothing))
    func = MOI.ScalarNonlinearFunction(:+, Any[MOI.VariableIndex(1), 2.0])
    wrapped = GenOpt.PolynomialFunction{F}(func)
    head, data = extension.ExaModelsMOI._exafy(wrapped, (), nothing)
    expected_head, expected_data =
        extension.ExaModelsMOI._exafy(func, (), nothing)
    @test typeof(head) == typeof(expected_head)
    @test data == expected_data
    @test wrapped.func === func
    return
end

function test_deferred_getindex()
    # `d[i + j]` involves two iterators so it is kept as a `getindex` of the
    # data which is not supported by the extension
    d = [1.0, 2.0, 3.0]
    model = Model()
    @variable(model, x[1:2])
    @objective(
        model,
        Min,
        GenOpt.lazy_sum(d[i+j] * x[i]^2 for i in 1:2, j in 1:1),
    )
    @test_throws ErrorException _optimize!(model)
    return
end

end  # module

TestExaModels.runtests()
