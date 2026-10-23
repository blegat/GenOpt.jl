# Copyright (c) 2024: Benoît Legat and contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestPolynomialFunctionBridges

using Test
import GenOpt
import HiGHS
import JuMP
import MathOptInterface as MOI

function runtests()
    for name in names(@__MODULE__; all = true)
        if startswith(string(name), "test_")
            @testset "$(name)" begin
                getfield(@__MODULE__, name)()
            end
        end
    end
    return
end

function _polynomial(::Type{F}, head, args...) where {F}
    return GenOpt.PolynomialFunction{F}(
        MOI.ScalarNonlinearFunction(head, Any[args...]),
    )
end

function test_polynomial_conversions()
    x = MOI.VariableIndex(1)
    for T in (Float32, Float64),
        F in (MOI.ScalarAffineFunction{T}, MOI.ScalarQuadraticFunction{T})

        raw = MOI.ScalarNonlinearFunction(:+, Any[x, T(2)])
        func = @inferred convert(GenOpt.PolynomialFunction{F}, raw)
        @test func.func === raw
        native = @inferred convert(F, func)
        @test native isa F
        @test native ≈ convert(
            F,
            MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(one(T), x)], T(2)),
        )
        reconstructed = @inferred convert(GenOpt.PolynomialFunction{F}, native)
        @test convert(F, reconstructed) ≈ native
        @test !MOI.Utilities.is_coefficient_type(typeof(func), T)
        @test MOI.Bridges.Constraint.conversion_cost(F, typeof(func)) == 1.0
    end
    @test isinf(
        MOI.Bridges.Constraint.conversion_cost(
            MOI.ScalarAffineFunction{Float64},
            MOI.ScalarNonlinearFunction,
        ),
    )
    invalid = _polynomial(MOI.ScalarAffineFunction{Float64}, :*, x, x)
    @test_throws InexactError convert(
        MOI.ScalarAffineFunction{Float64},
        invalid,
    )
    return
end

function test_polynomial_constraint_attributes()
    for T in (Float32, Float64),
        F in (MOI.ScalarAffineFunction{T}, MOI.ScalarQuadraticFunction{T})

        inner = MOI.Utilities.MockOptimizer(
            MOI.Utilities.UniversalFallback(MOI.Utilities.Model{T}()),
            T,
        )
        model = GenOpt.PolynomialFunctions{T}(inner)
        x = MOI.add_variables(model, 2)
        func = _polynomial(
            F,
            :+,
            MOI.ScalarNonlinearFunction(:*, Any[T(2), x[1]]),
            x[2],
            T(3),
        )
        set = MOI.LessThan(T(10))
        @test MOI.supports_constraint(model, typeof(func), typeof(set))
        ci = MOI.add_constraint(model, func, set)
        bridge = MOI.Bridges.bridge(model, ci)
        @test bridge isa GenOpt.PolynomialFunctionBridge{T,F,typeof(set)}
        @test MOI.get(bridge, MOI.NumberOfConstraints{F,typeof(set)}()) == 1
        @test MOI.get(bridge, MOI.ListOfConstraintIndices{F,typeof(set)}()) ==
              [bridge.constraint]
        @test MOI.get(inner, MOI.ConstraintFunction(), bridge.constraint).constant ===
              zero(T)
        @test MOI.get(inner, MOI.ConstraintSet(), bridge.constraint) ==
              MOI.LessThan(T(7))
        @test MOI.get(model, MOI.ConstraintSet(), ci) == set
        @test convert(F, MOI.get(model, MOI.ConstraintFunction(), ci)) ≈
              convert(F, func)
        MOI.set(model, MOI.ConstraintPrimalStart(), ci, nothing)
        @test MOI.get(model, MOI.ConstraintPrimalStart(), ci) === nothing
        MOI.set(model, MOI.ConstraintPrimalStart(), ci, T(9))
        @test MOI.get(inner, MOI.ConstraintPrimalStart(), bridge.constraint) ===
              T(6)
        @test MOI.get(model, MOI.ConstraintPrimalStart(), ci) === T(9)
        MOI.set(model, MOI.ConstraintDualStart(), ci, T(2))
        @test MOI.get(model, MOI.ConstraintDualStart(), ci) === T(2)
        MOI.Utilities.mock_optimize!(inner, T[1, 2], (F, typeof(set)) => T[5])
        @test MOI.get(model, MOI.ConstraintPrimal(), ci) === T(7)
        @test MOI.get(model, MOI.ConstraintDual(), ci) === T(5)
        replacement = _polynomial(
            F,
            :+,
            MOI.ScalarNonlinearFunction(:*, Any[T(3), x[1]]),
            x[2],
            T(-2),
        )
        MOI.set(model, MOI.ConstraintFunction(), ci, replacement)
        @test MOI.get(model, MOI.ConstraintSet(), ci) == set
        @test MOI.get(inner, MOI.ConstraintSet(), bridge.constraint) ==
              MOI.LessThan(T(12))
        @test convert(F, MOI.get(model, MOI.ConstraintFunction(), ci)) ≈
              convert(F, replacement)
        MOI.set(model, MOI.ConstraintSet(), ci, MOI.LessThan(T(4)))
        @test MOI.get(inner, MOI.ConstraintSet(), bridge.constraint) ==
              MOI.LessThan(T(6))
        @test MOI.get(model, MOI.ConstraintSet(), ci) == MOI.LessThan(T(4))
        MOI.delete(model, x[2])
        expected = convert(
            F,
            MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(T(3), x[1])], T(-2)),
        )
        @test convert(F, MOI.get(model, MOI.ConstraintFunction(), ci)) ≈
              expected
        MOI.delete(model, ci)
        @test !MOI.is_valid(model, ci)
        @test !MOI.is_valid(inner, bridge.constraint)
    end
    return
end

function test_polynomial_unshiftable_set()
    F = MOI.ScalarAffineFunction{Float64}
    inner = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    model = GenOpt.PolynomialFunctions{Float64}(inner)
    x = MOI.add_variable(model)
    func = _polynomial(F, :+, x, 2.0)
    ci = MOI.add_constraint(model, func, MOI.Integer())
    bridge = MOI.Bridges.bridge(model, ci)
    @test bridge.constant == 0.0
    @test MOI.get(model, MOI.ConstraintSet(), ci) == MOI.Integer()
    @test convert(F, MOI.get(model, MOI.ConstraintFunction(), ci)) ≈
          convert(F, func)
    replacement = _polynomial(F, :-, x, 1.0)
    MOI.set(model, MOI.ConstraintFunction(), ci, replacement)
    MOI.set(model, MOI.ConstraintSet(), ci, MOI.Integer())
    @test MOI.get(model, MOI.ConstraintSet(), ci) == MOI.Integer()
    @test convert(F, MOI.get(model, MOI.ConstraintFunction(), ci)) ≈
          convert(F, replacement)
    return
end

function test_polynomial_objective_bridge()
    for T in (Float32, Float64),
        F in (MOI.ScalarAffineFunction{T}, MOI.ScalarQuadraticFunction{T})

        inner = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{T}(), T)
        model = MOI.Bridges.full_bridge_optimizer(inner, T)
        x = MOI.add_variables(model, 2)
        func = _polynomial(F, :+, x[1], x[2], T(3))
        attr = MOI.ObjectiveFunction{typeof(func)}()
        @test MOI.supports(model, attr)
        MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
        MOI.set(model, attr, func)
        @test MOI.get(inner, MOI.ObjectiveFunctionType()) == F
        @test MOI.get(model, MOI.ObjectiveFunctionType()) == typeof(func)
        @test convert(F, MOI.get(model, attr)) ≈ convert(F, func)
        MOI.Utilities.mock_optimize!(inner, T[1, 2])
        @test MOI.get(model, MOI.ObjectiveValue()) === T(6)
        MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
        @test MOI.get(inner, MOI.ObjectiveSense()) == MOI.MAX_SENSE
        MOI.delete(model, x[2])
        expected = convert(
            F,
            MOI.ScalarAffineFunction(
                [MOI.ScalarAffineTerm(one(T), x[1])],
                T(3),
            ),
        )
        @test convert(F, MOI.get(model, attr)) ≈ expected
        MOI.set(model, MOI.ObjectiveSense(), MOI.FEASIBILITY_SENSE)
        @test isempty(MOI.Bridges.Objective.bridges(model))
    end
    return
end

function test_highs_polynomial_support()
    inner = HiGHS.Optimizer()
    model = MOI.Bridges.full_bridge_optimizer(inner, Float64)
    MOI.Bridges.add_bridge(model, GenOpt.PolynomialFunctionBridge{Float64})
    A = GenOpt.PolynomialFunction{MOI.ScalarAffineFunction{Float64}}
    Q = GenOpt.PolynomialFunction{MOI.ScalarQuadraticFunction{Float64}}
    S = MOI.LessThan{Float64}
    @test MOI.supports_constraint(model, A, S)
    @test !MOI.supports_constraint(model, Q, S)
    @test MOI.supports(model, MOI.ObjectiveFunction{A}())
    @test MOI.supports(model, MOI.ObjectiveFunction{Q}())
    @test !MOI.supports_constraint(model, MOI.ScalarNonlinearFunction, S)
    @test !MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarNonlinearFunction}(),
    )
    return
end

function test_highs_lazy_sum_objectives_and_constraints()
    model = JuMP.Model(HiGHS.Optimizer)
    JuMP.set_silent(model)
    JuMP.add_bridge(model, GenOpt.PolynomialFunctionBridge)
    JuMP.@variable(model, x[1:3] >= 0)
    affine = GenOpt.lazy_sum(x[i] + 1 for i in 1:3)
    JuMP.@constraint(model, affine <= 9)
    JuMP.@constraint(model, -GenOpt.lazy_sum(x[i] for i in 1:3) <= -6)
    quadratic = GenOpt.lazy_sum((x[i] - i)^2 for i in 1:3)
    JuMP.@objective(model, Min, quadratic)
    JuMP.optimize!(model)
    @test JuMP.termination_status(model) == MOI.OPTIMAL
    @test JuMP.value.(x) ≈ [1.0, 2.0, 3.0] atol = 1e-6
    @test JuMP.objective_value(model) ≈ 0.0 atol = 1e-6
    @test MOI.get(JuMP.unsafe_backend(model), MOI.ObjectiveFunctionType()) ==
          MOI.ScalarQuadraticFunction{Float64}
    JuMP.@objective(model, Max, GenOpt.lazy_sum(x[i] for i in 1:3))
    JuMP.optimize!(model)
    @test JuMP.termination_status(model) == MOI.OPTIMAL
    @test JuMP.objective_value(model) ≈ 6.0
    @test MOI.get(JuMP.unsafe_backend(model), MOI.ObjectiveFunctionType()) ==
          MOI.ScalarAffineFunction{Float64}
    return
end

end

TestPolynomialFunctionBridges.runtests()
