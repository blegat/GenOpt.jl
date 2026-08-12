using Test
import MathOptInterface as MOI

include(joinpath(@__DIR__, "model.jl"))

# The model is built with `N = 3` and `n = 9` so the 10 `@constraint` calls
# using `container = ParametrizedArray` encode `9 + 9 * 3 == 36` scalar
# equalities. The point of GenOpt is that they stay grouped as 10 generators
# instead of being scalarized, so that is what we check here.
const N, n = 3, 9

# `x[i+1, j] == x[i, j] + x[i, j+1] * dt` for `j in (1, 3, 5)` is affine, as is
# the initial condition `x[1, i] == x0[i]`. The 6 remaining dynamics involve
# `cos`, `sin` or `tan` and are nonlinear.
const AFFINE = ExprGenerator{JuMP.AffExpr,JuMP.VariableRef}
const NONLINEAR = ExprGenerator{JuMP.NonlinearExpr,JuMP.VariableRef}

model = build_model(; N = N, n = n)
@test model isa JuMP.Model

@testset "list_of_constraint_types" begin
    types = JuMP.list_of_constraint_types(model)
    # Both generator types are present and, importantly, the affine block was
    # not widened into `NonlinearExpr` by the nonlinear dynamics.
    @test Set(types) == Set([(AFFINE, MOI.Zeros), (NONLINEAR, MOI.Zeros)])
    # Nothing was scalarized: a scalarized model would report the standard
    # `(AffExpr, MOI.EqualTo{Float64})` pair instead.
    @test !any(S <: MOI.AbstractScalarSet for (_, S) in types)
    @test all(F <: ExprGenerator for (F, _) in types)
end

@testset "generators are not scalarized" begin
    b = JuMP.backend(model)
    moi_types = MOI.get(b, MOI.ListOfConstraintTypesPresent())
    # `list_of_constraint_types` is the JuMP image of the MOI list.
    @test JuMP.list_of_constraint_types(model) ==
          [(JuMP.jump_function_type(model, F), S) for (F, S) in moi_types]

    dims = Dict{Type,Vector{Int}}()
    for (F, S) in moi_types
        cis = MOI.get(b, MOI.ListOfConstraintIndices{F,S}())
        dims[JuMP.jump_function_type(model, F)] = sort!([
            MOI.output_dimension(MOI.get(b, MOI.ConstraintFunction(), ci))
            for ci in cis
        ])
    end
    # 1 initial condition over `n` and 3 affine dynamics over `N`.
    @test dims[AFFINE] == [N, N, N, n]
    # The 6 remaining dynamics, each over `N`.
    @test dims[NONLINEAR] == fill(N, 6)
    # 10 generators encoding 36 scalar equalities.
    @test sum(length, values(dims)) == 10
    @test sum(sum, values(dims)) == n + 9 * N
end

@testset "constraint_object" begin
    b = JuMP.backend(model)
    for (F, S) in MOI.get(b, MOI.ListOfConstraintTypesPresent())
        E = JuMP.jump_function_type(model, F)
        for ci in MOI.get(b, MOI.ListOfConstraintIndices{F,S}())
            ref = JuMP.ConstraintRef(model, ci, JuMP.VectorShape())
            con = JuMP.constraint_object(ref)
            @test con isa IteratedConstraint
            @test con.func isa E
            @test con.set isa MOI.Zeros
            # The generator expands lazily to one scalar expression per index.
            func = MOI.get(b, MOI.ConstraintFunction(), ci)
            @test length(con.func) == MOI.output_dimension(func)
            @test MOI.dimension(con.set) == length(con.func)
            @test all(e -> e isa JuMP.AbstractJuMPScalar, con.func)
        end
    end
end

@testset "objective keeps its generators" begin
    b = JuMP.backend(model)
    F = MOI.get(b, MOI.ObjectiveFunctionType())
    @test F == MOI.ScalarNonlinearFunction
    obj = MOI.get(b, MOI.ObjectiveFunction{F}())
    # The three `lazy_sum` terms are summed without being expanded.
    @test obj.head == :+
    @test all(a -> a isa SumGenerator, obj.args)
    terms = [prod(it -> length(it.values), a.iterators) for a in obj.args]
    # `N * p` control terms, `N * n` stage terms and `n` terminal terms.
    @test terms == [N * 4, N * n, n]
end
