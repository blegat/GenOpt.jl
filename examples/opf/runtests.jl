module TestOPF

using Test
import MathOptInterface as MOI
import PGLib

include(joinpath(@__DIR__, "model.jl"))

# `case3_lmbd` is the smallest pglib case: 3 buses, 3 branches (hence 6 arcs) and 3
# generators. The variables are `va`, `vm` (per bus), `pg`, `qg` (per generator) and `p`,
# `q` (per arc), so `3 * 4 + 6 * 2 == 24`.
model = build_model(PGLib.pglib("case3_lmbd"))
@test model isa JuMP.Model
@test JuMP.num_variables(model) == 24

const AFFINE = FunctionGenerator{MOI.ScalarAffineFunction{Float64}}
const QUADRATIC = FunctionGenerator{MOI.ScalarQuadraticFunction{Float64}}
const NONLINEAR = FunctionGenerator{MOI.ScalarNonlinearFunction}

# The point of GenOpt is that each `@constraint` of `model.jl` stays a single generator
# instead of being scalarized into one constraint per component, so that is what we check.
@testset "constraints are not scalarized" begin
    b = JuMP.backend(model)
    dims = Dict{Tuple{Type,Type},Vector{Int}}()
    for (F, S) in MOI.get(b, MOI.ListOfConstraintTypesPresent())
        if F == MOI.VariableIndex
            # The bounds written in the `@variable` declarations.
            @test S <: Union{MOI.GreaterThan,MOI.LessThan}
            continue
        end
        @test F <: FunctionGenerator
        cis = MOI.get(b, MOI.ListOfConstraintIndices{F,S}())
        dims[(F, S)] = sort!([
            MOI.output_dimension(MOI.get(b, MOI.ConstraintFunction(), ci))
            for ci in cis
        ])
    end
    # The reference bus angle, over the single reference bus.
    @test dims[(AFFINE, MOI.Zeros)] == [1]
    # The 4 branch flow definitions and the 2 bus power balances, over 3 components each.
    @test dims[(NONLINEAR, MOI.Zeros)] == fill(3, 6)
    # `angmin[i] <= va[..] - va[..] <= angmax[i]` stays one interval constraint carrying
    # the per-branch bounds, rather than two inequalities per branch.
    @test dims[(AFFINE, MOI.HyperRectangle{Float64})] == [3]
    # The two thermal limits `|S|^2 <= rate_a^2`, over the 3 branches.
    @test dims[(QUADRATIC, MOI.Nonpositives)] == [3, 3]
    # 10 generators encoding 28 scalar constraints.
    @test sum(length, values(dims)) == 10
    @test sum(sum, values(dims)) == 28
end

@testset "objective keeps its generator" begin
    b = JuMP.backend(model)
    F = MOI.get(b, MOI.ObjectiveFunctionType())
    # The `lazy_sum` over the generators is not expanded into 3 cost terms.
    @test F == SumGenerator{MOI.ScalarQuadraticFunction{Float64}}
    obj = MOI.get(b, MOI.ObjectiveFunction{F}())
    @test prod(it -> length(it.values), obj.iterators) == 3
end

end  # module TestOPF
