# Copyright (c) 2024: Benoît Legat and contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestRegroup

using Test
import MathOptInterface as MOI
import GenOpt
import JuMP
import PowerModels
import Random

const FORMULATIONS = [
    PowerModels.ACPPowerModel,
    PowerModels.ACRPowerModel,
    PowerModels.ACTPowerModel,
    PowerModels.DCPPowerModel,
    PowerModels.SOCWRPowerModel,
    PowerModels.QCRMPowerModel,
    PowerModels.LPACCPowerModel,
]

function _powermodels_moi(case::String, F)
    dir = joinpath(dirname(dirname(pathof(PowerModels))), "test", "data", "matpower")
    data = PowerModels.parse_file(joinpath(dir, case))
    PowerModels.standardize_cost_terms!(data, order = 2)
    PowerModels.calc_thermal_limits!(data)
    pm = PowerModels.instantiate_model(data, F, PowerModels.build_opf)
    return JuMP.backend(pm.model)
end

const _SCALAR_SETS = Union{
    MOI.EqualTo{Float64},
    MOI.LessThan{Float64},
    MOI.GreaterThan{Float64},
    MOI.Interval{Float64},
}

# The multiset of residuals of the scalar function constraints of `model` at
# the point `x` (a value per variable, in `ListOfVariableIndices` order),
# normalized the way `GenOpt.regroup` does: the right-hand side is subtracted
# for `EqualTo`/`LessThan`/`GreaterThan`, and `Interval` rows are `(g, l, u)`
# triples.
function _scalar_residuals(model, x::Vector{Float64})
    val = Dict(vi => x[i] for (i, vi) in enumerate(MOI.get(model, MOI.ListOfVariableIndices())))
    residuals = Float64[]
    intervals = Tuple{Float64,Float64,Float64}[]
    n = 0
    for (F, S) in MOI.get(model, MOI.ListOfConstraintTypesPresent())
        if F == MOI.VariableIndex || !(S <: _SCALAR_SETS)
            continue
        end
        for ci in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
            func = MOI.get(model, MOI.ConstraintFunction(), ci)
            set = MOI.get(model, MOI.ConstraintSet(), ci)
            g = MOI.Utilities.eval_variables(Base.Fix1(getindex, val), model, func)
            n += 1
            if set isa MOI.Interval{Float64}
                push!(intervals, (g, set.lower, set.upper))
            else
                push!(residuals, g - MOI.constant(set))
            end
        end
    end
    return residuals, intervals, n
end

# Same, for the `FunctionGenerator` groups of a regrouped model: each row of
# each group is expanded back to a scalar function with `GenOpt._expand` and
# evaluated.
function _group_residuals(model, x::Vector{Float64})
    val = Dict(vi => x[i] for (i, vi) in enumerate(MOI.get(model, MOI.ListOfVariableIndices())))
    residuals = Float64[]
    intervals = Tuple{Float64,Float64,Float64}[]
    ngroups = 0
    nrows = 0
    for (F, S) in MOI.get(model, MOI.ListOfConstraintTypesPresent())
        if !(F <: GenOpt.FunctionGenerator)
            continue
        end
        for ci in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
            func = MOI.get(model, MOI.ConstraintFunction(), ci)
            set = MOI.get(model, MOI.ConstraintSet(), ci)
            ngroups += 1
            for (r, row) in enumerate(only(func.iterators).values)
                scalar = GenOpt._expand(func.func, [row])
                g = MOI.Utilities.eval_variables(Base.Fix1(getindex, val), model, scalar)
                nrows += 1
                if set isa GenOpt.VectorInterval{Float64}
                    push!(intervals, (g, set.lower[r], set.upper[r]))
                else
                    @assert set isa Union{MOI.Zeros,MOI.Nonpositives,MOI.Nonnegatives}
                    push!(residuals, g)
                end
            end
        end
    end
    return residuals, intervals, ngroups, nrows
end

# Number of `FunctionGenerator` groups expected for each formulation and case.
# The count is independent of the instance size except for the constraints
# with a per-row number of terms (the power balances): those group by term
# count, so larger cases with a more diverse bus degree distribution have a
# few more groups — but the count stays bounded by the degree diversity, it
# does not scale with the number of constraints (case30 has 4x the
# constraints of case5).
const EXPECTED_GROUPS = Dict(
    #                               case5.m  case30.m
    PowerModels.ACPPowerModel => (13, 21),
    PowerModels.ACRPowerModel => (16, 27),
    PowerModels.ACTPowerModel => (13, 20),
    PowerModels.DCPPowerModel => (9, 17),
    PowerModels.SOCWRPowerModel => (11, 19),
    PowerModels.QCRMPowerModel => (23, 29),
    PowerModels.LPACCPowerModel => (12, 20),
)

function test_powermodels_regroup()
    PowerModels.silence()
    for F in FORMULATIONS
        for (k, case) in enumerate(["case5.m", "case30.m"])
            src = _powermodels_moi(case, F)
            dest = GenOpt.regroup(src)
            Random.seed!(k)
            x = randn(MOI.get(src, MOI.NumberOfVariables()))
            res_src, int_src, n_src = _scalar_residuals(src, x)
            res_dest, int_dest, ngroups, nrows = _group_residuals(dest, x)
            # every scalar constraint ends up as exactly one row of a group
            @test nrows == n_src
            @test ngroups == EXPECTED_GROUPS[F][k]
            # and the rows evaluate to the same residuals
            @test sort(res_dest) ≈ sort(res_src)
            @test length(int_dest) == length(int_src)
            for (a, b) in zip(sort(int_dest), sort(int_src))
                @test collect(a) ≈ collect(b)
            end
        end
    end
end

function test_variable_data_passthrough()
    PowerModels.silence()
    src = _powermodels_moi("case5.m", PowerModels.ACPPowerModel)
    dest = GenOpt.regroup(src)
    @test MOI.get(dest, MOI.NumberOfVariables()) ==
          MOI.get(src, MOI.NumberOfVariables())
    @test MOI.get(dest, MOI.ObjectiveSense()) == MOI.get(src, MOI.ObjectiveSense())
    # variable bounds are copied as-is
    for S in (MOI.GreaterThan{Float64}, MOI.LessThan{Float64})
        @test MOI.get(dest, MOI.NumberOfConstraints{MOI.VariableIndex,S}()) ==
              MOI.get(src, MOI.NumberOfConstraints{MOI.VariableIndex,S}())
    end
    # starting values (PowerModels sets vm = 1.0) are copied
    starts = [
        MOI.get(dest, MOI.VariablePrimalStart(), vi) for
        vi in MOI.get(dest, MOI.ListOfVariableIndices())
    ]
    @test any(!isnothing, starts)
    # no scalar function constraint is left ungrouped
    for (F, S) in MOI.get(dest, MOI.ListOfConstraintTypesPresent())
        @test F == MOI.VariableIndex ||
              F <: GenOpt.FunctionGenerator ||
              !(S <: _SCALAR_SETS)
    end
end

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

end # module

TestRegroup.runtests()
