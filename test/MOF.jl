# Copyright (c) 2024: Benoît Legat and contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestMOF

using Test
import GenOpt
import MathOptInterface as MOI

function test_function_generator_round_trip()
    source = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    x = MOI.add_variables(source, 3)
    for (i, xi) in enumerate(x)
        MOI.set(source, MOI.VariableName(), xi, "x[$i]")
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
    generator = GenOpt.FunctionGenerator{MOI.ScalarAffineFunction{Float64}}(
        template,
        [GenOpt.Iterator(1:3)],
    )
    MOI.add_constraint(source, generator, MOI.Zeros(3))
    objective = GenOpt.SumGenerator{MOI.ScalarAffineFunction{Float64}}(
        template,
        [GenOpt.Iterator(1:3)],
    )
    MOI.set(source, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(source, MOI.ObjectiveFunction{typeof(objective)}(), objective)

    writer_model = MOI.FileFormats.MOF.Model(use_nlp_block = false)
    MOI.copy_to(writer_model, source)
    text = sprint(write, writer_model)
    @test occursin("GenOptFunctionGenerator", text)
    @test occursin("GenOptSumGenerator", text)
    @test occursin("GenOptRangeIterator", text)

    destination = MOI.FileFormats.MOF.Model(use_nlp_block = false)
    read!(IOBuffer(text), destination)
    cis = MOI.get(
        destination,
        MOI.ListOfConstraintIndices{typeof(generator),MOI.Zeros}(),
    )
    @test length(cis) == 1
    restored = MOI.get(destination, MOI.ConstraintFunction(), only(cis))
    @test isapprox(restored, generator)

    restored_objective =
        MOI.get(destination, MOI.ObjectiveFunction{typeof(objective)}())
    @test isapprox(restored_objective.func, objective.func)
    @test only(restored_objective.iterators).values ==
          only(objective.iterators).values
    return
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

end

TestMOF.runtests()
