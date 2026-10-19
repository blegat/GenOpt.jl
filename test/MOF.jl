# Copyright (c) 2024: Benoît Legat and contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestMOF

using Test
import GenOpt
import MathOptInterface as MOI

function _round_trip(source)
    writer_model = MOI.FileFormats.MOF.Model(use_nlp_block = false)
    MOI.copy_to(writer_model, source)
    text = sprint(write, writer_model)
    destination = MOI.FileFormats.MOF.Model(use_nlp_block = false)
    read!(IOBuffer(text), destination)
    return text, destination
end

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

    text, destination = _round_trip(source)
    @test occursin("GenOptFunctionGenerator", text)
    @test occursin("GenOptSumGenerator", text)
    @test occursin("GenOptRangeIterator", text)

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

function test_quadratic_nonlinear_and_explicit_iterators()
    source = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    x = MOI.add_variables(source, 3)
    for (i, xi) in enumerate(x)
        MOI.set(source, MOI.VariableName(), xi, "x[$i]")
    end
    x_block = GenOpt.ContiguousArrayOfVariables(0, (3,))
    entry = MOI.ScalarNonlinearFunction(
        :getindex,
        Any[x_block, GenOpt.IteratorIndex(1)],
    )
    # The values of this iterator are tuples of an integer and a symbol so they
    # exercise the explicit encoding instead of the range one.
    pairs = GenOpt.Iterator([(1, :a), (2, :b), (3, :c)])
    generator = GenOpt.FunctionGenerator{MOI.ScalarQuadraticFunction{Float64}}(
        MOI.ScalarNonlinearFunction(:*, Any[entry, entry]),
        [pairs],
    )
    MOI.add_constraint(source, generator, MOI.Zeros(3))
    # The values are integers but they are not evenly spaced so the explicit
    # encoding is used as well.
    objective = GenOpt.SumGenerator{MOI.ScalarNonlinearFunction}(
        MOI.ScalarNonlinearFunction(:log, Any[entry]),
        [GenOpt.Iterator([1, 2, 5])],
    )
    MOI.set(source, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(source, MOI.ObjectiveFunction{typeof(objective)}(), objective)

    text, destination = _round_trip(source)
    @test occursin("ScalarQuadraticFunction", text)
    @test occursin("ScalarNonlinearFunction", text)
    @test occursin("GenOptExplicitIterator", text)
    @test occursin("GenOptTuple", text)
    @test occursin("GenOptSymbol", text)
    @test !occursin("GenOptRangeIterator", text)

    cis = MOI.get(
        destination,
        MOI.ListOfConstraintIndices{typeof(generator),MOI.Zeros}(),
    )
    restored = MOI.get(destination, MOI.ConstraintFunction(), only(cis))
    @test isapprox(restored.func, generator.func)
    @test only(restored.iterators).values == pairs.values
    restored_objective =
        MOI.get(destination, MOI.ObjectiveFunction{typeof(objective)}())
    @test isapprox(restored_objective.func, objective.func)
    @test only(restored_objective.iterators).values ==
          only(objective.iterators).values
    return
end

function test_unsupported_mof_objects()
    @test_throws(
        ErrorException("Unsupported GenOpt iterator value: GenOptVector"),
        GenOpt._mof_iterator_value_to_moi(Dict("type" => "GenOptVector")),
    )
    @test_throws(
        ErrorException("Unsupported GenOpt iterator: GenOptProductIterator"),
        GenOpt._mof_iterator_to_moi(Dict("type" => "GenOptProductIterator")),
    )
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
