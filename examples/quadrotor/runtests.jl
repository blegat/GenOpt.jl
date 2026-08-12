using Test

include(joinpath(@__DIR__, "model.jl"))

model = build_model()
@test model isa JuMP.Model
