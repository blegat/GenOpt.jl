import MadNLP
import ExaModels

include(joinpath(@__DIR__, "model.jl"))
model = build_model()

# Needs https://github.com/exanauts/ExaModels.jl/pull/237
set_optimizer(model, () -> ExaModels.Optimizer(MadNLP.madnlp))
optimize!(model)
value.(x)
value.(u)
