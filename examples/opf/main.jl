import MadNLP
import ExaModels
import PGLib
model = build_model(PGLib.pglib("case3_lmbd"));
# Needs https://github.com/exanauts/ExaModels.jl/pull/237
set_optimizer(model, () -> ExaModels.Optimizer(MadNLP.madnlp))
optimize!(model)
value.(model[:vm])
value.(model[:pg])
