using JuMP, GenOpt

model = Model()
@variable(model, x)
@variable(model, y[1:2])

d1 = Dict(:a => -1, :b => 1)
d2 = Dict(:a => π, :b => 0.0)

i = GenOpt.iterator([:a, :b])

d1[i]
d2[i]


x + d1[i] - d2[i]


c = @constraint(model, [i in [:a, :b]], x + d1[i] >= d2[i], container = ParametrizedArray)

f = MOI.ScalarNonlinearFunction(:getindex, [y, 2]);
x
