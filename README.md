# GenOpt

Still very much a draft. I'll add more details later.
When the user uses the container syntax to create constraints, MOI receives each constraint independently so ExaModels.jl has to recover the repeated structure. We discussed during the JuMP-dev hackathon of a way to communicate the repeated structure through MOI more explicitly and this PR is a POC the resulting design.
As a byproduct, this also gives a solution for https://github.com/jump-dev/JuMP.jl/issues/1654 since the constraint will be expanded at the MOI level with a bridge from FunctionGenerator in Zeros to Scalar?Function in EqualTo (we need a way to know the scalar function type but that should be doable)
We can also recover the pretty printing of containers that we had in JuMP v0.18.
Started as https://github.com/jump-dev/JuMP.jl/pull/3890
