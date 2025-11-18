# Quadrotor tutorial of ExaModels translated to GenOpt
# See https://exanauts.github.io/ExaModels.jl/stable/quad/

N = 3

n = 9
p = 4
d(i, j, N) =
    (j == 1 ? 1 * sin(2 * pi / N * i) : 0.0) +
    (j == 3 ? 2 * sin(4 * pi / N * i) : 0.0) +
    (j == 5 ? 2 * i / N : 0.0)
dt = 1/N
R = fill(1 / 10, 4)
Q = [1, 0, 1, 0, 1, 0, 1, 1, 1]
Qf = [1, 0, 1, 0, 1, 0, 1, 1, 1] / dt

x0 = zeros(n)

using JuMP
model = Model()

@variable(model, x[1:(N+1), 1:n])
@variable(model, u[1:N, 1:p])

using GenOpt
container = ParametrizedArray

@constraint(
    model,
    [i in 1:n],
    x[1, i] == x0[i],
    container = container,
)
@constraint(
    model,
    [i in 1:N],
    x[i+1, 1] == x[i, 1] + (x[i, 2]) * dt,
    container = container,
)
@constraint(
    model,
    [i in 1:N],
    x[i+1, 2] ==
    x[i, 2] +
    (
        u[i, 1] * cos(x[i, 7]) * sin(x[i, 8]) * cos(x[i, 9]) +
        u[i, 1] * sin(x[i, 7]) * sin(x[i, 9])
    ) * dt,
    container = container,
)
@constraint(
    model,
    [i in 1:N],
    x[i+1, 3] == x[i, 3] + (x[i, 4]) * dt,
    container = container,
)
@constraint(
    model,
    [i in 1:N],
    x[i+1, 4] ==
    x[i, 4] +
    (
        u[i, 1] * cos(x[i, 7]) * sin(x[i, 8]) * sin(x[i, 9]) -
        u[i, 1] * sin(x[i, 7]) * cos(x[i, 9])
    ) * dt,
    container = container,
)
@constraint(
    model,
    [i in 1:N],
    x[i+1, 5] == x[i, 5] + (x[i, 6]) * dt,
    container = container,
)
@constraint(
    model,
    [i in 1:N],
    x[i+1, 6] == x[i, 6] + (u[i, 1] * cos(x[i, 7]) * cos(x[i, 8]) - 9.8) * dt,
    container = container,
)
@constraint(
    model,
    [i in 1:N],
    x[i+1, 7] ==
    x[i, 7] +
    (u[i, 2] * cos(x[i, 7]) / cos(x[i, 8]) + u[i, 3] * sin(x[i, 7]) / cos(x[i, 8])) * dt,
    container = container,
)
@constraint(
    model,
    [i in 1:N],
    x[i+1, 8] == x[i, 8] + (-u[i, 2] * sin(x[i, 7]) + u[i, 3] * cos(x[i, 7])) * dt,
    container = container,
)
@constraint(
    model,
    [i in 1:N],
    x[i+1, 9] ==
    x[i, 9] +
    (
        u[i, 2] * cos(x[i, 7]) * tan(x[i, 8]) +
        u[i, 3] * sin(x[i, 7]) * tan(x[i, 8]) +
        u[i, 4]
    ) * dt,
    container = container,
)

ss = lazy_sum(R[j] * u[1, j] for j in 1:p)
ss = lazy_sum(R[j] * (u[i, j]^2) for i in 1:N, j in 1:p)
itr1 = [(i, j, d(i, j, N)) for i in 1:N, j in 1:n]
itr2 = [(j, d(N + 1, j, N)) for j in 1:n]
@objective(
    model,
    Min,
    lazy_sum(0.5 * R[j] * (u[i, j]^2) for i in 1:N, j in 1:p) +
    lazy_sum(0.5 * Q[it[2]] * (x[it[1], it[2]] - it[3])^2 for it in itr1) +
    lazy_sum(0.5 * Qf[it[1]] * (x[N+1, it[1]] - it[2])^2 for it in itr2),
)

using MadNLP
set_optimizer(model, () -> GenOpt.ExaOptimizer(madnlp))
optimize!(model)
value.(x)
value.(u)
