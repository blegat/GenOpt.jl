# AC-OPF tutorial of ExaModels translated to GenOpt
# See https://exanauts.github.io/ExaModels.jl/stable/opf/
#
# We build the model straight from the `Dict`s that PowerModels returns, indexing the
# variables by the PowerModels component ids (as PowerModels itself does — that gives JuMP
# `DenseAxisArray`s). GenOpt indexes `Dict`s / `DenseAxisArray`s by an iterator, and it
# *evaluates* subexpressions that only involve iterators (no variables), so most data can be
# read straight from `ref` inline. We only precompute a `Dict` when a value is looked up at
# an iterator index (`gen_bus`/`arc_bus` in the balance filters, `cost*` in the objective)
# or when it needs a PowerModels function that GenOpt can't evaluate (`calc_branch_y/t`).

using JuMP, GenOpt, PowerModels

case = tempname() * ".m"

import Downloads
Downloads.download(
    "https://raw.githubusercontent.com/power-grid-lib/pglib-opf/dc6be4b2f85ca0e776952ec22cbd4c22396ea5a3/pglib_opf_case3_lmbd.m",
    case,
)

pm = PowerModels.parse_file(case)
PowerModels.standardize_cost_terms!(pm, order = 2)
PowerModels.calc_thermal_limits!(pm)
ref = PowerModels.build_ref(pm)[:it][:pm][:nw][0]

# Arcs are `(branch, from_bus, to_bus)` tuples without a natural scalar id, so the flow
# variables `p`/`q` are indexed by their position in `ref[:arcs]`.
narc = length(ref[:arcs])
arcdict = Dict(a => k for (k, a) in enumerate(ref[:arcs]))

# Looked up at an iterator index (objective / balance filters), so kept as `Dict`s.
cost1 = Dict(k => v["cost"][1] for (k, v) in ref[:gen])
cost2 = Dict(k => v["cost"][2] for (k, v) in ref[:gen])
cost3 = Dict(k => v["cost"][3] for (k, v) in ref[:gen])
gen_bus = Dict(k => v["gen_bus"] for (k, v) in ref[:gen])
arc_bus = Dict(k => i for (k, (l, i, j)) in enumerate(ref[:arcs]))

# The series admittance and transformer ratio come from PowerModels functions, so we
# precompute them; the coefficients `c1..c8` are then plain algebra and are written inline.
g = Dict{Int,Float64}()
b = Dict{Int,Float64}()
tr = Dict{Int,Float64}()
ti = Dict{Int,Float64}()
ttm = Dict{Int,Float64}()
f_idx = Dict{Int,Int}()
t_idx = Dict{Int,Int}()
f_bus = Dict{Int,Int}()
t_bus = Dict{Int,Int}()
for (k, branch) in ref[:branch]
    g[k], b[k] = PowerModels.calc_branch_y(branch)
    tr[k], ti[k] = PowerModels.calc_branch_t(branch)
    ttm[k] = tr[k]^2 + ti[k]^2
    f_idx[k] = arcdict[(k, branch["f_bus"], branch["t_bus"])]
    t_idx[k] = arcdict[(k, branch["t_bus"], branch["f_bus"])]
    f_bus[k] = branch["f_bus"]
    t_bus[k] = branch["t_bus"]
end

ref_buses = collect(keys(ref[:ref_buses]))

container = ParametrizedArray

model = Model()

@variable(model, va[keys(ref[:bus])])
@variable(model, ref[:bus][i]["vmin"] <= vm[i in keys(ref[:bus])] <= ref[:bus][i]["vmax"], start = 1.0)
@variable(model, ref[:gen][i]["pmin"] <= pg[i in keys(ref[:gen])] <= ref[:gen][i]["pmax"])
@variable(model, ref[:gen][i]["qmin"] <= qg[i in keys(ref[:gen])] <= ref[:gen][i]["qmax"])
@variable(model, -ref[:branch][ref[:arcs][i][1]]["rate_a"] <= p[i in 1:narc] <= ref[:branch][ref[:arcs][i][1]]["rate_a"])
@variable(model, -ref[:branch][ref[:arcs][i][1]]["rate_a"] <= q[i in 1:narc] <= ref[:branch][ref[:arcs][i][1]]["rate_a"])

@objective(
    model,
    Min,
    lazy_sum(cost1[i] * pg[i]^2 + cost2[i] * pg[i] + cost3[i] for i in keys(ref[:gen])),
)

@constraint(model, [i in ref_buses], va[i] == 0, container = container)

# For the branch constraints the index `i` is a concrete branch id, so `ref[:branch][i][...]`
# and the coefficients `c1..c8` are evaluated to plain numbers inline.
@constraint(
    model,
    [i in keys(ref[:branch])],
    p[f_idx[i]] == (g[i] + ref[:branch][i]["g_fr"]) / ttm[i] * vm[f_bus[i]]^2 +
    (-g[i] * tr[i] + b[i] * ti[i]) / ttm[i] *
    (vm[f_bus[i]] * vm[t_bus[i]] * cos(va[f_bus[i]] - va[t_bus[i]])) +
    (-b[i] * tr[i] - g[i] * ti[i]) / ttm[i] *
    (vm[f_bus[i]] * vm[t_bus[i]] * sin(va[f_bus[i]] - va[t_bus[i]])),
)

@constraint(
    model,
    [i in keys(ref[:branch])],
    q[f_idx[i]] +
    (b[i] + ref[:branch][i]["b_fr"]) / ttm[i] * vm[f_bus[i]]^2 +
    (-b[i] * tr[i] - g[i] * ti[i]) / ttm[i] *
    (vm[f_bus[i]] * vm[t_bus[i]] * cos(va[f_bus[i]] - va[t_bus[i]])) ==
    (-g[i] * tr[i] + b[i] * ti[i]) / ttm[i] *
    (vm[f_bus[i]] * vm[t_bus[i]] * sin(va[f_bus[i]] - va[t_bus[i]])),
)

@constraint(
    model,
    [i in keys(ref[:branch])],
    p[t_idx[i]] - (g[i] + ref[:branch][i]["g_to"]) * vm[t_bus[i]]^2 -
    (-g[i] * tr[i] - b[i] * ti[i]) / ttm[i] *
    (vm[t_bus[i]] * vm[f_bus[i]] * cos(va[t_bus[i]] - va[f_bus[i]])) ==
    (-b[i] * tr[i] + g[i] * ti[i]) / ttm[i] *
    (vm[t_bus[i]] * vm[f_bus[i]] * sin(va[t_bus[i]] - va[f_bus[i]])),
)

@constraint(
    model,
    [i in keys(ref[:branch])],
    q[t_idx[i]] +
    (b[i] + ref[:branch][i]["b_to"]) * vm[t_bus[i]]^2 +
    (-b[i] * tr[i] + g[i] * ti[i]) / ttm[i] *
    (vm[t_bus[i]] * vm[f_bus[i]] * cos(va[t_bus[i]] - va[f_bus[i]])) ==
    (-g[i] * tr[i] - b[i] * ti[i]) / ttm[i] *
    (vm[t_bus[i]] * vm[f_bus[i]] * sin(va[t_bus[i]] - va[f_bus[i]])),
)

# |S|^2 <= rate_a
@constraint(
    model,
    [i in keys(ref[:branch])],
    p[f_idx[i]]^2 + q[f_idx[i]]^2 <= ref[:branch][i]["rate_a"]^2,
)
@constraint(
    model,
    [i in keys(ref[:branch])],
    p[t_idx[i]]^2 + q[t_idx[i]]^2 <= ref[:branch][i]["rate_a"]^2,
)

@constraint(
    model,
    [i in keys(ref[:bus])],
    sum(ref[:load][l]["pd"] for l in ref[:bus_loads][i]; init = 0.0) ==
    -sum(ref[:shunt][s]["gs"] for s in ref[:bus_shunts][i]; init = 0.0) * vm[i]^2 -
    lazy_sum(p[j] for j in 1:narc if arc_bus[j] == i) +
    lazy_sum(pg[j] for j in keys(ref[:gen]) if gen_bus[j] == i),
)

@constraint(
    model,
    [i in keys(ref[:bus])],
    sum(ref[:load][l]["qd"] for l in ref[:bus_loads][i]; init = 0.0) ==
    sum(ref[:shunt][s]["bs"] for s in ref[:bus_shunts][i]; init = 0.0) * vm[i]^2 -
    lazy_sum(q[j] for j in 1:narc if arc_bus[j] == i) +
    lazy_sum(qg[j] for j in keys(ref[:gen]) if gen_bus[j] == i),
)

import MadNLP
import ExaModels
# Needs https://github.com/exanauts/ExaModels.jl/pull/237
set_optimizer(model, () -> ExaModels.Optimizer(MadNLP.madnlp))
optimize!(model)
value.(vm)
value.(pg)
