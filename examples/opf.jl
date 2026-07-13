# AC-OPF tutorial of ExaModels translated to GenOpt
# See https://exanauts.github.io/ExaModels.jl/stable/opf/

using JuMP, GenOpt

data = parse_ac_power_data(case)

container = ParametrizedArray

model = Model()

@variable(model, va[eachindex(data.bus)])
@variable(
    model,
    data.vmin[i] <= vm[i in eachindex(data.bus)] <= data.vmax[i],
    start = 1.0,
)
@variable(
    model,
    data.pmin[i] <= pg[i in eachindex(data.gen)] <= data.pmax[i],
)
@variable(
    model,
    data.qmin[i] <= qg[i in eachindex(data.gen)] <= data.qmax[i],
)
@variable(
    model,
    -data.rate_a[i] <= p[i in eachindex(data.arc)] <= data.rate_a[i],
)
@variable(
    model,
    -data.rate_a[i] <= q[i in eachindex(data.arc)] <= data.rate_a[i],
)

cost1 = getfield.(data.gen, :cost1)
cost2 = getfield.(data.gen, :cost2)
cost3 = getfield.(data.gen, :cost3)
gen_id = getfield.(data.gen, :i)
gen_bus = getfield.(data.gen, :bus)

@objective(
    model,
    Min,
    lazy_sum(cost1[i] * pg[gen_id[i]]^2 + cost2[i] * pg[gen_id[i]] + cost3[i] for i in eachindex(data.gen)),
)

#@constraint(model, [i in data.ref_buses], va[i] in MOI.EqualTo(0.0), container = container)
@constraint(model, [i in data.ref_buses], va[i] == 0, container = container)

branch_c1 = getfield.(data.branch, :c1)
branch_c2 = getfield.(data.branch, :c2)
branch_c3 = getfield.(data.branch, :c3)
branch_c4 = getfield.(data.branch, :c4)
branch_c5 = getfield.(data.branch, :c5)
branch_c6 = getfield.(data.branch, :c6)
branch_c7 = getfield.(data.branch, :c7)
branch_c8 = getfield.(data.branch, :c8)
branch_f_idx = getfield.(data.branch, :f_idx)
branch_f_bus = getfield.(data.branch, :f_bus)
branch_t_idx = getfield.(data.branch, :t_idx)
branch_t_bus = getfield.(data.branch, :t_bus)
branch_rate_a_sq = getfield.(data.branch, :rate_a_sq)

@constraint(
    model,
    [i in eachindex(branch_f_idx)],
    p[branch_f_idx[i]] == branch_c5[i] * vm[branch_f_bus[i]]^2 +
    branch_c3[i] * (vm[branch_f_bus[i]] * vm[branch_t_bus[i]] * cos(va[branch_f_bus[i]] - va[branch_t_bus[i]])) +
    branch_c4[i] * (vm[branch_f_bus[i]] * vm[branch_t_bus[i]] * sin(va[branch_f_bus[i]] - va[branch_t_bus[i]])),
)

@constraint(
    model,
    [i in eachindex(branch_f_idx)],
    q[branch_f_idx[i]] +
    branch_c6[i] * vm[branch_f_bus[i]]^2 +
    branch_c4[i] * (vm[branch_f_bus[i]] * vm[branch_t_bus[i]] * cos(va[branch_f_bus[i]] - va[branch_t_bus[i]])) ==
    branch_c3[i] * (vm[branch_f_bus[i]] * vm[branch_t_bus[i]] * sin(va[branch_f_bus[i]] - va[branch_t_bus[i]])),
)

@constraint(
    model,
    [i in eachindex(branch_t_idx)],
    p[branch_t_idx[i]] - branch_c7[i] * vm[branch_t_bus[i]]^2 -
    branch_c1[i] * (vm[branch_t_bus[i]] * vm[branch_f_bus[i]] * cos(va[branch_t_bus[i]] - va[branch_f_bus[i]])) ==
    branch_c2[i] * (vm[branch_t_bus[i]] * vm[branch_f_bus[i]] * sin(va[branch_t_bus[i]] - va[branch_f_bus[i]])),
)

@constraint(
    model,
    [i in eachindex(branch_t_idx)],
    q[branch_t_idx[i]] +
    branch_c8[i] * vm[branch_t_bus[i]]^2 +
    branch_c2[i] * (vm[branch_t_bus[i]] * vm[branch_f_bus[i]] * cos(va[branch_t_bus[i]] - va[branch_f_bus[i]])) ==
    branch_c1[i] * (vm[branch_t_bus[i]] * vm[branch_f_bus[i]] * sin(va[branch_t_bus[i]] - va[branch_f_bus[i]])),
)

# |S|^2 <= rate_a
@constraint(
    model,
    [i in eachindex(branch_f_idx)],
    p[branch_f_idx[i]]^2 + q[branch_f_idx[i]]^2 <= branch_rate_a_sq[i],
)
@constraint(
    model,
    [i in eachindex(branch_t_idx)],
    p[branch_t_idx[i]]^2 + q[branch_t_idx[i]]^2 <= branch_rate_a_sq[i],
)

bus_pd = getfield.(data.bus, :pd)
bus_qd = getfield.(data.bus, :qd)
bus_gs = getfield.(data.bus, :gs)
bus_bs = getfield.(data.bus, :bs)
bus_id = getfield.(data.bus, :i)

arc_id = getfield.(data.arc, :i)
arc_bus = getfield.(data.arc, :bus)

@constraint(
    model,
    [i in eachindex(bus_id)],
    bus_pd[i] == -bus_gs[i] * vm[bus_id[i]]^2 -
    lazy_sum(p[arc_id[j]] for j in eachindex(arc_id) if arc_bus[j] == i) +
    lazy_sum(pg[gen_id[j]] for j in eachindex(gen_id) if gen_bus[j] == i),
)

@constraint(
    model,
    [i in eachindex(bus_id)],
    bus_qd[i] == bus_bs[i] * vm[bus_id[i]]^2 -
    lazy_sum(q[arc_id[j]] for j in eachindex(arc_id) if arc_bus[j] == i) +
    lazy_sum(qg[gen_id[j]] for j in eachindex(gen_id) if gen_bus[j] == i),
)

import MadNLP
import ExaModels
# Needs https://github.com/exanauts/ExaModels.jl/pull/237
set_optimizer(model, () -> ExaModels.Optimizer(MadNLP.madnlp))
optimize!(model)
value.(vm)
value.(pg)
