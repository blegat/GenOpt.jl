# AC-OPF tutorial of ExaModels translated to GenOpt
# See https://exanauts.github.io/ExaModels.jl/stable/opf/
#
# PowerModels returns its network as `Dict`s keyed by component id, so instead of
# building an intermediate array-of-structs (like `parse_ac_power_data`) and then
# transposing it into per-field vectors, we build the data straight into `Dict`s and
# index them with GenOpt's dictionary support (`d[i]` where `i` is an iterator).

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

# PowerModels component ids can be non-contiguous; remap them to 1:n so the JuMP
# variable blocks are contiguous. `arcdict` maps an arc `(branch, from, to)` to its index.
busdict = Dict(k => i for (i, (k, v)) in enumerate(ref[:bus]))
gendict = Dict(k => i for (i, (k, v)) in enumerate(ref[:gen]))
branchdict = Dict(k => i for (i, (k, v)) in enumerate(ref[:branch]))
arcdict = Dict(a => k for (k, a) in enumerate(ref[:arcs]))

nbus = length(ref[:bus])
ngen = length(ref[:gen])
narc = length(ref[:arcs])
nbranch = length(ref[:branch])

# Bus data (keyed by contiguous bus index)
vmin = Dict(busdict[k] => v["vmin"] for (k, v) in ref[:bus])
vmax = Dict(busdict[k] => v["vmax"] for (k, v) in ref[:bus])
bus_pd = Dict(busdict[k] => sum(ref[:load][l]["pd"] for l in ref[:bus_loads][k]; init = 0.0) for (k, v) in ref[:bus])
bus_qd = Dict(busdict[k] => sum(ref[:load][l]["qd"] for l in ref[:bus_loads][k]; init = 0.0) for (k, v) in ref[:bus])
bus_gs = Dict(busdict[k] => sum(ref[:shunt][s]["gs"] for s in ref[:bus_shunts][k]; init = 0.0) for (k, v) in ref[:bus])
bus_bs = Dict(busdict[k] => sum(ref[:shunt][s]["bs"] for s in ref[:bus_shunts][k]; init = 0.0) for (k, v) in ref[:bus])

# Generator data (keyed by contiguous gen index)
pmin = Dict(gendict[k] => v["pmin"] for (k, v) in ref[:gen])
pmax = Dict(gendict[k] => v["pmax"] for (k, v) in ref[:gen])
qmin = Dict(gendict[k] => v["qmin"] for (k, v) in ref[:gen])
qmax = Dict(gendict[k] => v["qmax"] for (k, v) in ref[:gen])
cost1 = Dict(gendict[k] => v["cost"][1] for (k, v) in ref[:gen])
cost2 = Dict(gendict[k] => v["cost"][2] for (k, v) in ref[:gen])
cost3 = Dict(gendict[k] => v["cost"][3] for (k, v) in ref[:gen])
gen_bus = Dict(gendict[k] => busdict[v["gen_bus"]] for (k, v) in ref[:gen])

# Arc data (keyed by arc index); an arc is `(branch, from_bus, to_bus)`
rate_a = Dict(k => ref[:branch][l]["rate_a"] for (k, (l, i, j)) in enumerate(ref[:arcs]))
arc_bus = Dict(k => busdict[i] for (k, (l, i, j)) in enumerate(ref[:arcs]))

# Branch data (keyed by contiguous branch index)
branch_c1 = Dict{Int,Float64}()
branch_c2 = Dict{Int,Float64}()
branch_c3 = Dict{Int,Float64}()
branch_c4 = Dict{Int,Float64}()
branch_c5 = Dict{Int,Float64}()
branch_c6 = Dict{Int,Float64}()
branch_c7 = Dict{Int,Float64}()
branch_c8 = Dict{Int,Float64}()
branch_f_idx = Dict{Int,Int}()
branch_t_idx = Dict{Int,Int}()
branch_f_bus = Dict{Int,Int}()
branch_t_bus = Dict{Int,Int}()
branch_rate_a_sq = Dict{Int,Float64}()
for (k, branch) in ref[:branch]
    b = branchdict[k]
    y_re, y_im = PowerModels.calc_branch_y(branch)
    tr, ti = PowerModels.calc_branch_t(branch)
    ttm = tr^2 + ti^2
    branch_c1[b] = (-y_re * tr - y_im * ti) / ttm
    branch_c2[b] = (-y_im * tr + y_re * ti) / ttm
    branch_c3[b] = (-y_re * tr + y_im * ti) / ttm
    branch_c4[b] = (-y_im * tr - y_re * ti) / ttm
    branch_c5[b] = (y_re + branch["g_fr"]) / ttm
    branch_c6[b] = (y_im + branch["b_fr"]) / ttm
    branch_c7[b] = (y_re + branch["g_to"])
    branch_c8[b] = (y_im + branch["b_to"])
    branch_f_idx[b] = arcdict[(k, branch["f_bus"], branch["t_bus"])]
    branch_t_idx[b] = arcdict[(k, branch["t_bus"], branch["f_bus"])]
    branch_f_bus[b] = busdict[branch["f_bus"]]
    branch_t_bus[b] = busdict[branch["t_bus"]]
    branch_rate_a_sq[b] = branch["rate_a"]^2
end

ref_buses = [busdict[k] for k in keys(ref[:ref_buses])]

container = ParametrizedArray

model = Model()

@variable(model, va[1:nbus])
@variable(model, vmin[i] <= vm[i in 1:nbus] <= vmax[i], start = 1.0)
@variable(model, pmin[i] <= pg[i in 1:ngen] <= pmax[i])
@variable(model, qmin[i] <= qg[i in 1:ngen] <= qmax[i])
@variable(model, -rate_a[i] <= p[i in 1:narc] <= rate_a[i])
@variable(model, -rate_a[i] <= q[i in 1:narc] <= rate_a[i])

@objective(
    model,
    Min,
    lazy_sum(cost1[i] * pg[i]^2 + cost2[i] * pg[i] + cost3[i] for i in 1:ngen),
)

@constraint(model, [i in ref_buses], va[i] == 0, container = container)

@constraint(
    model,
    [i in 1:nbranch],
    p[branch_f_idx[i]] == branch_c5[i] * vm[branch_f_bus[i]]^2 +
    branch_c3[i] * (vm[branch_f_bus[i]] * vm[branch_t_bus[i]] * cos(va[branch_f_bus[i]] - va[branch_t_bus[i]])) +
    branch_c4[i] * (vm[branch_f_bus[i]] * vm[branch_t_bus[i]] * sin(va[branch_f_bus[i]] - va[branch_t_bus[i]])),
)

@constraint(
    model,
    [i in 1:nbranch],
    q[branch_f_idx[i]] +
    branch_c6[i] * vm[branch_f_bus[i]]^2 +
    branch_c4[i] * (vm[branch_f_bus[i]] * vm[branch_t_bus[i]] * cos(va[branch_f_bus[i]] - va[branch_t_bus[i]])) ==
    branch_c3[i] * (vm[branch_f_bus[i]] * vm[branch_t_bus[i]] * sin(va[branch_f_bus[i]] - va[branch_t_bus[i]])),
)

@constraint(
    model,
    [i in 1:nbranch],
    p[branch_t_idx[i]] - branch_c7[i] * vm[branch_t_bus[i]]^2 -
    branch_c1[i] * (vm[branch_t_bus[i]] * vm[branch_f_bus[i]] * cos(va[branch_t_bus[i]] - va[branch_f_bus[i]])) ==
    branch_c2[i] * (vm[branch_t_bus[i]] * vm[branch_f_bus[i]] * sin(va[branch_t_bus[i]] - va[branch_f_bus[i]])),
)

@constraint(
    model,
    [i in 1:nbranch],
    q[branch_t_idx[i]] +
    branch_c8[i] * vm[branch_t_bus[i]]^2 +
    branch_c2[i] * (vm[branch_t_bus[i]] * vm[branch_f_bus[i]] * cos(va[branch_t_bus[i]] - va[branch_f_bus[i]])) ==
    branch_c1[i] * (vm[branch_t_bus[i]] * vm[branch_f_bus[i]] * sin(va[branch_t_bus[i]] - va[branch_f_bus[i]])),
)

# |S|^2 <= rate_a
@constraint(
    model,
    [i in 1:nbranch],
    p[branch_f_idx[i]]^2 + q[branch_f_idx[i]]^2 <= branch_rate_a_sq[i],
)
@constraint(
    model,
    [i in 1:nbranch],
    p[branch_t_idx[i]]^2 + q[branch_t_idx[i]]^2 <= branch_rate_a_sq[i],
)

@constraint(
    model,
    [i in 1:nbus],
    bus_pd[i] == -bus_gs[i] * vm[i]^2 -
    lazy_sum(p[j] for j in 1:narc if arc_bus[j] == i) +
    lazy_sum(pg[j] for j in 1:ngen if gen_bus[j] == i),
)

@constraint(
    model,
    [i in 1:nbus],
    bus_qd[i] == bus_bs[i] * vm[i]^2 -
    lazy_sum(q[j] for j in 1:narc if arc_bus[j] == i) +
    lazy_sum(qg[j] for j in 1:ngen if gen_bus[j] == i),
)

import MadNLP
import ExaModels
# Needs https://github.com/exanauts/ExaModels.jl/pull/237
set_optimizer(model, () -> ExaModels.Optimizer(MadNLP.madnlp))
optimize!(model)
value.(vm)
value.(pg)
