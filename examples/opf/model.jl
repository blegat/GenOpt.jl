# AC-OPF tutorial of ExaModels translated to GenOpt
# See https://exanauts.github.io/ExaModels.jl/stable/opf/
#
# We build the model straight from the `Dict`s that PowerModels returns, indexing the
# variables by the PowerModels component ids (as PowerModels itself does — that gives JuMP
# `DenseAxisArray`s). The constraints use `container = ParametrizedArray` so the index `i`
# is an iterator: GenOpt evaluates the coefficient subexpressions (e.g. `c1..c8`) into
# `IteratorValues` automatically, so we only precompute the data that a PowerModels function
# produces (`calc_branch_y/t`) or that is looked up at an iterator index.

using JuMP, GenOpt, PowerModels

function build_model(pm::Dict)
    PowerModels.standardize_cost_terms!(pm, order = 2)
    PowerModels.calc_thermal_limits!(pm)
    ref = PowerModels.build_ref(pm)[:it][:pm][:nw][0]

    # Arcs are `(branch, from_bus, to_bus)` tuples without a natural scalar id, so the flow
    # variables `p`/`q` are indexed by their position in `ref[:arcs]`.
    narc = length(ref[:arcs])
    arcdict = Dict(a => k for (k, a) in enumerate(ref[:arcs]))

    # Generator / objective data (looked up at an iterator index)
    cost1 = Dict(k => v["cost"][1] for (k, v) in ref[:gen])
    cost2 = Dict(k => v["cost"][2] for (k, v) in ref[:gen])
    cost3 = Dict(k => v["cost"][3] for (k, v) in ref[:gen])
    gen_bus = Dict(k => v["gen_bus"] for (k, v) in ref[:gen])
    arc_bus = Dict(k => i for (k, (l, i, j)) in enumerate(ref[:arcs]))

    # Bus aggregated load / shunt data
    bus_pd = Dict(k => sum(ref[:load][l]["pd"] for l in ref[:bus_loads][k]; init = 0.0) for (k, v) in ref[:bus])
    bus_qd = Dict(k => sum(ref[:load][l]["qd"] for l in ref[:bus_loads][k]; init = 0.0) for (k, v) in ref[:bus])
    bus_gs = Dict(k => sum(ref[:shunt][s]["gs"] for s in ref[:bus_shunts][k]; init = 0.0) for (k, v) in ref[:bus])
    bus_bs = Dict(k => sum(ref[:shunt][s]["bs"] for s in ref[:bus_shunts][k]; init = 0.0) for (k, v) in ref[:bus])

    # The series admittance / transformer ratio come from PowerModels functions, so we precompute
    # them (together with the topology and the shunt terms); the coefficients `c1..c8` are then
    # plain algebra written inline in the constraints.
    g = Dict{Int,Float64}()
    b = Dict{Int,Float64}()
    tr = Dict{Int,Float64}()
    ti = Dict{Int,Float64}()
    ttm = Dict{Int,Float64}()
    g_fr = Dict{Int,Float64}()
    b_fr = Dict{Int,Float64}()
    g_to = Dict{Int,Float64}()
    b_to = Dict{Int,Float64}()
    rate_a_sq = Dict{Int,Float64}()
    f_idx = Dict{Int,Int}()
    t_idx = Dict{Int,Int}()
    f_bus = Dict{Int,Int}()
    t_bus = Dict{Int,Int}()
    angmin = Dict{Int,Float64}()
    angmax = Dict{Int,Float64}()
    for (k, branch) in ref[:branch]
        g[k], b[k] = PowerModels.calc_branch_y(branch)
        tr[k], ti[k] = PowerModels.calc_branch_t(branch)
        ttm[k] = tr[k]^2 + ti[k]^2
        g_fr[k] = branch["g_fr"]
        b_fr[k] = branch["b_fr"]
        g_to[k] = branch["g_to"]
        b_to[k] = branch["b_to"]
        rate_a_sq[k] = branch["rate_a"]^2
        f_idx[k] = arcdict[(k, branch["f_bus"], branch["t_bus"])]
        t_idx[k] = arcdict[(k, branch["t_bus"], branch["f_bus"])]
        f_bus[k] = branch["f_bus"]
        t_bus[k] = branch["t_bus"]
        angmin[k] = branch["angmin"]
        angmax[k] = branch["angmax"]
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

    @constraint(
        model,
        [i in keys(ref[:branch])],
        p[f_idx[i]] == (g[i] + g_fr[i]) / ttm[i] * vm[f_bus[i]]^2 +
        (-g[i] * tr[i] + b[i] * ti[i]) / ttm[i] *
        (vm[f_bus[i]] * vm[t_bus[i]] * cos(va[f_bus[i]] - va[t_bus[i]])) +
        (-b[i] * tr[i] - g[i] * ti[i]) / ttm[i] *
        (vm[f_bus[i]] * vm[t_bus[i]] * sin(va[f_bus[i]] - va[t_bus[i]])),
        container = container,
    )

    @constraint(
        model,
        [i in keys(ref[:branch])],
        q[f_idx[i]] +
        (b[i] + b_fr[i]) / ttm[i] * vm[f_bus[i]]^2 +
        (-b[i] * tr[i] - g[i] * ti[i]) / ttm[i] *
        (vm[f_bus[i]] * vm[t_bus[i]] * cos(va[f_bus[i]] - va[t_bus[i]])) ==
        (-g[i] * tr[i] + b[i] * ti[i]) / ttm[i] *
        (vm[f_bus[i]] * vm[t_bus[i]] * sin(va[f_bus[i]] - va[t_bus[i]])),
        container = container,
    )

    @constraint(
        model,
        [i in keys(ref[:branch])],
        p[t_idx[i]] - (g[i] + g_to[i]) * vm[t_bus[i]]^2 -
        (-g[i] * tr[i] - b[i] * ti[i]) / ttm[i] *
        (vm[t_bus[i]] * vm[f_bus[i]] * cos(va[t_bus[i]] - va[f_bus[i]])) ==
        (-b[i] * tr[i] + g[i] * ti[i]) / ttm[i] *
        (vm[t_bus[i]] * vm[f_bus[i]] * sin(va[t_bus[i]] - va[f_bus[i]])),
        container = container,
    )

    @constraint(
        model,
        [i in keys(ref[:branch])],
        q[t_idx[i]] +
        (b[i] + b_to[i]) * vm[t_bus[i]]^2 +
        (-b[i] * tr[i] + g[i] * ti[i]) / ttm[i] *
        (vm[t_bus[i]] * vm[f_bus[i]] * cos(va[t_bus[i]] - va[f_bus[i]])) ==
        (-g[i] * tr[i] - b[i] * ti[i]) / ttm[i] *
        (vm[t_bus[i]] * vm[f_bus[i]] * sin(va[t_bus[i]] - va[f_bus[i]])),
        container = container,
    )

    @constraint(
        model,
        [i in keys(ref[:branch])],
        angmin[i] <= va[f_bus[i]] - va[t_bus[i]] <= angmax[i],
        container = container,
    )

    # |S|^2 <= rate_a
    @constraint(
        model,
        [i in keys(ref[:branch])],
        p[f_idx[i]]^2 + q[f_idx[i]]^2 <= rate_a_sq[i],
        container = container,
    )
    @constraint(
        model,
        [i in keys(ref[:branch])],
        p[t_idx[i]]^2 + q[t_idx[i]]^2 <= rate_a_sq[i],
        container = container,
    )

    @constraint(
        model,
        [i in keys(ref[:bus])],
        bus_pd[i] == -bus_gs[i] * vm[i]^2 -
        lazy_sum(p[j] for j in 1:narc if arc_bus[j] == i) +
        lazy_sum(pg[j] for j in keys(ref[:gen]) if gen_bus[j] == i),
        container = container,
    )

    @constraint(
        model,
        [i in keys(ref[:bus])],
        bus_qd[i] == bus_bs[i] * vm[i]^2 -
        lazy_sum(q[j] for j in 1:narc if arc_bus[j] == i) +
        lazy_sum(qg[j] for j in keys(ref[:gen]) if gen_bus[j] == i),
        container = container,
    )
    return model
end
