# Compare the ExaModels internals obtained from the GenOpt AC-OPF model
# (GenOpt/examples/opf/model.jl) when built
#   (a) with `container = ParametrizedArray` (structured GenOpt path), vs
#   (b) without it (scalar constraints, structure re-discovered by ExaModelsMOI).
using JuMP, GenOpt
import PowerModels
import ExaModels
import MathOptInterface as MOI
import Random
import SparseArrays: sparse
import LinearAlgebra: Diagonal, diag
import ExaModels: NLPModels

const ExaMOI = Base.get_extension(ExaModels, :ExaModelsMOI)

const CASE = "/home/node/.julia/packages/PGLib/LWTKI/test/pglib_opf_case3_lmbd.m"

const ALL_CONS = [
    :refbus, :p_from, :q_from, :p_to, :q_to, :ang, :s_from, :s_to, :p_bal, :q_bal,
]

function build_model(
    pm::Dict;
    container,
    lazy::Bool,
    enabled = ALL_CONS,
    objective::Bool = true,
)
    # `lazy = true` uses GenOpt's `lazy_sum` (structured path); `lazy = false` uses a plain
    # Julia `sum` so the model is vanilla JuMP (`init` handles buses without generators)
    SUM = lazy ? lazy_sum : (g -> sum(g; init = 0.0))
    PowerModels.standardize_cost_terms!(pm, order = 2)
    PowerModels.calc_thermal_limits!(pm)
    ref = PowerModels.build_ref(pm)[:it][:pm][:nw][0]

    narc = length(ref[:arcs])
    arcdict = Dict(a => k for (k, a) in enumerate(ref[:arcs]))

    cost1 = Dict(k => v["cost"][1] for (k, v) in ref[:gen])
    cost2 = Dict(k => v["cost"][2] for (k, v) in ref[:gen])
    cost3 = Dict(k => v["cost"][3] for (k, v) in ref[:gen])
    gen_bus = Dict(k => v["gen_bus"] for (k, v) in ref[:gen])
    arc_bus = Dict(k => i for (k, (l, i, j)) in enumerate(ref[:arcs]))

    bus_pd = Dict(k => sum(ref[:load][l]["pd"] for l in ref[:bus_loads][k]; init = 0.0) for (k, v) in ref[:bus])
    bus_qd = Dict(k => sum(ref[:load][l]["qd"] for l in ref[:bus_loads][k]; init = 0.0) for (k, v) in ref[:bus])
    bus_gs = Dict(k => sum(ref[:shunt][s]["gs"] for s in ref[:bus_shunts][k]; init = 0.0) for (k, v) in ref[:bus])
    bus_bs = Dict(k => sum(ref[:shunt][s]["bs"] for s in ref[:bus_shunts][k]; init = 0.0) for (k, v) in ref[:bus])

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

    model = Model()

    @variable(model, va[keys(ref[:bus])])
    @variable(model, ref[:bus][i]["vmin"] <= vm[i in keys(ref[:bus])] <= ref[:bus][i]["vmax"], start = 1.0)
    @variable(model, ref[:gen][i]["pmin"] <= pg[i in keys(ref[:gen])] <= ref[:gen][i]["pmax"])
    @variable(model, ref[:gen][i]["qmin"] <= qg[i in keys(ref[:gen])] <= ref[:gen][i]["qmax"])
    @variable(model, -ref[:branch][ref[:arcs][i][1]]["rate_a"] <= p[i in 1:narc] <= ref[:branch][ref[:arcs][i][1]]["rate_a"])
    @variable(model, -ref[:branch][ref[:arcs][i][1]]["rate_a"] <= q[i in 1:narc] <= ref[:branch][ref[:arcs][i][1]]["rate_a"])

    if objective
        @objective(
            model,
            Min,
            SUM(cost1[i] * pg[i]^2 + cost2[i] * pg[i] + cost3[i] for i in keys(ref[:gen])),
        )
    else
        # ExaModelsMOI rejects FEASIBILITY_SENSE, use a trivial objective
        @objective(model, Min, 0)
    end

    if :refbus in enabled
        @constraint(model, [i in ref_buses], va[i] == 0, container = container)
    end

    if :p_from in enabled
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
    end

    if :q_from in enabled
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
    end

    if :p_to in enabled
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
    end

    if :q_to in enabled
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
    end

    if :ang in enabled
        @constraint(
            model,
            [i in keys(ref[:branch])],
            angmin[i] <= va[f_bus[i]] - va[t_bus[i]] <= angmax[i],
            container = container,
        )
    end

    if :s_from in enabled
        @constraint(
            model,
            [i in keys(ref[:branch])],
            p[f_idx[i]]^2 + q[f_idx[i]]^2 <= rate_a_sq[i],
            container = container,
        )
    end
    if :s_to in enabled
        @constraint(
            model,
            [i in keys(ref[:branch])],
            p[t_idx[i]]^2 + q[t_idx[i]]^2 <= rate_a_sq[i],
            container = container,
        )
    end

    if :p_bal in enabled
        @constraint(
            model,
            [i in keys(ref[:bus])],
            bus_pd[i] == -bus_gs[i] * vm[i]^2 -
            SUM(p[j] for j in 1:narc if arc_bus[j] == i) +
            SUM(pg[j] for j in keys(ref[:gen]) if gen_bus[j] == i),
            container = container,
        )
    end

    if :q_bal in enabled
        @constraint(
            model,
            [i in keys(ref[:bus])],
            bus_qd[i] == bus_bs[i] * vm[i]^2 -
            SUM(q[j] for j in 1:narc if arc_bus[j] == i) +
            SUM(qg[j] for j in keys(ref[:gen]) if gen_bus[j] == i),
            container = container,
        )
    end
    return model
end

function to_core(model)
    moim = JuMP.backend(model).model_cache
    core, _ = ExaMOI.to_exacore(moim)
    return core
end

_kind(x) = string(nameof(typeof(x)))

function structure(core)
    entries = Tuple{String,String,Int}[]
    for con in core.cons
        push!(entries, (_kind(con), ExaModels._expr_string(con.f.f), length(con.itr)))
    end
    for o in core.obj
        push!(entries, (("Objective"), ExaModels._expr_string(o.f.f), length(o.itr)))
    end
    return entries
end

function print_structure(io, label, core)
    println(io, "== $label ==")
    for (k, e, n) in structure(core)
        println(io, "  [$k |I|=$n]  $e")
    end
end

_symmetrize(H) = H + H' - Diagonal(diag(H))

function numeric_report(io, core1, core2)
    m1 = ExaModels.ExaModel(core1)
    m2 = ExaModels.ExaModel(core2)
    n1, n2 = NLPModels.get_nvar(m1), NLPModels.get_nvar(m2)
    p1, p2 = NLPModels.get_ncon(m1), NLPModels.get_ncon(m2)
    println(io, "  nvar: $n1 vs $n2   ncon: $p1 vs $p2")
    if n1 != n2 || p1 != p2
        println(io, "  SIZE MISMATCH")
        return false
    end
    Random.seed!(42)
    x = randn(n1)
    y = randn(p1)
    ok = true
    # subtracting equal infinite bounds gives NaN, so compare with a bounded diff
    bdiff(a, b) = maximum(abs.(clamp.(a, -1e30, 1e30) - clamp.(b, -1e30, 1e30)); init = 0.0)
    dlv = bdiff(m1.meta.lvar, m2.meta.lvar)
    duv = bdiff(m1.meta.uvar, m2.meta.uvar)
    o1, o2 = NLPModels.obj(m1, x), NLPModels.obj(m2, x)
    g1, g2 = NLPModels.grad(m1, x), NLPModels.grad(m2, x)
    c1, c2 = NLPModels.cons(m1, x), NLPModels.cons(m2, x)
    # residuals relative to bounds are invariant to moving constants between
    # function and set
    r1l = c1 - m1.meta.lcon
    r2l = c2 - m2.meta.lcon
    r1u = m1.meta.ucon - c1
    r2u = m2.meta.ucon - c2
    J1 = sparse(NLPModels.jac_structure(m1)..., NLPModels.jac_coord(m1, x), p1, n1)
    J2 = sparse(NLPModels.jac_structure(m2)..., NLPModels.jac_coord(m2, x), p2, n2)
    H1 = _symmetrize(sparse(NLPModels.hess_structure(m1)..., NLPModels.hess_coord(m1, x, y), n1, n1))
    H2 = _symmetrize(sparse(NLPModels.hess_structure(m2)..., NLPModels.hess_coord(m2, x, y), n2, n2))
    function report(name, d)
        stat = d <= 1e-10 ? "OK " : "DIFF"
        d > 1e-10 && (ok = false)
        println(io, "  $stat $name: max abs diff = $d")
    end
    report("lvar", dlv)
    report("uvar", duv)
    report("obj ", abs(o1 - o2))
    report("grad", maximum(abs, g1 - g2; init = 0.0))
    report("lcon res", maximum(abs, filter(isfinite, r1l - r2l); init = 0.0))
    report("ucon res", maximum(abs, filter(isfinite, r1u - r2u); init = 0.0))
    report("jac ", maximum(abs, J1 - J2; init = 0.0))
    report("hess", maximum(abs, H1 - H2; init = 0.0))
    if !ok && p1 > 0
        # constraint rows may be ordered differently (scalar path groups by MOI
        # constraint type); find the permutation matching rows on
        # (residuals, dense jacobian row) and re-check everything under it
        key(J, rl, ru, i) = (clamp(rl[i], -1e30, 1e30), clamp(ru[i], -1e30, 1e30), Vector(J[i, :])...)
        perm1 = sortperm([key(J1, r1l, r1u, i) for i in 1:p1])
        perm2 = sortperm([key(J2, r2l, r2u, i) for i in 1:p1])
        dres = maximum(abs, filter(isfinite, r1l[perm1] - r2l[perm2]); init = 0.0)
        djac = maximum(abs, J1[perm1, :] - J2[perm2, :]; init = 0.0)
        # permute the multipliers consistently and recompute the second Hessian
        y2 = zeros(p1)
        y2[perm2] = y[perm1]
        H2b = _symmetrize(sparse(NLPModels.hess_structure(m2)..., NLPModels.hess_coord(m2, x, y2), n2, n2))
        dhess = maximum(abs, H1 - H2b; init = 0.0)
        println(io, "  after row-permutation matching: res diff = $dres, jac diff = $djac, hess diff = $dhess")
        ok = dres <= 1e-10 && djac <= 1e-10 && dhess <= 1e-10
        ok && println(io, "  => identical up to row permutation")
    end
    return ok
end

function compare(pm; enabled = ALL_CONS, objective = true, io = stdout)
    m_struct = build_model(deepcopy(pm); container = ParametrizedArray, lazy = true, enabled, objective)
    m_scalar = build_model(deepcopy(pm); container = JuMP.Containers.AutoContainerType, lazy = false, enabled, objective)
    core_struct = to_core(m_struct)
    core_scalar = to_core(m_scalar)
    print_structure(io, "structured (container = ParametrizedArray)", core_struct)
    print_structure(io, "scalar (no container)", core_scalar)
    numeric_report(io, core_struct, core_scalar)
end

function main(io = stdout)
    pm = PowerModels.parse_file(CASE)
    for con in ALL_CONS
        println(io, "\n######## constraint: $con (no objective) ########")
        compare(pm; enabled = [con], objective = false, io)
    end
    println(io, "\n######## objective only ########")
    compare(pm; enabled = Symbol[], objective = true, io)
    println(io, "\n######## full model ########")
    compare(pm; enabled = ALL_CONS, objective = true, io)
end
