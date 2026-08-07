using PowerModels
using Ipopt
using JuMP
using JSON

function run_pf(file, solver; kwargs...)
    return run_generic_model(file, PowerModels.ACRPowerModel, solver, post_pf)
end

function post_pf(pm::GenericPowerModel)
    start_time = time()
    PowerModels.variable_voltage(pm, bounded=false)
    PowerModels.variable_generation(pm, bounded=false)
    println("post variable time: $(time() - start_time)")

    start_time = time()
    PowerModels.constraint_voltage(pm)

    for (i,bus) in ref(pm, :bus)
        if length(ref(pm, :bus_gens, i)) > 0 && !(i in ids(pm,:ref_buses))
            @assert bus["bus_type"] == 2

            PowerModels.constraint_voltage_magnitude_setpoint(pm, i)
            for j in ref(pm, :bus_gens, i)
                PowerModels.constraint_active_gen_setpoint(pm, j)
            end
        end
    end

    for i in ids(pm, :ref_buses)
    #    PowerModels.constraint_theta_ref(pm, i)
        JuMP.@constraint(pm.model, var(pm, :vi, i) == 0)
    end
    println("misc constraints time: $(time() - start_time)")


    vr = var(pm, :vr)
    vi = var(pm, :vi)

    start_time = time()
    p = Dict{Tuple{Int64,Int64,Int64},GenericQuadExpr{Float64,VariableRef}}()
    q = Dict{Tuple{Int64,Int64,Int64},GenericQuadExpr{Float64,VariableRef}}()
    for (i,branch) in ref(pm, :branch)
        #PowerModels.constraint_ohms_yt_from(pm, i)
        #PowerModels.constraint_ohms_yt_to(pm, i)

        f_bus_id = branch["f_bus"]
        t_bus_id = branch["t_bus"]
        f_idx = (i, f_bus_id, t_bus_id)
        t_idx = (i, t_bus_id, f_bus_id)

        f_bus = ref(pm, :bus, f_bus_id)
        t_bus = ref(pm, :bus, t_bus_id)

        #g, b = PowerModels.calc_branch_y(branch)
        #tr, ti = PowerModels.calc_branch_t(branch)
        g = branch["g"]
        b = branch["b"]
        tr = branch["tr"]
        ti = branch["ti"]

        g_fr = branch["g_fr"]
        b_fr = branch["b_fr"]
        g_to = branch["g_to"]
        b_to = branch["b_to"]
        tm = branch["tap"]

        vr_fr = vr[f_bus_id] #var(pm, :vr, f_bus_id)
        vr_to = vr[t_bus_id] #var(pm, :vr, t_bus_id)
        vi_fr = vi[f_bus_id] #var(pm, :vi, f_bus_id)
        vi_to = vi[t_bus_id] #var(pm, :vi, t_bus_id)

        p[f_idx] =  (g+g_fr)/tm^2*(vr_fr^2 + vi_fr^2) + (-g*tr+b*ti)/tm^2*(vr_fr*vr_to + vi_fr*vi_to) + (-b*tr-g*ti)/tm^2*(vi_fr*vr_to - vr_fr*vi_to)
        q[f_idx] = -(b+b_fr)/tm^2*(vr_fr^2 + vi_fr^2) - (-b*tr-g*ti)/tm^2*(vr_fr*vr_to + vi_fr*vi_to) + (-g*tr+b*ti)/tm^2*(vi_fr*vr_to - vr_fr*vi_to)
        p[t_idx] =  (g+g_to)*(vr_to^2 + vi_to^2)      + (-g*tr-b*ti)/tm^2*(vr_fr*vr_to + vi_fr*vi_to) + (-b*tr+g*ti)/tm^2*(-(vi_fr*vr_to - vr_fr*vi_to))
        q[t_idx] = -(b+b_to)*(vr_to^2 + vi_to^2)      - (-b*tr+g*ti)/tm^2*(vr_fr*vr_to + vi_fr*vi_to) + (-g*tr-b*ti)/tm^2*(-(vi_fr*vr_to - vr_fr*vi_to))
    end
    println("flow expr time: $(time() - start_time)")


    pg = var(pm, :pg)
    qg = var(pm, :qg)

    start_time = time()
    for (i,bus) in ref(pm, :bus)
        #PowerModels.constraint_kcl_shunt(pm, i)

        bus_arcs = ref(pm, :bus_arcs, i)
        bus_arcs_dc = ref(pm, :bus_arcs_dc, i)
        bus_gens = ref(pm, :bus_gens, i)
        bus_loads = ref(pm, :bus_loads, i)
        bus_shunts = ref(pm, :bus_shunts, i)

        bus_pd = Dict(k => ref(pm, :load, k, "pd") for k in bus_loads)
        bus_qd = Dict(k => ref(pm, :load, k, "qd") for k in bus_loads)

        bus_gs = Dict(k => ref(pm, :shunt, k, "gs") for k in bus_shunts)
        bus_bs = Dict(k => ref(pm, :shunt, k, "bs") for k in bus_shunts)

        @constraint(pm.model, sum(p[a] for a in bus_arcs) == sum(pg[g] for g in bus_gens) - sum(pd for pd in values(bus_pd)) - sum(gs for gs in values(bus_gs))*(vr[i]^2 + vi[i]^2))
        @constraint(pm.model, sum(q[a] for a in bus_arcs) == sum(qg[g] for g in bus_gens) - sum(qd for qd in values(bus_qd)) + sum(bs for bs in values(bus_bs))*(vr[i]^2 + vi[i]^2))
    end
    println("power balance constraint time: $(time() - start_time)")

end


PowerModels.silence()

ipopt_solver = with_optimizer(Ipopt.Optimizer, tol=1e-6, print_level=0)


data = [
    ("pglib_opf_case10000_tamu.m", "pglib_opf_case10000_tamu_setpoint.json"),
    ("pglib_opf_case9241_pegase.m", "pglib_opf_case9241_pegase_setpoint.json")
]

for (network, setpoint) in data
    println("")
    println("working on: $(network)")
    data = PowerModels.parse_file(network)
    solution = JSON.parsefile(setpoint)
    PowerModels.update_data!(data, solution)

    for (i,branch) in data["branch"]
        g, b = PowerModels.calc_branch_y(branch)
        tr, ti = PowerModels.calc_branch_t(branch)
        branch["g"] = g
        branch["b"] = b
        branch["tr"] = tr
        branch["ti"] = ti
    end

    for (i,bus) in data["bus"]
        bus["vm_start"] = bus["vm"]
        bus["va_start"] = bus["va"]
        bus["vr_start"] = bus["vm"]*cos(bus["va"])
        bus["vi_start"] = bus["vm"]*sin(bus["va"])
    end

    for (i,gen) in data["gen"]
        gen["pg_start"] = gen["pg"]
        gen["qg_start"] = gen["qg"]
    end

    result = run_pf(data, ipopt_solver)
    println("JuMP Solve Time: $(result["solve_time"])")
end


