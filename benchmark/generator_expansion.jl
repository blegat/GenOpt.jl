# Copyright (c) 2024: Benoît Legat and contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module GeneratorExpansionBenchmark

import GenOpt
import MathOptInterface as MOI

snf(head, args...) = MOI.ScalarNonlinearFunction(head, Any[args...])

function coordinate_family(n)
    cells = (n + 1)^2
    count = cells * n
    y = GenOpt.ContiguousArrayOfVariables(0, (n,))
    r = GenOpt.ContiguousArrayOfVariables(n, (count,))
    i, f = GenOpt.IteratorIndex(1), GenOpt.IteratorIndex(2)
    position = snf(:+, i, snf(:*, cells, f), 1)
    coordinates = [(q % (n + 1)) / n for q in 0:(cells-1)]
    expression = snf(
        :+,
        snf(
            :-,
            snf(:getindex, r, position),
            snf(:getindex, coordinates, snf(:+, i, 1)),
        ),
        snf(:getindex, y, snf(:+, f, 1)),
    )
    iterators = GenOpt.Iterator[
        GenOpt.Iterator(collect(0:(cells-1))),
        GenOpt.Iterator(collect(0:(n-1))),
    ]
    generator = GenOpt.FunctionGenerator{MOI.ScalarAffineFunction{Float64}}(
        expression,
        iterators,
    )
    return n + count, generator
end

function pde_family(n)
    side = n + 1
    y = GenOpt.ContiguousArrayOfVariables(0, (side, side))
    i, j = GenOpt.IteratorIndex(1), GenOpt.IteratorIndex(2)
    current = snf(:getindex, y, i, j)
    next = snf(:getindex, y, snf(:+, i, 1), j)
    stencil = snf(
        :+,
        snf(:getindex, y, i, snf(:-, j, 1)),
        snf(:*, -2.0, current),
        snf(:getindex, y, i, snf(:+, j, 1)),
        snf(:getindex, y, snf(:+, i, 1), snf(:-, j, 1)),
        snf(:*, -2.0, next),
        snf(:getindex, y, snf(:+, i, 1), snf(:+, j, 1)),
    )
    expression = snf(
        :-,
        snf(:*, (1 / n)^2, snf(:-, next, current)),
        snf(:*, 0.5 * 1.58 / n, stencil),
    )
    iterators = GenOpt.Iterator[
        GenOpt.Iterator(collect(1:n)),
        GenOpt.Iterator(collect(2:n)),
    ]
    generator = GenOpt.FunctionGenerator{MOI.ScalarAffineFunction{Float64}}(
        expression,
        iterators,
    )
    return side^2, generator
end

function build(kind::Symbol, n::Int, batch::Bool)
    variables, generator =
        kind == :coordinate ? coordinate_family(n) : pde_family(n)
    model = MOI.Utilities.Model{Float64}()
    MOI.add_variables(model, variables)
    if batch
        indices =
            GenOpt._try_batch_constraints(model, generator, MOI.EqualTo(0.0))
        @assert indices !== nothing
    else
        GenOpt._add_constraints_scalar(model, generator, MOI.EqualTo(0.0))
    end
    return model
end

function measure(kind, n, batch)
    GC.gc(true)
    stats = @timed build(kind, n, batch)
    rows = MOI.get(
        stats.value,
        MOI.NumberOfConstraints{
            MOI.ScalarAffineFunction{Float64},
            MOI.EqualTo{Float64},
        }(),
    )
    return (;
        seconds = stats.time,
        gc_seconds = stats.gctime,
        bytes = stats.bytes,
        rows,
    )
end

function main(args)
    repeats = isempty(args) ? 3 : parse(Int, args[1])
    println("family,size,mode,sample,rows,seconds,gc_seconds,allocated_bytes")
    for (kind, n) in ((:coordinate, 100), (:pde, 2000))
        for batch in (false, true)
            build(kind, 5, batch)
        end
        for sample in 1:repeats, batch in (false, true)
            result = measure(kind, n, batch)
            mode = batch ? "batch" : "scalar"
            println(
                join(
                    (
                        kind,
                        n,
                        mode,
                        sample,
                        result.rows,
                        result.seconds,
                        result.gc_seconds,
                        result.bytes,
                    ),
                    ',',
                ),
            )
            flush(stdout)
        end
    end
    return
end

if abspath(PROGRAM_FILE) == @__FILE__
    main(ARGS)
end

end
