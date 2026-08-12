using Test

for dir in readdir(@__DIR__)
    if isdir(joinpath(@__DIR__, dir))
        @testset "$dir" begin
            include(joinpath(@__DIR__, dir, "runtests.jl"))
        end
    end
end
