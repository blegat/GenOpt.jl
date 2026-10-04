using Test

"""
    _include_sandbox(filename)

Include the `filename` in a temporary module that acts as a sandbox. (Ensuring
no constants or functions leak into other files.)

This function was taken from `JuMP/docs/make.jl`.
"""
function _include_sandbox(filename)
    mod = @eval module $(gensym()) end
    return Base.include(mod, filename)
end

for dir in readdir(@__DIR__)
    if isdir(joinpath(@__DIR__, dir))
        @testset "$dir" begin
            _include_sandbox(joinpath(@__DIR__, dir, "runtests.jl"))
        end
    end
end
