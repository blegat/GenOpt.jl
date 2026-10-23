# Copyright (c) 2024: Benoît Legat and contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

using Test

include(joinpath(@__DIR__, "MyApp", "src", "MyApp.jl"))

@testset "JuliaC bridge workload" begin
    @test MyApp.main(String[]) == 0
end
