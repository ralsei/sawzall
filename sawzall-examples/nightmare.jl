using DataFrames
using CSV
using Pipe
using Statistics

flights = DataFrame(CSV.File("data/flights.csv", missingstring="NA"))

function f(df)
    @pipe df |>
        groupby(_, :dest) |>
        combine(_,
                nrow => :count,
                :distance => x -> mean(skipmissing(x)),
                :arr_delay => x -> mean(skipmissing(x))) |>
        filter(:count => x -> x > 20, _) |>
        filter(:dest => !=("HNL"), _)
end
