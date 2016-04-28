# Construction

type DVertex{T} <: AVertex{T}
  value::T
  inputs::Vector{Needle{DVertex{T}}}
  outputs::Set{DVertex{T}}

  DVertex(x) = new(x, [], Set{DVertex{T}}())
end

DVertex(x) = DVertex{typeof(x)}(x)

value(v::DVertex) = v.value
inputs(v::DVertex) = v.inputs
outputs(v::DVertex) = v.outputs

function thread!(to::DVertex, from::Needle)
  push!(inputs(to), from)
  push!(outputs(from.vertex), to)
  return to
end

vertex(a...) = DVertex{Any}(a...)

dl(v::AVertex) = convert(DVertex, v)
