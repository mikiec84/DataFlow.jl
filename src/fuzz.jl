module Fuzz

using ..DataFlow
import DataFlow: thread!

export grow

grow{T<:Vertex}(::Type{T}, value) = T(value())

function grow{T<:Vertex}(::Type{T}, nodes::Integer, edges::Integer = nodes)
  vs = [grow(T, ()->i) for i = 1:nodes]
  for _ = 1:edges
    thread!(rand(vs), rand(vs))
  end
  return rand(vs)
end

# using Atom
# function testcase(test, tries, cap = 10)
#   @progress for nodes = 1:cap
#     for _ = 1:tries
#       g = grow(DVertex, nodes)
#       test(g) || return g
#     end
#   end
# end

end
