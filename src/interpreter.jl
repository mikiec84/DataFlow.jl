# TODO: stack and error messages

type Context{T}
  interp::T
  cache::ObjectIdDict
  stack::Vector{Line}
  data::Dict{Symbol,Any}
end

Context(interp; kws...) = Context(interp, ObjectIdDict(), Line[], Dict{Symbol,Any}(kws))

Base.getindex(ctx::Context, k::Symbol) = ctx.data[k]
Base.setindex!(ctx::Context, v, k::Symbol) = ctx.data[k] = v

stack(c::Context) = copy(c.stack)

function interpret(ctx::Context, graph::IVertex, args::IVertex...)
  graph = spliceinputs(graph, args...)
  interpret(ctx, graph)
end

interpret(ctx::Context, graph::IVertex, args...) =
  interpret(ctx, graph, map(constant, args)...)

function interpret(ctx::Context, graph::IVertex)
  haskey(ctx.cache, graph) && return ctx.cache[graph]
  ctx.cache[graph] = ctx.interp(ctx, value(graph), inputs(graph)...)
end

interpret(ctx::Context, xs::Tuple) = map(x -> interpret(ctx, x), xs)

# Composable interpreter pieces

function interpline(f)
  function interp(ctx::Context, l::Line, v)
    push!(ctx.stack, l)
    val = interpret(ctx, v)
    pop!(ctx.stack)
    return val
  end
  interp(args...) = f(args...)
end

function interpconst(f)
  interp(ctx::Context, x::Constant) = x.value
  interp(args...) = f(args...)
end

function interptuple(f)
  function interp(ctx::Context, s::Split, xs)
    xs = interpret(ctx, xs)
    isa(xs, Vertex) && value(xs) == tuple ? inputs(xs)[s.n] :
    isa(xs, Tuple) ? xs[s.n] :
    f(ctx, s, constant(xs))
  end
  interp(args...) = f(args...)
end

function interplambda(f)
  interp(ctx::Context, f::Flosure, body, vars...) =
    (xs...) -> interpret(ctx, flopen(body), vars..., xs...)
  interp(args...) = f(args...)
end

const interpid =
  interpconst((ctx, f, xs...) -> vertex(f, map(constant, interpret(ctx, xs))...))

const interpeval =
  interpline(interplambda(interpconst(interptuple((ctx, f, xs...) -> f(interpret(ctx, xs)...)))))

interpret(graph::IVertex, args...) =
  interpret(Context(interpeval), graph, args...)
