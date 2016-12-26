mux(f) = f
mux(m, f) = (xs...) -> m(f, xs...)
mux(ms...) = foldr(mux, ms)

type Context{T}
  interp::T
  cache::ObjectIdDict
  stack::Vector{Any}
  data::Dict{Symbol,Any}
end

Context(interp; kws...) = Context(interp, ObjectIdDict(), [], Dict{Symbol,Any}(kws))

Base.getindex(ctx::Context, k::Symbol) = ctx.data[k]
Base.setindex!(ctx::Context, v, k::Symbol) = ctx.data[k] = v

function stack(c::Context)
  stk = []
  isempty(c.stack) && return stk
  for i = 2:length(c.stack)-1
    isa(c.stack[i], Frame) && isa(c.stack[i-1], Line) && push!(stk, c.stack[i-1])
  end
  isa(c.stack[end], Line) && push!(stk, c.stack[end])
  return stk
end

function interpv(ctx::Context, graph::IVertex)
  haskey(ctx.cache, graph) && return ctx.cache[graph]
  ctx.cache[graph] = ctx.interp(ctx, value(graph), inputs(graph)...)
end

interpv(ctx::Context, xs::Tuple) = map(x -> interpv(ctx, x), xs)

function interpret(ctx::Context, graph::IVertex, args::IVertex...)
  graph = spliceinputs(graph, args...)
  interpv(ctx, graph)
end

interpret(ctx::Context, graph::IVertex, args...) =
  interpret(ctx, graph, map(constant, args)...)

# The `ifoo` convention denotes a piece of interpreter middleware

iconst(f, ctx::Context, x::Constant) = x.value

function iline(f, ctx::Context, l::Union{Line,Frame}, v)
  push!(ctx.stack, l)
  val = interpv(ctx, v)
  pop!(ctx.stack)
  return val
end

function ituple(f, ctx::Context, s::Split, xs)
  xs = interpv(ctx, xs)
  isa(xs, Vertex) && value(xs) == tuple ? inputs(xs)[s.n] :
  isa(xs, Tuple) ? xs[s.n] :
    f(ctx, s, constant(xs))
end

ilambda(f, ctx::Context, ::Flosure, body, vars...) =
  (xs...) -> interpret(ctx, flopen(body), vars..., xs...)

iargs(cb, ctx::Context, f, xs...) = cb(f, interpv(ctx, xs)...)

for m in :[iconst, iline, ituple, ilambda].args
  @eval $m(f, ctx::Context, args...) = f(ctx, args...)
end

interpeval = mux(iline, ilambda, iconst, ituple, iargs, (f, xs...) -> f(xs...))

interpret(graph::IVertex, args...) =
  interpret(Context(interpeval), graph, args...)
