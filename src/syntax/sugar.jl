import Base: ==

# Basic julia sugar

function desugar(ex)
  MacroTools.prewalk(ex) do ex
    @capture(ex, (xs__,)) ? :(tuple($(xs...))) :
    @capture(ex, xs_[i__]) ? :(getindex($xs, $(i...))) :
    ex
  end
end

# Constants

immutable Constant{T}
  value::T
end

tocall(c::Constant) = c.value

isconstant(v::Vertex) = isa(value(v), Constant)

mapconst(f, g) = map(x -> isa(x, Constant) ? Constant(f(x.value)) : f(x), g)

a::Constant == b::Constant = a.value == b.value

Base.hash(c::Constant, h::UInt = UInt(0)) = hash((Constant, c.value), h)

for (c, v) in [(:constant, :vertex), (:dconstant, :dvertex)]
  @eval $c(x) = $v(Constant(x))
  @eval $c(v::Vertex) = $v(v)
end

type Do end

tocall(::Do, a...) = :($(a...);)

# Static tuples

# TODO: just use `getindex` and `tuple` to represent these?
immutable Split
  n::Int
end

# TODO: printing
function normsplits(ex)
  MacroTools.prewalk(ex) do ex
    @capture(ex, (xs__,) = y_) || return ex
    @gensym edge
    quote
      $edge = $y
      $((:($(xs[i]) = $(Split(i))($edge)) for i = 1:length(xs))...)
    end
  end |> MacroTools.flatten |> block
end

tocall(::typeof(tuple), args...) = :($(args...),)

tocall(s::Split, x) = :($x[$(s.n)])

group(xs...) = vertex(tuple, xs...)

function detuple(v::IVertex)
  postwalk(v) do v
    if isa(value(v), Split) && value(v[1]) == tuple
      v[1][value(v).n]
    else
      v
    end
  end
end

# Bindings

immutable Bind
  name::Symbol
end

# TODO: printing
function insertbinds(ex)
  ls = map(ex.args) do l
    @capture(l, x_ = y_) || return l
    :($x = $(Bind(x))($y))
  end
  :($(ls...);)
end

# Inputs

immutable Input end

splitnode(v, n) = vertex(Split(n), v)

inputnode(n) = splitnode(constant(Input()), n)

isinput(v::IVertex) = isa(value(v), Split) && value(v[1]) == Constant(Input())

function bumpinputs(v::IVertex)
  prewalk(v) do v
    isinput(v) ?
      inputnode(value(v).n + 1) :
      v
  end
end

function spliceinput(v::IVertex, input::IVertex)
  postwalk(v) do v
    value(v) == Constant(Input()) ? input : v
  end
end

spliceinputs(v::IVertex, inputs::Vertex...) =
  spliceinput(v, group(inputs...))

function graphinputs(v::IVertex)
  n = 0
  prewalk(v) do v
    isinput(v) && (n = max(n, value(v).n))
    v
  end
  return n
end

# Closures

immutable Flosure end
immutable LooseEnd end

# TODO: scope
function normclosures(ex)
  bs = bindings(ex)
  MacroTools.prewalk(shortdef(ex)) do ex
    @capture(ex, (args__,) -> body_) || return ex
    @assert all(arg -> isa(arg, Symbol), args)
    closed = filter(x -> inexpr(body, x), bs)
    vars = vcat(closed, args)
    body = MacroTools.prewalk(body) do ex
      ex in vars ?
        Expr(:call, Split(findfirst(x->x==ex, vars)), LooseEnd()) :
        ex
    end
    :($(Flosure())($body, $(closed...)))
  end |> MacroTools.flatten |> block
end

flopen(v::IVertex) = mapconst(x->x==LooseEnd()?Input():x,v)
