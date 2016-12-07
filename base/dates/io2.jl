importall Base.Dates


"""
A type of token in a date time string

each subtype must define

    tryparsenext(t::TokenType, str, i, [locale])

and

    format(io, t::TokenType, [locale])
"""
abstract AbstractDateToken

"""
Locale type for dates. By default `DateLocale{:english}`
is used. This object is passed as the last argument to
`tryparsenext` and `format` defined for each `AbstractDateToken` type.
"""
immutable DateLocale{lang} end

"""
Information for parsing and formatting date time values.
"""
immutable DateFormat{D, T<:Tuple, L<:DateLocale, N}
    result_type::Type{D}
    tokens::T
    locale::L
    field_defaults::NTuple{N, Int}
    field_order::NTuple{N,Int}
end


### Token types

immutable DatePart{c, n, fixedwidth} <: AbstractDateToken end

for c in "yYmdHMSs"
    @eval begin
        # in the case where the token is not to be treated fixed width
        @inline function tryparsenext{N}(::DatePart{$c, N, false}, str, i)
            tryparsenext_base10(str,i,20)
        end

        # parse upto a maximum of N numeric characters
        # (will be chosen in case of yyyymmdd for example)
        @inline function tryparsenext{N}(::DatePart{$c, N, true}, str, i)
            tryparsenext_base10(str,i,N)
        end
    end
end

# fallback to tryparsenext methods that don't care about locale
@inline function tryparsenext(d::AbstractDateToken, str, i, locale)
    tryparsenext(d, str, i)
end

function month_from_abbr_name(word, locale::DateLocale{:english})
    get(Dates.abbrenglish, word, 0)
end

function month_from_name(word, locale::DateLocale{:english})
    get(Dates.english, word, 0)
end

for (tok, fn) in zip("uU", [month_from_abbr_name, month_from_name]),
    (fixed, nchars) in zip([false, true], [typemax(Int), :N])
    @eval @inline function tryparsenext{N}(d::DatePart{$tok,N,$fixed}, str, i, locale)
        R = Nullable{Int}
        c, ii = tryparsenext_word(str, i, locale, $nchars)
        @show word = str[i:ii-1]
        @show x = $fn(lowercase(word), locale)
        ((x == 0 ? R() : R(x)), ii)
    end
end

@inline function tryparsenext_word(str, i, locale::DateLocale{:english}, maxchars=typemax(Int))
    @show maxchars
    R = Nullable{Int}
    for j=1:maxchars
        if done(str, i)
            break
        end
        @show c, ii = next(str, i)
        if !((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z'))
            break
        end
        i=ii
    end
    return R(0), i
end

for (c, fn) in zip("yYmdHMSs", [year, year, month, day, hour, minute, second, millisecond])
    @eval function format{N}(io, ::DatePart{$c, N}, dt)
        minwidth($fn(dt), N)
    end
end

function minwidth(num, n)
    s = string(num)
    length(s) < n ?  lpad(s, n, 0) : s
end

### Delim ###

immutable Delim{T, length} <: AbstractDateToken d::T end

Delim(c::Char, n) = Delim{Char, n}(c)
Delim(c::Char) = Delim(c,1)

@inline function tryparsenext{N}(d::Delim{Char,N}, str, i::Int)
    R = Nullable{Int}
    for j=1:N
        c, i = next(str, i)
        if c != d.d
            return R(), i
        end
    end
    return R(0), i
end

@inline function tryparsenext{N}(d::Delim{String,N}, str, i::Int)
    i1=i
    i2=start(d.d)
    for j=1:N
        c1, i1 = next(str, i1)
        c2, i2 = next(str, i2)
        if c1 != c2
            return R(), i1
        end
    end
    return R(0), i1
end
const SLOT_RULE = Dict{Char, Type}(
    'y' => Year,
    'Y' => Year,
    'm' => Month,
    'u' => Month,
    'U' => Month,
    'e' => Dates.DayOfWeekSlot,
    'E' => Dates.DayOfWeekSlot,
    'd' => Day,
    'H' => Hour,
    'M' => Minute,
    'S' => Second,
    's' => Millisecond,
)

const date_format_letters = join(collect(keys(SLOT_RULE)), "")

_default_fields(::Type{DateTime}) = (1,1,1,0,0,0,0)

function DateFormat{T}(f::AbstractString, ::Type{T}=DateTime; default_fields=_default_fields(T), locale::Symbol=:english)
    tokens = AbstractDateToken[]
    localeobj = DateLocale{Symbol(locale)}()
    prefix = ""
    dateorder = [Year, Month, Day, Hour, Minute, Second, Millisecond]
    order = zeros(Int, length(default_fields))
    last_offset = 1

    for m in eachmatch(Regex("(?<!\\\\)([\\Q$date_format_letters\\E])\\1*"), f)
        letter = f[m.offset]
        typ = SLOT_RULE[letter]

        width = length(m.match)
        delim = replace(f[last_offset:m.offset-1], r"\\(.)", s"\1")

        push_delim_token!(tokens, delim)
        push!(tokens, DatePart{letter, width, true}())
        idx = findfirst(dateorder, typ)
        if idx != 0
            order[idx] = length(tokens)
        end

        last_offset = m.offset+width
    end

    remaining = f[last_offset:end]
    push_delim_token!(tokens, remaining)
    if length(tokens) > 0
        tokens[end] = make_variable_width(tokens[end])
    end

    return DateFormat(T, (tokens...), localeobj, default_fields, (order...))
end

function shuffle_args{Nv, Ni}(val::NTuple{Nv}, idx::NTuple{Ni}, default::NTuple{Ni})
    ntuple(Val{Ni}) do i
        if idx[i] == 0
            default[i]
        else
            val[idx[i]]
        end
    end
end


macro chk1(expr,label=:error)
    quote
        x = $(esc(expr))
        if isnull(x[1])
            @goto $label
        else
            get(x[1]),x[2]
        end
    end
end
@generated function Base.tryparse{N}(fmt::DateFormat{DateTime, NTuple{N}}, str::AbstractString)
    quote
        R = Nullable{DateTime}
        t = fmt.tokens
        l = fmt.locale

        state = start(str)
        Base.@nexprs $N i->((val_i, state) = @chk1 tryparsenext(t[i], str, state, l))
        parts = Base.@ntuple $N val

        @label done
        return R(DateTime((shuffle_args(parts, fmt.field_order, fmt.field_defaults)::NTuple{7,Int})))

        @label error
        return R()
    end
end

function DateTime(xs::NTuple{7,Int} #y::Int64,m::Int64=1,d::Int64=1,
                  #h::Int64=0,mi::Int64=0,s::Int64=0,ms::Int64=0
                  )
    0 < xs[2] < 13 || throw(ArgumentError("Month: $(xs[2]) out of range (1:12)"))
    0 < xs[3] < daysinmonth(xs[1],xs[2])+1 || throw(ArgumentError("Day: $(xs[3]) out of range (1:$(daysinmonth(y,m)))"))
    -1 < xs[4] < 24 || throw(ArgumentError("Hour: $(xs[4]) out of range (0:23)"))
    -1 < xs[5] < 60 || throw(ArgumentError("Minute: $(xs[5]) out of range (0:59)"))
    -1 < xs[6] < 60 || throw(ArgumentError("Second: $(xs[6]) out of range (0:59)"))
    -1 < xs[7] < 1000 || throw(ArgumentError("Millisecond: $(xs[7]) out of range (0:999)"))
    rata = xs[7] + 1000*(xs[6] + 60*xs[5] + 3600*xs[4] + 86400*Base.Dates.totaldays(xs[1],xs[2],xs[3]))
    return DateTime(Base.Dates.UTM(rata))
end



@inline function tryparsenext{N, P<:Period}(d::DatePart{P, N}, str, i)
    tryparsenext_base10(str,i,10)
end

function Base.tryparse{T<:Union{Date,DateTime}}(::Type{T}, str::AbstractString)
    i = start(str)
    i = skipwhitespace(str,i)
    nd, i = tryparsenext(T, str, i)
    i = skipwhitespace(str,i)
    if !done(str,i)
        return Nullable{T}()
    else
        return nd
    end
end

@inline function skipwhitespace(str,i)
    while !done(str,i)
        c,ii = next(str,i)
        if !isspace(c)
            break
        end
        i = ii
    end
    return i
end

@inline function tryparsenext(::Type{Date},str,i)
    R = Nullable{Date}
    dm = dd = 1
    dy, i = @chk1 tryparsenext_base10(str,i,10)
    c,  i = @chk1 tryparsenext_char(str,i,'-')
    dm, i = @chk1 tryparsenext_base10(str,i,2) done
    c,  i = @chk1 tryparsenext_char(str,i,'-') done
    dd, i = @chk1 tryparsenext_base10(str,i,2) done

    @label done
    d = Date(dy,dm,dd)
    return R(d), i

    @label error
    return R(), i
end


@inline function tryparsenext(::Type{DateTime},str,i)
    R = Nullable{DateTime}
    dm = dd = 1
    th = tm = ts = tms = 0
    dy, i = @chk1 tryparsenext_base10(str,i,10)
    c,  i = @chk1 tryparsenext_char(str,i,'-')
    dm, i = @chk1 tryparsenext_base10(str,i,2) done
    c,  i = @chk1 tryparsenext_char(str,i,'-') done
    dd, i = @chk1 tryparsenext_base10(str,i,2) done
    c,  i = @chk1 tryparsenext_char(str,i,'T') done
    th, i = @chk1 tryparsenext_base10(str,i,2) done
    c,  i = @chk1 tryparsenext_char(str,i,':') done
    tm, i = @chk1 tryparsenext_base10(str,i,2) done
    c,  i = @chk1 tryparsenext_char(str,i,':') done
    ts, i = @chk1 tryparsenext_base10(str,i,2) done
    c,  i = @chk1 tryparsenext_char(str,i,'.') done
    tms,i = @chk1 tryparsenext_base10_frac(str,i,3) done

    @label done
    d = DateTime(dy,dm,dd,th,tm,ts,tms)
    return R(d), i

    @label error
    return R(), i
end

@inline function tryparsenext_base10_digit(str,i)
    R = Nullable{Int}
    done(str,i) && @goto error
    c,ii = next(str,i)
    '0' <= c <= '9' || @goto error
    return R(c-'0'), ii

    @label error
    return R(), i
end

@inline function tryparsenext_base10(str,i,maxdig)
    R = Nullable{Int}
    r,i = @chk1 tryparsenext_base10_digit(str,i)
    for j = 2:maxdig
        d,i = @chk1 tryparsenext_base10_digit(str,i) done
        r = r*10 + d
    end
    @label done
    return R(r), i

    @label error
    return R(), i
end

@inline function tryparsenext_base10_frac(str,i,maxdig)
    R = Nullable{Int}
    r,i = @chk1 tryparsenext_base10_digit(str,i)
    for j = 2:maxdig
        nd,i = tryparsenext_base10_digit(str,i)
        if isnull(nd)
            for k = j:maxdig
                r *= 10
            end
            break
        end
        d = get(nd)
        r = 10*r + d
    end
    return R(r), i

    @label error
    return R(), i
end


@inline function tryparsenext_char(str,i,cc::Char)::Tuple{Nullable{Char},Int}
    R = Nullable{Char}
    done(str,i) && @goto error
    c,ii = next(str,i)
    c == cc || @goto error
    return R(c), ii

    @label error
    return R(), i
end


using BenchmarkTools
function foo()
    fmt = DateFormat("yyyy-mm-ddTHH:MM:SS.sss")
    @show @benchmark tryparse(DateTime, "2016-10-18T13:56:28.788")
    @show @benchmark dateparse("2016-10-18T13:56:28.788", $fmt)
end

make_variable_width{C,N}(p::DatePart{C, N, true}) = DatePart{C, N, false}()
make_variable_width(p) = p


function push_delim_token!(tokens, delim)
    if !isempty(delim)
        if length(tokens) > 0
            tokens[end] = make_variable_width(tokens[end])
        end
        if length(delim) == 1
            push!(tokens, Delim(first(delim)))
        else
            push!(tokens, Delim{typeof(delim), length(delim)}(delim))
        end
    end
end
