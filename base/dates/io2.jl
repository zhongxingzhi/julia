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

# mapping symbols to types

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


### Token types ###

immutable DatePart{c, n, fixedwidth} <: AbstractDateToken end


### Numeric tokens

for c in "yYmdHMS"
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

@inline function tryparsenext{N}(::DatePart{'s', N, false}, str, i)
    tryparsenext_base10_frac(str,i,3)
end

@inline function tryparsenext{N}(::DatePart{'s', N, true}, str, i)
    tryparsenext_base10_frac(str,i,N)
end

for (c, fn) in zip("YmdHMSs", [year, month, day, hour, minute, second, millisecond])
    @eval function format{N}(io, ::DatePart{$c, N}, dt, locale)
        write(io, minwidth($fn(dt), N))
    end
end

# special cases

function format{N}(io, ::DatePart{'y', N}, dt, locale)
    write(io, rfixwidth(year(dt), N))
end

function format{N}(io, ::DatePart{'s', N}, dt, locale)
    write(io, lfixwidth(millisecond(dt), N))
end

### Text tokens

# fallback to tryparsenext methods that don't care about locale
@inline function tryparsenext(d::AbstractDateToken, str, i, locale)
    tryparsenext(d, str, i)
end

function month_from_abbr_name{l}(word, locale::DateLocale{l})
    @show l, word
    get(Dates.MONTHTOVALUEABBR[string(l)], word, 0)
end

function month_from_name{l}(word, locale::DateLocale{l})
    @show l, word
    get(Dates.MONTHTOVALUE[string(l)], word, 0)
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
        word = str[i:ii-1]
        x = $fn(lowercase(word), locale)
        ((x == 0 ? R() : R(x)), ii)
    end
end

# ignore day of week while parsing
@inline function tryparsenext(d::Union{DatePart{'e'}, DatePart{'E'}}, str, i, locale)
    tryparsenext_word(str, i, locale)
end

for (tok, fn) in zip("uU", [monthabbr, monthname])
    @eval function format{l}(io, d::DatePart{$tok}, dt, locale::DateLocale{l})
        write(io, $fn(dt; locale=string(l)))
    end
end

for (tok, dict) in zip("eE", [:VALUETODAYOFWEEKABBR, :VALUETODAYOFWEEK])
    @eval function format{l}(io, d::DatePart{$tok}, dt, locale::DateLocale{l})
        write(io, $dict[string(locale)][dayofweek(dt)])
    end
end

# fast version for English
@inline function tryparsenext_word(str, i, locale::DateLocale{:english}, maxchars=typemax(Int))
    for j=1:maxchars
        done(str, i) && break
        c, ii = next(str, i)
        !((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')) && break
        i=ii
    end
    return Nullable{Int}(0), i
end

@inline function tryparsenext_word(str, i, locale, maxchars=typemax(Int))
    for j=1:maxchars
        done(str, i) && break
        c, ii = next(str, i)
        !isalpha(c) && break
        i=ii
    end
    return Nullable{Int}(0), i
end

function minwidth(num, n)
    s = string(num)
    length(s) < n ?  lpad(s, n, 0) : s
end

function rfixwidth(num, n)
    s = string(num)
    length(s) > n ? s[end-(n-1):end] : lpad(s, n, 0)
end

function lfixwidth(num, n)
    s = string(num)
    length(s) > n ? s[1:n] : rpad(s, n, 0)
end


### delimiters

immutable Delim{T, length} <: AbstractDateToken d::T end

Delim(c::Char, n) = Delim{Char, n}(c)
Delim(c::Char) = Delim(c,1)

@inline function tryparsenext{N}(d::Delim{Char,N}, str, i::Int)
    R = Nullable{Int}
    for j=1:N
        done(str, i) && return (R(), i)
        c, i = next(str, i)
        c != d.d && return (R(), i)
    end
    return R(0), i
end

@inline function tryparsenext{N}(d::Delim{String,N}, str, i::Int)
    R = Nullable{Int}
    i1=i
    i2=start(d.d)
    for j=1:N
        if done(str, i1)
            return R(), i1
        end
        c1, i1 = next(str, i1)
        c2, i2 = next(d.d, i2)
        if c1 != c2
            return R(), i1
        end
    end
    return R(0), i1
end

function format(io, d::Delim, str, i)
    write(io, d.d)
end

_default_fields(::Type{DateTime}) = (1,1,1,0,0,0,0)

function DateFormat{T}(f::AbstractString, locale=:english, ::Type{T}=DateTime; default_fields=_default_fields(T))
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
        Base.@nexprs $N i->((val_i, state) = (@chk1 tryparsenext(t[i], str, state, l)))
        parts = Base.@ntuple $N val

        @label done
        return R(DateTime((reorder_args(parts, fmt.field_order, fmt.field_defaults)::NTuple{7,Int})))

        @label error
        return R()
    end
end

function DateTime(xs::NTuple{7,Int})
    0 < xs[2] < 13 || throw(ArgumentError("Month: $(xs[2]) out of range (1:12)"))
    0 < xs[3] < daysinmonth(xs[1],xs[2])+1 || throw(ArgumentError("Day: $(xs[3]) out of range (1:$(daysinmonth(xs[1],xs[2])))"))
    -1 < xs[4] < 24 || throw(ArgumentError("Hour: $(xs[4]) out of range (0:23)"))
    -1 < xs[5] < 60 || throw(ArgumentError("Minute: $(xs[5]) out of range (0:59)"))
    -1 < xs[6] < 60 || throw(ArgumentError("Second: $(xs[6]) out of range (0:59)"))
    -1 < xs[7] < 1000 || throw(ArgumentError("Millisecond: $(xs[7]) out of range (0:999)"))
    rata = xs[7] + 1000*(xs[6] + 60*xs[5] + 3600*xs[4] + 86400*Base.Dates.totaldays(xs[1],xs[2],xs[3]))
    return DateTime(Base.Dates.UTM(rata))
end

### Parsing utilities

function reorder_args{Nv, Ni}(val::NTuple{Nv}, idx::NTuple{Ni}, default::NTuple{Ni})
    ntuple(Val{Ni}) do i
        if idx[i] == 0
            default[i]
        else
            val[idx[i]]
        end
    end
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

### Utility functions for DateFormat construction

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

### Common formats
const ISODateTimeFormat = DateFormat("yyyy-mm-dd\\THH:MM:SS.s")
const ISODateFormat = DateFormat("yyyy-mm-dd")
const RFC1123Format = DateFormat("e, dd u yyyy HH:MM:SS")

"""
    DateTime(dt::AbstractString, format::AbstractString; locale="english") -> DateTime

Construct a `DateTime` by parsing the `dt` date string following the pattern given in
the `format` string. The following character codes can be used to construct the `format`
string:

| Code       | Matches   | Comment                                                      |
|:-----------|:----------|:-------------------------------------------------------------|
| `y`        | 1996, 96  | Returns year of 1996, 0096                                   |
| `Y`        | 1996, 96  | Returns year of 1996, 0096. Equivalent to `y`                |
| `m`        | 1, 01     | Matches 1 or 2-digit months                                  |
| `u`        | Jan       | Matches abbreviated months according to the `locale` keyword |
| `U`        | January   | Matches full month names according to the `locale` keyword   |
| `d`        | 1, 01     | Matches 1 or 2-digit days                                    |
| `H`        | 00        | Matches hours                                                |
| `M`        | 00        | Matches minutes                                              |
| `S`        | 00        | Matches seconds                                              |
| `s`        | .500      | Matches milliseconds                                         |
| `e`        | Mon, Tues | Matches abbreviated days of the week                         |
| `E`        | Monday    | Matches full name days of the week                           |
| `yyyymmdd` | 19960101  | Matches fixed-width year, month, and day                     |

Characters not listed above are normally treated as delimiters between date and time slots.
For example a `dt` string of "1996-01-15T00:00:00.0" would have a `format` string like
"y-m-dTH:M:S.s". If you need to use a code character as a delimiter you can escape it using
backslash. The date "1995y01m" would have the format "y\\ym\\m".
"""
DateTime(dt::AbstractString, format::AbstractString;locale::AbstractString="english") = DateTime(dt,DateFormat(format,locale))

function tryfailparse(dt, df)
    maybedt = tryparse(df, dt)
    if isnull(maybedt)
        throw(ArgumentError("Invalid date string for given format"))
    else
        get(maybedt)
    end
end

"""
    DateTime(dt::AbstractString, df::DateFormat) -> DateTime

Construct a `DateTime` by parsing the `dt` date string following the pattern given in
the [`DateFormat`](:func:`Dates.DateFormat`) object. Similar to
`DateTime(::AbstractString, ::AbstractString)` but more efficient when repeatedly parsing
similarly formatted date strings with a pre-created `DateFormat` object.
"""
DateTime(dt::AbstractString,df::DateFormat=ISODateTimeFormat) = tryfailparse(dt,df)

"""
    Date(dt::AbstractString, format::AbstractString; locale="english") -> Date

Construct a `Date` object by parsing a `dt` date string following the pattern given in the
`format` string. Follows the same conventions as
`DateTime(::AbstractString, ::AbstractString)`.
"""
Date(dt::AbstractString,format::AbstractString;locale::AbstractString="english") = Date(dt,DateFormat(format,locale))

"""
    Date(dt::AbstractString, df::DateFormat) -> Date

Parse a date from a date string `dt` using a `DateFormat` object `df`.
"""
Date(dt::AbstractString,df::DateFormat=ISODateFormat) = Date(tryfailparse(dt,df))

format(io, t, dt, locale) = format(io, t, dt)

function format(dt::DateTime, fmt::DateFormat)
    io = IOBuffer()
    for t in fmt.tokens
        format(io, t, dt, fmt.locale)
    end
    takebuf_string(io)
end

function format(dt::Date, fmt::DateFormat)
    format(DateTime(dt), fmt)
end

"""
    format(dt::TimeType, format::AbstractString; locale="english") -> AbstractString

Construct a string by using a `TimeType` object and applying the provided `format`. The
following character codes can be used to construct the `format` string:

| Code       | Examples  | Comment                                                      |
|:-----------|:----------|:-------------------------------------------------------------|
| `y`        | 6         | Numeric year with a fixed width                              |
| `Y`        | 1996      | Numeric year with a minimum width                            |
| `m`        | 1, 12     | Numeric month with a minimum width                           |
| `u`        | Jan       | Month name shortened to 3-chars according to the `locale`    |
| `U`        | January   | Full month name according to the `locale` keyword            |
| `d`        | 1, 31     | Day of the month with a minimum width                        |
| `H`        | 0, 23     | Hour (24-hour clock) with a minimum width                    |
| `M`        | 0, 59     | Minute with a minimum width                                  |
| `S`        | 0, 59     | Second with a minimum width                                  |
| `s`        | 000, 500  | Millisecond with a minimum width of 3                        |
| `e`        | Mon, Tue  | Abbreviated days of the week                                 |
| `E`        | Monday    | Full day of week name                                        |

The number of sequential code characters indicate the width of the code. A format of
`yyyy-mm` specifies that the code `y` should have a width of four while `m` a width of two.
Codes that yield numeric digits have an associated mode: fixed-width or minimum-width.
The fixed-width mode left-pads the value with zeros when it is shorter than the specified
width and truncates the value when longer. Minimum-width mode works the same as fixed-width
except that it does not truncate values longer than the width.

When creating a `format` you can use any non-code characters as a separator. For example to
generate the string "1996-01-15T00:00:00" you could use `format`: "yyyy-mm-ddTHH:MM:SS".
Note that if you need to use a code character as a literal you can use the escape character
backslash. The string "1996y01m" can be produced with the format "yyyy\\ymm\\m".
"""
format(dt::TimeType,f::AbstractString;locale::AbstractString="english") = format(dt,DateFormat(f,locale))

# vectorized
DateTime{T<:AbstractString}(Y::AbstractArray{T},format::AbstractString;locale::AbstractString="english") = DateTime(Y,DateFormat(format,locale))
function DateTime{T<:AbstractString}(Y::AbstractArray{T},df::DateFormat=ISODateTimeFormat)
    return reshape(DateTime[tryfailparse(y,df) for y in Y], size(Y))
end
Date{T<:AbstractString}(Y::AbstractArray{T},format::AbstractString;locale::AbstractString="english") = Date(Y,DateFormat(format,locale))
function Date{T<:AbstractString}(Y::AbstractArray{T},df::DateFormat=ISODateFormat)
    return reshape(Date[Date(tryfailparse(y,df)) for y in Y], size(Y))
end

format{T<:TimeType}(Y::AbstractArray{T},format::AbstractString;locale::AbstractString="english") = Dates.format(Y,DateFormat(format,locale))
function format(Y::AbstractArray{Date},df::DateFormat=ISODateFormat)
    return reshape([Dates.format(y,df) for y in Y], size(Y))
end
function format(Y::AbstractArray{DateTime},df::DateFormat=ISODateTimeFormat)
    return reshape([Dates.format(y,df) for y in Y], size(Y))
end
