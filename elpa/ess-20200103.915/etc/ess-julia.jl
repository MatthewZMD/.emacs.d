module ESS

# These methods have been deprecated / moved
macro current_module()
    return VERSION >= v"0.7-" ? :(@__MODULE__) : :(current_module())
end

parse = VERSION >= v"0.7-" ? Base.Meta.parse : Base.parse
function_module = VERSION >= v"0.7-" ? Base.parentmodule : Base.function_module

function all_help_topics()
    ## There are not clear topics anymore. Approximate those with a very general
    ## apropos(" ")
    Base.Docs.apropos(" ")
end 

function help(topic::AbstractString)
    if (VERSION >= v"1.0-")
        Core.eval(parentmodule(ESS), parse("@doc $topic"))
    elseif  (VERSION >= v"0.4-")
        Core.eval(@current_module(), parse("@doc $topic"))
    else
        Base.Help.help(topic)
    end
end    

## modified version of function show(io::IO, m::Method)
function fun_args(m::Method)
    tv, decls, file, line = Base.arg_decl_parts(m)
    io = VERSION >= v"0.7-" ? Base.stdout : STDOUT::IO # STDOUT is no longer in 1.0
    if !isempty(tv)
        Base.show_delim_array(io, tv, '{', ',', '}', false)
    end
    print(io, "(")
    join(io, [escape_string(isempty(d[2]) ? d[1] : d[1]*"::"*d[2]) for d in decls],
         ",", ",")    
    Base.print(io, ")")
end 

## modified versionof show(io::IO, mt::MethodTable)
function fun_args(f::Function)
    mt = Base.MethodList(methods(f).mt)
    mod = function_module(f)   # Base.function_module deprecated in 0.7
    if mod == Main
        mod = "nil"
    end 
    print("(list \"$mod\" nil '(")
    for d in mt
        print("\"")
        ## method
        fun_args(d)
        print("\" ")
    end
    print("))")
end

function fun_args(s::AbstractString)
    try
        mod = VERSION >= v"1.0-" ?  parentmodule(ESS) : @current_module()
        m = Core.eval(mod, parse(s))
        if ! isa(m, String)
            fun_args(m)
        end
    catch
        print("(list nil nil nil)")
    end
end 

function fun_args(t::DataType)
    print("(list nil nil '(")
    for d = fieldnames(t)
        print("\"$d\" ")
    end
    print("))")
end 


### OBJECT COMPLETION
# Must print an output of the form:
# 
# Cache                         Module
# Write                         Module
# add                           Function
# free                          Function
function components(m::Module)
    for v in sort(names(m, all=true, imported=true))
        s = string(v)
        if !startswith(s, "#") && isdefined(m,v)
            println(rpad(s, 30), summary(Core.eval(m,v)))
        end
    end
end

function components(t::DataType)
    for v in sort(fieldnames(t))
        println(rpad(string(v), 30), "field")
    end
end

function components(v)
    t = typeof(v)
    if isa(t, DataType)
        return components(t)
    end
end


### MISC
function main_modules(m::Module)
    for nm in names(m)
        if isdefined(m, nm)
            mod = Core.eval(m, nm)
            if isa(mod, Module)
                print("\"$nm\" ")
            end
        end
    end
end

if VERSION >= v"0.7-"
    main_modules() = main_modules(Base.parentmodule(@current_module()))
else
    main_modules() = main_modules(@current_module())
end

end 
