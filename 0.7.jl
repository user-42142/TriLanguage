using Symbolics
using ArgParse
settings = ArgParseSettings()
@add_arg_table settings begin
    "input_file"
        help = "Arquivo a ser compilado"
        arg_type = String
        default = "teste.mrl"
    "--output"
        help = "Saída do compilador"
        arg_type = String
        default = nothing
    "--debug"
        help = "Modo de Debug"
        action = :store_true
end
args = parse_args(settings)
texto = read(args["input_file"], String)
function compile(args, texto, __vars__)
#remover comentários
incomment = false
instring = false
inchar = false
lbracket = 0
previnslash = false
newline = ""
for char in texto
    inslash = false
    if char == '\\' 
        inslash = true
    end
    if char == '"' && !inchar && !previnslash
        instring = !instring
    end
    if char == ''' && !instring && !previnslash
        inchar = !inchar
    end
    if !incomment && char == '(' && !inchar && !instring
        incomment = true
    elseif incomment && char == ')' && !inchar && !instring
        incomment = false
    elseif !incomment
        if (instring || inchar) && char == '\n'
            newline *= "\\n"
        else
            newline *= char
        end
    end
    previnslash = inslash
end
texto = newline
lines = split(texto, '\n')
function extrair_valores(linha::AbstractString, padroes::Vector{String})
    for padrao in padroes
        # Escapar caracteres regex perigosos
        regex = replace(padrao, r"([\\\.\+\*\?\[\]\(\)\^\$\|])" => s"\\\1")

        # Substituir os elementos especiais
        regex = replace(regex, r" " => " +")  # espaço normal = ao menos um espaço
        regex = replace(regex, "{spaces}" => " *")
        regex = replace(regex, r"<opt>(.*?)</opt>" => s"(?:\1)?")
        regex = replace(regex, "{val}" => "(.+?)")

        # Regex de correspondência completa
        re = Regex("^" * regex * "\$", "i")
        m = match(re, linha)
        if m !== nothing
            return [String(v) for v in m.captures if v !== nothing]
        end
    end
    return nothing
end
function limpar_floats(texto)
    replace(texto, r"(\d+)\.0+\b" => s"\1")
end

function resolver_equacao(lhs::AbstractString, rhs::AbstractString, varname::AbstractString)
    function expressao_simples(txt)
        occursin(r"^[a-zA-Z_]\w*$", txt) || occursin(r"^\".*\"$", txt)
    end
    
    # Check if rhs contains a function call like readline()
    if occursin(r"\w+\s*\(.*\)", rhs)
        return limpar_floats("$(varname) = $(rhs)")
    end
    
    if expressao_simples(lhs) && expressao_simples(rhs)
        return limpar_floats("$(varname) = $(rhs)")
    end
    if lhs == varname
        return limpar_floats("$(varname) = $(rhs)")
    end
    texto_total = lhs * " " * rhs
    nomes_vars = unique([m.match for m in eachmatch(r"[a-zA-Z_]\w*", texto_total)])
    simbolos = Dict{String,Any}()
    for nome in nomes_vars
        simbolos[nome] = Symbolics.variable(Symbol(nome))
    end
    function substituir(expr)
        if expr isa Symbol
            s = String(expr)
            if haskey(simbolos, s)
                return simbolos[s]
            else
                return expr
            end
        elseif expr isa Number
            return expr
        elseif expr isa Expr
            return Expr(expr.head, substituir.(expr.args)...)
        else
            return expr
        end
    end
    lhs_parsed = Meta.parse(lhs)
    rhs_parsed = Meta.parse(rhs)
    lhs_sub = substituir(lhs_parsed)
    rhs_sub = substituir(rhs_parsed)
    lhs_eval = eval(lhs_sub)
    rhs_eval = eval(rhs_sub)
    eq = lhs_eval ~ rhs_eval
    if !haskey(simbolos, varname)
        return limpar_floats("variável $(varname) não encontrada nas expressões")
    end
    var = simbolos[varname]
    sol = try
        Symbolics.solve_for(eq, var)
    catch
        nothing
    end
    function invalido(s)
        isnothing(s) ||
        s == [] ||
        (isa(s, Number) && isnan(s)) ||
        string(s) == string(eq)
    end
    if invalido(sol)
        isolado = simplify(rhs_eval - (lhs_eval - var))
        return limpar_floats("$(varname) = $(string(isolado))")
    else
        sol = sol isa AbstractArray ? sol[1] : sol
        sol_simp = simplify(sol)
        return limpar_floats("$(varname) = $(string(sol_simp))")
    end
end
function substituir_vars(texto::AbstractString, lista, funcs=nothing, __vars__=nothing)
    resultado = texto
    
    # Substituir {vals[i]} por valores da lista
    regex = r"\{vals\[(\d+)\]\}"
    for m in eachmatch(regex, texto)
        idx = parse(Int, m.captures[1])
        if idx >= 1 && idx <= length(lista)
            valor = lista[idx]
            # NÃO substituir nomes de variáveis aqui — faremos isso apenas após aplicar as funções
            resultado = replace(resultado, m.match => valor)
        end
    end
    
    return resultado
end




# registra/retorna placeholder para um nome de variável (aceita nomes com espaços)
function get_var_placeholder(nome::AbstractString)
    idx = findfirst(x -> x == nome, __vars__)
    if idx === nothing
        push!(__vars__, nome)
        idx = length(__vars__)
    end
    return "variable_$idx"
end

# substitui valores capturados por placeholders se forem nomes registrados em __vars__
function valores_para_placeholders(vals::Vector{String})
    [ (findfirst(x -> x == v, __vars__) === nothing) ? v : "variable_$(findfirst(x->x==v,__vars__))" for v in vals ]
end


defsvars = ["{val}{spaces}={spaces}{val}", 
            "define {val} como {val}", 
            "set {val} to {val}", 
            "{val} agora é <opt>igual a </opt>{val}", 
            "{val} now is <opt>equal to</opt>{val}",
            "{val} passa a ser <opt>igual a </opt>{val}",
            "{val} se torna {val}",
            "declare {val} como {val}",
            "ajuste {val} para {val}",
            "mude {val} para {val}",
            "<opt>define que </opt>{val} <opt>vale</opt><opt>é igual</opt><opt> a</opt><opt> á</opt> {val}",
            "faça {val} <opt>ser </opt><opt>igual a </opt>{val}",
            "configure {val} para {val}",
            "define {val} as {val}",
            "{val} becomes {val}",
            "change {val} to {val}",
            "{val} equals {val}",
            "{val} is equal to {val}",
            "adjust {val} to {val}",
            "update {val} to {val}",
            "var {val}{spaces}={spaces}{val}",
            "let {val}{spaces}={spaces}{val}",
            "assign {val} {val}"]
impnls = ["printe {val}<opt>, mas</opt> sem <opt>dar </opt>enter",
          "imprime {val}<opt>, mas</opt> sem <opt>dar </opt>enter", 
          "imprima {val}<opt>, mas</opt> sem <opt>dar </opt>enter", 
          "printnl {val}", 
          "print {val}<opt>, but</opt> without <opt>giving </opt>enter", 
          "mostre {val}<opt>, mas</opt> sem <opt>dar </opt>enter", 
          "show {val}<opt>, but</opt> without <opt>giving </opt>enter",
          "write {val}<opt>, but</opt> without <opt>giving </opt>enter",
          "display {val}<opt>, but</opt> without <opt>giving </opt>enter",
          "echo {val}<opt>, but</opt> without <opt>giving </opt>enter",
          "output {val}<opt>, but</opt> without <opt>giving </opt>enter",
          "echonl {val}",
          "outnl {val}",
          "writenl {val}"]
imprs = ["printe {val}", 
         "imprime {val}", 
         "imprima {val}", 
         "print {val}", 
         "mostre <opt>linha </opt><opt>na tela </opt>{val}<opt> na tela</opt>",
         "mostra <opt>linha </opt><opt>na tela </opt>{val}<opt> na tela</opt>",
         "show {val}",
         "escreva <opt>linha </opt>{val}",
         "exiba {val}",
         "display {val}",
         "write {val}",
         "echo {val}",
         "output {val}",
         "println {val}",
         "out {val}"]
inputsquest = ["responda {val}", 
               "answer {val}",
               "input {val}", 
               "entrada {val}", 
               "ask {val}", 
               "pergunte {val}",
               "<opt>a </opt>resposta <opt>do usuário </opt>para<opt> a pergunta{spaces}</opt><opt>:</opt>{spaces}{val}",
               "<opt>o </opt>texto escrito pelo usuário em resposta a{spaces}<opt>:<opt>{spaces}{val}",
               "<opt>the </opt><opt>user's </opt>answer to<opt> the question{spaces}</opt><opt>:</opt>{spaces}{val}",
               "<opt>the </opt>text written by the user in response to{spaces}<opt>:<opt>{spaces}{val}"]
readlines = ["readline",
             "read line",
             "ler linha", 
             "lerlinha", 
             "input",
             "<opt>a </opt>linha digitada<opt> pelo usuário</opt>", 
             "<opt>the </opt>typed line",
             "the line typed by the user",
             "esperar o usuário apertar enter",
             "wait for the user <opt>to </opt>press enter",
             "<opt>a </opt> entrada do teclado",
             "<opt>the </opt>keyboard's input",
             "<opt>o </opt>texto escrito pelo usuário"]
increments = [
              "incremente {val} em {val}",
              "increment {val} by {val}",
              "{val} += {val}",
              "adicionar <opt>a</opt><opt>á variável</opt> {val} <opt>o </opt><opt>valor</opt><opt>número</opt><opt> de</opt> {val}",
              "add to<opt> the variable</opt> {val} <opt>the </opt><opt>value</opt><opt>number</opt><opt> of</opt> {val}"
]
increments2 = [
              "incremente {val}",
              "increment {val}",
              "{val} ++"
]
decrements = [
              "<opt>decremente</opt><opt>tire</opt> <opt>de </opt>{val} <opt>o valor </opt>{val}",
              "decrement {val} by {val}",
              "{val} -= {val}",
]
decrements2 = [
              "decremente {val}",
              "decrement {val}",
              "{val} --"
]
multiplies = [
              "multiplique {val} por {val}",
              "multiply {val} by {val}",
              "{val} *= {val}",
]
divides = [
              "divida {val} por {val}",
              "divide {val} by {val}",
              "{val} /= {val}",
]
elevates = [
              "eleve {val} a {val}",
              "raise {val} to the power of {val}",
              "{val} ^= {val}",
]
plusigns = [" mais ", " plus "]
minusigns = [" menos ", " minus "]
timesigns = [" vezes ", " multiplied by ", " times ", "<opt> junto</opt> com <opt>o texto </opt>", "<opt> together</opt> with <opt>the text </opt>"]
divsigns = [" dividido por ", " divided by "]
squaredsigns = [" <opt>elevado </opt>ao quadrado", " squared "]
raisedsigns = [" elevado <opt>a</opt><opt>á potência de</opt> ", " <opt>raised </opt>to the power of "]
modsigns = [" mod "]
funcs = Dict(defsvars => "var", 
             impnls => "print({vals[1]})",
             imprs => "println({vals[1]})",
             inputsquest => "input({vals[1]})",
             readlines => "readline()",
             increments => "{vals[1]} = {vals[1]} + {vals[2]}",
             increments2 => "{vals[1]} = {vals[1]} + 1",
             decrements => "{vals[1]} = {vals[1]} - {vals[2]}",
             decrements2 => "{vals[1]} = {vals[1]} - 1",
             multiplies => "{vals[1]} = {vals[1]} * {vals[2]}",
             divides => "{vals[1]} = {vals[1]} / {vals[2]}",
             elevates => "{vals[1]} = {vals[1]} ^ {vals[2]}",
             plusigns => " + ",
             minusigns => " - ",
             timesigns => " * ",
             divsigns => " / ",
             squaredsigns => " ^ 2 ",
             raisedsigns => " ^ ",
             modsigns => " % ",
             ["["] => "(",
             ["]"] => ")",
            )
comandos = []

for linha_sub in lines
    linha = strip(String(linha_sub))
    if isempty(linha)
        continue
    end
    for (k,v) in funcs
        if !occursin("{val}",k[1])
            for s in k
                linha = replace(linha, s => v)
            end
            continue
        end
        vals = extrair_valores(linha,k)
        if isnothing(vals)
            continue
        end
        push!(comandos,(v,vals))
        break
    end
end


# COMPILAR PARA JULIA
julia_code = "function input(question)
    print(question)
    return readline()
end
"

for comando in comandos
    tipo, valores = comando
    if args["debug"]
        println("Processando comando do tipo '$tipo' com valores: ", valores)
    end
    if tipo == "nr"
        julia_code *= valores[1] * "\n"
        continue
    end
    if tipo == "var"
        nome_var = valores[1]
        valor_var = valores[2]

        # PRIMEIRO: Substituir operações matemáticas ANTES de processar variáveis
        valor_var = replace(valor_var, "mais" => "+", "menos" => "-", "vezes" => "*", "dividido por" => "/", "elevado a" => "^")
        valor_var = replace(valor_var, r"^\s+|\s+$" => "")  # remover espaços extras
        nome_var = replace(nome_var, "mais" => "+", "menos" => "-", "vezes" => "*", "dividido por" => "/", "elevado a" => "^")
        nome_var = replace(nome_var, r"^\s+|\s+$" => "")  # remover espaços extras

        # Detectar/registrar e substituir nomes (podem conter espaços) no nome_var
        matches = [m.match for m in eachmatch(r"[A-Za-zÁÀÂÃÉÊÍÓÔÕÚÇáàâãéêíóôõúç_][A-Za-zÁÀÂÃÉÊÍÓÔÕÚÇáàâãéêíóôõúç0-9_ ]*", nome_var)]
        if isempty(matches)
            continue
        end
        for n in matches
            placeholder = get_var_placeholder(n)
            esc = replace(n, r"([\\\.\+\*\?\[\]\(\)\^\$\|])" => s"\\\1")
            pattern = Regex("(?<![A-Za-z0-9_])" * esc * "(?![A-Za-z0-9_])")
            nome_var = replace(nome_var, pattern => placeholder)
        end

        # --- NOVO: detectar e aplicar padrões/funções dentro do valor_var ---
        # tenta casar padrões de `funcs` dentro do valor (ex.: frases de input)
        for (kp, vp) in funcs
            # se o padrão contém {val}, tente extrair valores do valor_var
            if any(contains.(kp, "{val}"))
                vals_inner = extrair_valores(valor_var, kp)
                if vals_inner !== nothing
                    # gera substituição aplicando os {vals[i]} capturados
                    valor_var = substituir_vars(vp, vals_inner, funcs, __vars__)
                    break
                end
            else
                # padrões sem {val} — substituições literais das variantes
                for s in kp
                    if occursin(s, valor_var)
                        valor_var = replace(valor_var, s => vp)
                    end
                end
            end
        end
        # --- fim do bloco novo ---

        # Processar nomes de variáveis no valor_var
        # Substituir ocorrências de variáveis já registradas (aceita nomes com espaços)
        if __vars__ !== nothing
            for n in __vars__
                if occursin(n, valor_var)
                    esc = replace(n, r"([\\\.\+\*\?\[\]\(\)\^\$\|])" => s"\\\1")
                    pattern = Regex("(?<![A-Za-z0-9_])" * esc * "(?![A-Za-z0-9_])")
                    idx = findfirst(x -> x == n, __vars__)
                    placeholder = "variable_$idx"
                    valor_var = replace(valor_var, pattern => placeholder)
                end
            end
        end

        nome_var_sub = String(nome_var)
        valor_var_sub = String(valor_var)

        m = match(r"[A-Za-z_]\w*", nome_var_sub)
        if isnothing(m)
            continue
        end
        varname = m.match
        equacao_resolvida = resolver_equacao(nome_var_sub, valor_var_sub, varname)

        julia_code *= equacao_resolvida * "\n"
        continue
    end

    # Para comandos não-var: processar PRIMEIRO as funções, DEPOIS as variáveis
    nova_linha = substituir_vars(tipo, valores, funcs, __vars__)
    
    # Processar operações matemáticas dentro de strings
    for (kp, vp) in funcs
        nova_linha = replace(nova_linha, kp[1] => vp)
    end
    
    # Agora processar nomes de variáveis DEPOIS das funções
    matches_final = [m.match for m in eachmatch(r"[A-Za-zÁÀÂÃÉÊÍÓÔÕÚÇáàâãéêíóôõúç_][A-Za-zÁÀÂÃÉÊÍÓÔÕÚÇáàâãéêíóôõúç0-9_ ]*", nova_linha)]
    # ordenar por comprimento decrescente para evitar substituições parciais (ex: "a" dentro de "variable_1")
    sort!(matches_final, by=length, rev=true)
    for n in matches_final
        idx = findfirst(x -> x == n, __vars__)
        if idx !== nothing
            placeholder = "variable_$idx"
            esc = replace(n, r"([\\\.\+\*\?\[\]\(\)\^\$\|])" => s"\\\1")
            pattern = Regex("(?<![A-Za-z0-9_])" * esc * "(?![A-Za-z0-9_])")
            nova_linha = replace(nova_linha, pattern => placeholder)
        end
    end
    
    julia_code *= nova_linha * "\n"
end

# AGORA converter { em $( e } em ) APENAS em strings dentro do código gerado
function converter_interpolacao(codigo::String)
    resultado = IOBuffer()
    instring = false
    previnslash = false
    lbracket = 0
    
    for char in codigo
        inslash = char == '\\'
        
        if char == '"' && !previnslash
            instring = !instring
            print(resultado, char)
        elseif instring && char == '{' && !previnslash && lbracket <= 0
            lbracket = 1
            print(resultado, "\$(")
        elseif instring && char == '{' && !previnslash
            lbracket += 1
            print(resultado, char)
        elseif instring && char == '}' && !previnslash && lbracket > 0
            lbracket -= 1
            print(resultado, ")")
        elseif instring && char == '}' && !previnslash
            lbracket -= 1
            print(resultado, char)
        elseif instring && char == '$' && !previnslash
            print(resultado, "\\\$")
        else
            print(resultado, char)
        end
        
        previnslash = inslash
    end
    
    String(take!(resultado))
end

julia_code = converter_interpolacao(julia_code)

if args["debug"]
    println("Código Julia gerado:\n", julia_code)
end
julia_code
end
code = compile(args,texto,[])

# SALVAR CÓDIGO JULIA
if isnothing(args["output"])
   output = replace(args["input_file"],".mrl" => ".jl")
   output = replace(output,".marley" => ".jl")
else
    output = args["output"]
end
open(output, "w") do file
    write(file, code)
end
if args["debug"]
    println("Código Julia salvo em $output")
end