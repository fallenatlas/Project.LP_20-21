:- [codigo_comum].
:- dynamic(faz_espaco/3).


% estrutura que representa um par,
% Ind e um indice de uma lista, comecando em 1, e Val e o valor nessa posicao.
% construtor.
faz_par(Ind, Val, (Ind, Val)).

% seletores.
ind_de((Ind, _), Ind).
val_de((_, Val), Val).

% estrutura que representa um espaco,
% Soma e a soma desse espaco e List_esp e a lista com as variaveis que constituem esse espaco.
% construtor.
faz_espaco(Soma, List_esp, espaco(Soma, List_esp)).

% seletores.
soma_de(espaco(Soma, _), Soma).
espaco_de(espaco(_, List_esp), List_esp).


%combinacoes_soma(N, Els, Soma, Combs)
%N e um inteiro, Els e uma lista de inteiros, Soma e um inteiro e Combs e a lista ordenada
%cujos elementos sao as combinacoes N a N, dos elementos de Els cuja soma e Soma.
combinacoes_soma(N, Els, Soma, Combs) :-
    findall(Comb, (combinacao(N, Els, Comb), 
    sumlist(Comb, Soma)), Combs).


%permutacoes_soma(N, Els, Soma, Perms)
%N e um inteiro, Els e uma lista de inteiros, Soma e um inteiro e Perms e a lista ordenada
%cujos elementos sao as permutacoes das combinacoes N a N, dos elementos de Els cuja soma e Soma.
permutacoes_soma(N, Els, Soma, Perms) :-
    combinacoes_soma(N, Els, Soma, Combs),
    findall(Perm, (member(L, Combs),
    permutation(L, Perm)), Perms_aux),
    sort(Perms_aux, Perms).

% sublist(Sl, L, N)
% SL e uma sublista de L de comprimento N.
sublist(SL, L, N) :- 
    append([_, SL, _], L),
    length(SL, N).

% get_soma(Lista, H_V, Soma)
% Lista e uma lista com 2 inteiros que representam somas na vertical
% e na horizontal, respetivamente, e H_V e v ou h conforme o objetivo
% seja Soma ter o valor da soma na vertical ou horizontal, respetivamente.
get_soma(Lista, h, Soma) :-
    nth1(2, Lista, Soma).

get_soma(Lista, v, Soma) :-
    nth1(1, Lista, Soma).


% find_space(Fila, Sum_Esp_Ac, H_V, Sum_Esp)
% Fila e uma fila de um puzzle, Esp_Ac e uma lista que acumula o espaco encontrado,
% H_V e h ou v dependendo se a fila e para ser interpretada horizontal ou verticalmente,
% respetivamente, e Esp e uma lista que contem na primeira posicao a soma desse espaco
% e as variaveis que constituem o espaco no resto.
find_space([], Esp, _, Esp).

find_space([P,Q|_], Esp, _, Esp) :-
    \+ is_list(P), 
    is_list(Q).

find_space([P,Q|R], Esp_Aux, H_V, Esp) :-
    is_list(P), \+ is_list(Q), 
    subsumes_term(Esp_Aux, []), 
    get_soma(P, H_V, S), 
    find_space([Q|R], [S,Q|Esp_Aux], H_V, Esp).

find_space([P,Q|R], Esp_Aux, H_V, Esp) :-
    \+ is_list(P), 
    \+ is_list(Q), 
    \+ subsumes_term(Esp_Aux, []), 
    append(Esp_Aux, [Q], Aux), 
    find_space([Q|R], Aux, H_V, Esp).

find_space([P|R], Esp, _, Esp) :-
    \+ is_list(P), 
    R == [].

find_space([_|R], Esp_Aux, H_V, Esp) :-
    subsumes_term(Esp_Aux, []), 
    find_space(R, Esp_Aux, H_V, Esp).


% verificar_sublista(Fila, Sum_Esp, L)
% O resto de Sum_Esp e uma sublista de Fila de tamanho L.
verificar_sublista(Fila, [_|R], L) :-
    L_1 is L-1, sublist(R, Fila, L_1), 
    maplist(nao_lista(), R).

% nao_lista(Elem)
% significa que Elem nao e uma lista.
nao_lista(Elem) :-
    \+ is_list(Elem).


% espaco_fila(Fila, Esp, H_V)
% Fila e uma fila (linha ou coluna) de um puzzle e H_V e um dos atomos h ou v,
% conforme se trate de uma fila horizontal ou vertical, respetivamente, significa
% que Esp e uma espaco de Fila.
espaco_fila(Fila, Esp, H_V) :-
    find_space(Fila, [], H_V, Esp_Aux), 
    length(Esp_Aux, L), L > 1, 
    nth1(1, Esp_Aux, S), number(S),
    espaco_fila_aux(Fila, Esp_Aux, Esp, L).

% espaco_fila_aux(Fila, Esp_Aux, Esp, L)
% significa que Esp e uma espaco de Fila
espaco_fila_aux(_, Esp_Aux, Esp, _) :-
    cria_espaco(Esp_Aux, Esp).

% cria_espaco(Esp_Aux, Esp)
% significa que Esp e o espaco representado por Esp_Aux.
cria_espaco([P|R], Esp) :-
    faz_espaco(P, R, Esp).


% espacos_fila(H_V, Fila, Espacos)
% Fila e uma fila (linha ou coluna) de uma grelha e H_V e um dos atomos h ou v,
% significa que espacos e a lista de todos os espacos de Fila, da esquerda para a direita.
espacos_fila(_, Fila, []) :-
    maplist(is_list(), Fila), !.

espacos_fila(h, Fila, Espacos) :-
    bagof(Esp, espaco_fila(Fila, Esp, h), Espacos).

espacos_fila(v, Fila, Espacos) :-
    bagof(Esp, espaco_fila(Fila, Esp, v), Espacos).


% espacos_puzzle(Puzzle, Espacos)
% Puzzle e um puzzle, significa que Espacos e a lista de espacos de Puzzle.
espacos_puzzle(Puzzle, Espacos) :-
    mat_transposta(Puzzle, Transp), 
    maplist(space(h), Puzzle, H_space), 
    maplist(space(v), Transp, V_space), 
    append(H_space, AH_space), 
    append(V_space, AV_space), 
    append(AH_space, AV_space, Espacos).

% space(H_V, Fila, Espacos)
% H_V e um dos atomos h ou v, Fila e uma fila (linha ou coluna) de uma grelha,
% significa que Espacos e a lista com todos os espacos de Fila.
space(h, Fila, Espacos) :-
    espacos_fila(h, Fila, Espacos).

space(v, Fila, Espacos) :-
    espacos_fila(v, Fila, Espacos).


% espacos_com_posicoes_comuns(Espacos, Esp, Esps_com)
% Espacos e uma lista de espacos e Esp e um espaco,
% significa que Esps_com e a lista de espacos com variaveis em comum com Esp,
% exceptuando Esp, pela mesma ordem que aparecem em Espacos.
espacos_com_posicoes_comuns(Espacos, Esp, Esps_com) :-
    espaco_de(Esp, Pos_esp), 
    maplist(find_comum(Espacos, Esp), Pos_esp, Pos_esps), 
    append(Pos_esps, Esps_com).

% find_comum(Espacos, Esp, Pos, Pos_Com)
% Espacos e uma lista de espacos, Esp e um espaco e Pos e uma posicao de Esp,
% significa que Pos_Com e um espaco em comum com Esp.
find_comum(Espacos, Esp, Pos, Pos_Com) :-
    bagof(E, (member(E, Espacos), 
    E \== Esp, 
    espaco_de(E, Esp_L), 
    membro(Pos, Esp_L)), Pos_Com), !.

find_comum(_, _, _, []).

% membro(X, L)
% significa que X e um elemento da lista L, nao unifica os elementos da lista com X.
membro(X, [P|_]) :- X == P.
membro(X, [_|R]) :- membro(X, R).


% permutacoes_soma_espacos(Espacos, Perms_soma)
% Espacos e uma lista de espacos, significa que Perms_soma e a lista de listas de 2 elementos,
% em que o primeiro elemento e um espaco de Espacos e o segundo e a lista ordenada de permutacoes
% cuja soma e igual a do espaco.
permutacoes_soma_espacos(Espacos, Perms_soma) :-
    maplist(find_perms(), Espacos, Perms_soma).

% find_perms(Espaco, Esp_Perm)
% Espaco e um espaco e Esp_Perm e a lista de 2 elementos em que o primeiro elemento e um espaco
% de Espacos e o segundo e a lista ordenada de permutacoes cuja soma e igual a do espaco.
find_perms(Espaco, Esp_Perm) :-
    soma_de(Espaco, Soma), 
    espaco_de(Espaco, Esp), 
    length(Esp, Len), 
    permutacoes_soma(Len, [1,2,3,4,5,6,7,8,9], Soma, Perms),
    include(subsumes_term(Esp), Perms, Perms_filt),
    append([Espaco], [Perms_filt], Esp_Perm).


% permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma)
% Perm e uma permutacao, Esp e um espaco, Espacos e uma lista de espacos,
% Perms_soma e uma lista de listas, tal como em permutacoes_soma_espacos,
% e significa que Perm e uma permutacao possivel para o espaco Esp.
permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma) :-
    permutacoes_soma_espacos([Esp], Esp_perm_aux),
    nth1(1, Esp_perm_aux, Esp_perm),
    nth1(2, Esp_perm, Perms),
    espacos_com_posicoes_comuns(Espacos, Esp, Esps_com),
    obter_perms_soma_com(Esps_com, Perms_soma, Perms_soma_com),
    perms_possiveis(Perms, Perms_soma_com, Perm).

% perms_possiveis(Perms, Perms_soma_com, Perm)
% Perms e uma lista de permutacoes e Perms_soma_com e uma lista de listas como em
% permutacoes_soma_espacos mas que apenas contem os espacos em comum com o espaco
% ao qual se referem as permutacoes em Perms e significa que Perm e uma permutacao
% possivel para aquele espaco.
perms_possiveis([Perm|_], Perms_soma_com, Perm) :-
    compativeis(Perm, Perms_soma_com).

perms_possiveis([_|R], Perms_soma_com, Perm) :-
    perms_possiveis(R, Perms_soma_com, Perm).

% compativeis(Perm, Perms_soma_com)
% Perms e uma permutacao e Perms_soma_com e uma lista de listas como em
% permutacoes_soma_espacos, significa que Perm e uma permutacao possivel para o espaco.
compativeis(_, []) :- !.

compativeis([Num|R2], [Esp_perm_com|R3]) :-
    nth1(2, Esp_perm_com, Perms_poss),
    num_in_perms(Num, Perms_poss),
    compativeis(R2, R3).

% num_in_perms(Num, Perms_poss)
% Num e um inteiro e Perms_poss e uma lista de permutacoes,
% significa que Num, existe em pelo menos uma das permutacoes de Perms_poss
num_in_perms(Num, [Perms|_]) :-
    member(Num, Perms), !.

num_in_perms(Num, [_|R]) :-
    num_in_perms(Num, R).

% obter_perms_soma_com(Esps_com, Perms_soma, Perms_soma_com)
% Esps_com e uma lista de espacos em comum, Perms_Soma e uma lista de listas como em
% permutacoes_soma_espacos, significa que Perms_soma_com e a lista que contem as
% listas de Perms_soma que contem os espacos de Esps_com.
obter_perms_soma_com(Esps_com, Perms_soma, Perms_soma_com) :-
    bagof(Elem, (membro_Esp_com(Esps_com, Perms_soma, Elem)), Perms_soma_com), !.

% membro_Esp_com(Esps_com, Perms_soma, Elem)
% Esps_com e uma lista de espacos, Perms soma e uma lista de listas com em 
% permutacoes_soma_espacos, significa que Elem e uma lista de Perms_soma
% que contem um espaco de Esps_com.
membro_Esp_com(Esps_com, Perms_soma, Elem) :-
    member(El, Esps_com), member(Elem, Perms_soma), membro(El, Elem).


% permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp, Perms_poss).
% Espacos e uma lista de espacos, Perms_soma e uma lista de listas como obtida pelo predicado
% permutacoes_soma_espacos, e Esp e uma espaco, significa que Perms_poss e uma lista de 2 elementos
% em que o primeiro e a lista de variaveis de Esp e o segundo e a lista ordenada de permutacoes 
% possiveis para o espaco Esp.
permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp, Perms_poss) :-
    espaco_de(Esp, Esp_vars),
    findall(Perm, permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma), L_perms),
    append([Esp_vars], [L_perms], Perms_poss).


% permutacoes_possiveis_espacos(Espacos, Perms_poss_esps)
% Espacos e uma lista de espacos, significa que Perms_poss_esps e a lista de permutacoes possiveis.
permutacoes_possiveis_espacos(Espacos, Perms_poss_esps) :-
    permutacoes_soma_espacos(Espacos, Perms_soma),
    maplist(permutacoes_possiveis_espaco(Espacos, Perms_soma), Espacos, Perms_poss_esps).


% numeros_comuns(Lst_Perms, Numeros_comuns)
% Lst_Perms e uma lista de permutacoes, significa que Numeros_comuns e uma lista de pares (pos, numero),
% significando que todas as listas de Lst_Perms contem o numero numero na posicao pos.
numeros_comuns(Lst_Perms, Numeros_comuns) :-
    nth1(1, Lst_Perms, Perm),
    length(Perm, N),
    build_list(N, List_Ind),
    findall(Par, (member(Ind, List_Ind), ind_comum(Lst_Perms, Ind, Par)), Numeros_comuns).

% ind_comum(Lst_Perms, Ind, Par)
% Lst_Perms e uma lista de permutacoes, Ind e um indice de uma permutacao e Par e um par,
% (pos, numero) que significa que todas as permutacoes contem o numero numero na posicao Ind.
ind_comum(Lst_Perms, Ind, Par) :-
    maplist(nth1(Ind), Lst_Perms, List_El),
    el_equal(List_El, El),
    Par = (Ind, El).

% el_equal(List_el, El)
% significa que El e o inteiro comum a todas as posicoes da lista.
el_equal(List_el, El) :-
    nth1(1, List_el, El),
    exclude(compare(=,El), List_el, List_el_aux),
    length(List_el_aux, N),
    N = 0.


% build_list(Ind, List_Ind)
% Significa que List_Ind e a lista que contem todos os inteiros, comecando em 1,
% ate ao inteiro Ind.
build_list(Ind, List_Ind) :-
    build_list_aux(Ind, 1, List_Ind).

build_list_aux(Ind, N, []) :-
    N > Ind, !.

build_list_aux(Ind, N, [N|R]) :-
    N_1 is N + 1,
    build_list_aux(Ind, N_1, R).


% atribui_comuns(Perms_Possiveis)
% Perm_Possiveis e uma lista de permutacoes possiveis, atualiza esta lista atribuindo a
% cada espaco numeros comuns a todas as permutacoes possiveis para esse espaco.
atribui_comuns(Perms_Possiveis) :-
    maplist(atribui_comuns_aux(), Perms_Possiveis).

% atribui_comuns_aux(Esp_Perm)
% Esp_Perm e uma lista de permutacoes possiveis que e atualizada atribuindo, que e atualizada
% atribuindo ao espaco os numeros comuns a todas as permutacoes.
atribui_comuns_aux(Esp_Perm) :-
    nth1(1, Esp_Perm, Esp),
    nth1(2, Esp_Perm, Perms),
    numeros_comuns(Perms, Numeros_comuns),
    maplist(atribui_val(Esp), Numeros_comuns).

% atribui_val(Esp, Par)
% aplica a substuicao indicada por par (pos, numero) no espaco Esp.
atribui_val(Esp, Par) :-
    ind_de(Par, Ind),
    val_de(Par, Val),
    nth1(Ind, Esp, Val).


% retira_impossiveis(Perms_Possiveis, Novas_Perms_Possiveis)
% Perms_Possiveis e uma lista de permutacoes possiveis, significa que Novas_Perms_Possiveis
% e o resultado de tirar permutacoes impossiveis de Perms_Possiveis.
retira_impossiveis(Perms_Possiveis, Novas_Perms_Possiveis) :-
    maplist(find_new_perms(), Perms_Possiveis, Novas_Perms_Possiveis).

% find_new_perms(Esp_Perms, Novas_Perms_Possiveis)
% Novas_Perms_Possiveis e o resultado de tirar permutacoes impossiveis a Esp_Perms.
find_new_perms(Esp_Perms, Novas_Perms_Possiveis) :-
    nth1(1, Esp_Perms, Esp),
    nth1(2, Esp_Perms, Perms),
    include(subsumes_term(Esp), Perms, Perms_filt),
    append([Esp], [Perms_filt], Novas_Perms_Possiveis).


% simplifica(Perms_Possiveis, Novas_Perms_Possiveis)
% Perms_Possiveis e uma lista de permutacoes possiveis, significa que Novas_Perms_Possiveis
% e o resultado de simplificar Perms_Possiveis.
simplifica(Novas_Perms_Possiveis, Novas_Perms_Possiveis) :-
    atribui_comuns(Novas_Perms_Possiveis),
    retira_impossiveis(Novas_Perms_Possiveis, Novas_Perms_Possiveis_aux),
    subsumes_term(Novas_Perms_Possiveis, Novas_Perms_Possiveis_aux), !.

simplifica(Perms_Possiveis, Novas_Perms_Possiveis) :-
    atribui_comuns(Perms_Possiveis),
    retira_impossiveis(Perms_Possiveis, Novas_Perms_Possiveis_aux),
    simplifica(Novas_Perms_Possiveis_aux, Novas_Perms_Possiveis).


% inicializa(Puzzle, Perms_Possiveis)
% Puzzle e uma puzzle, significa que Perms_Possiveis e a lista de permutacoes possiveis
% simplificada para Puzzle
inicializa(Puzzle, Perms_Possiveis) :-
    espacos_puzzle(Puzzle, Espacos),
    permutacoes_possiveis_espacos(Espacos, Perms_Possiveis_aux),
    simplifica(Perms_Possiveis_aux, Perms_Possiveis), !.


% escolhe_menos_alternativas(Perms_Possiveis, Escolha)
% Perms_Possiveis e uma lista de permutacoes possiveis, significa que Escolha e o 
% elemento de Perms_Possiveis escolhido por ser o primeiro com menos permutacoes.
escolhe_menos_alternativas(Perms_Possiveis, Escolha) :-
    maplist(conta_n_perms(), Perms_Possiveis, N_Perms),
    find_min(N_Perms, Min_Perms),
    find_escolha(Perms_Possiveis, N_Perms, Min_Perms, Escolha).

% conta_n_perms(Esp_Perms, N_perm)
% N_perm e o numero de Permutacoes de Esp_Perms.
conta_n_perms(Esp_Perms, N_perm) :-
    nth1(2, Esp_Perms, Perms),
    length(Perms, N_perm).

% find_min(N_Perms, Min_Perms)
% N_Perms e uma lista de inteiros, significa que Min_Perms e o
% menor inteiro nessa lista.
find_min(N_Perms, Min_Perms) :-
    include(compare(<,1), N_Perms, N_Perms_1),
    min_list(N_Perms_1, Min_Perms).

% find_escolha(Perms_Possiveis, N_Perms, Min_Perms, Escolha)
% Escolha e o primeiro elemento de Perms_Possiveis que tem numero de permutacoes
% N_Perms igual ao minimo Min_Perms. 
find_escolha([Escolha|_], [Min|_], Min, Escolha) :- !.

find_escolha([_|R_E], [_|R_N], Min, Escolha) :-
    find_escolha(R_E, R_N, Min, Escolha).


% experimenta_perm(Escolha, Perms_Possiveis, Novas_Perms_Possiveis)
% Perms_Possiveis e uma lista de permutacoes possieis, e Escolha e um dos seus elementos,
% significa que Novas_Perms_Possiveis e o resultado de substituir em Perms_Possiveis, o 
% elemento Escolha pelo elemento [Esp, [Perm]] com Esp o espaco de Escolha e Perm as permutacoes
% de Escolha, com Esp unificado com Perm.
experimenta_perm(Escolha, Perms_Possiveis, Novas_Perms_Possiveis) :-
    nth1(1, Escolha, Esp),
    nth1(2, Escolha, Lst_Perms),
    member(Perm, Lst_Perms),
    Esp = Perm,
    substitui_perms_possiveis(Escolha, Perms_Possiveis, Esp, Perm, Novas_Perms_Possiveis).

% substitui_perms_possiveis(Escolha, Perms_Possiveis, Esp, Perm, Novas_Perms_Possiveis)
% faz a substituicao de Escolha por [Esp, [Perm]] como especificado em experimenta_perm.
substitui_perms_possiveis(Escolha, [Escolha|R], Esp, Perm, [[Esp,[Perm]]|R]) :- !.

substitui_perms_possiveis(Escolha, [Esp_Perms|R1], Esp, Perm, [Esp_Perms|R2]) :-
    Escolha \= Esp_Perms,
    substitui_perms_possiveis(Escolha, R1, Esp, Perm, R2).


% resolve_aux(Perms_Possiveis, Novas_Perms_Possiveis)
% Perms_Possiveis e uma lista de permutacoes possiveis, significa que Novas_Perms_Possiveis
% e o resultado de aplicar uma substituicao com experimenta_perm e simplificar Perms_Possiveis.
resolve_aux(Novas_Perms_Possiveis, Novas_Perms_Possiveis) :-
    \+ escolhe_menos_alternativas(Novas_Perms_Possiveis, _).

resolve_aux(Perms_Possiveis, Novas_Perms_Possiveis) :-
    escolhe_menos_alternativas(Perms_Possiveis, Escolha),
    experimenta_perm(Escolha, Perms_Possiveis, Exp_Perms_Possiveis),
    simplifica(Exp_Perms_Possiveis, Novas_Perms_Possiveis_aux),
    resolve_aux(Novas_Perms_Possiveis_aux, Novas_Perms_Possiveis), !.


% resolve(Puz)
% Puz e um Puzzle, resolve esse Puzzle.
resolve(Puz) :-
    inicializa(Puz, Perms_Possiveis),
    resolve_aux(Perms_Possiveis, _).