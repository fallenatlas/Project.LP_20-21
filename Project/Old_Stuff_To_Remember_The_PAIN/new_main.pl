:- [codigo_comum].
:- dynamic(faz_espaco/3).
    
combinacoes_soma(N, Els, Soma, Combs) :-
    findall(Comb, (combinacao(N, Els, Comb), sumlist(Comb, Soma)), Combs).

permutacoes_soma(N, Els, Soma, Perms) :-
    combinacoes_soma(N, Els, Soma, Combs),
    findall(Perm, (member(L, Combs), permutation(L, Perm)), Perms_aux),
    sort(Perms_aux, Perms).

faz_par(Ind, Val, (Ind, Val)).

ind_de((Ind, _), Ind).
val_de((_, Val), Val).

faz_espaco(Soma, List_esp, espaco(Soma, List_esp)).

soma_de(espaco(Soma, _), Soma).
espaco_de(espaco(_, List_esp), List_esp).

muda_soma(Soma, espaco(_, List_esp), espaco(Soma, List_esp)).
muda_espaco(List_esp, espaco(Soma, _), espaco(Soma, List_esp)).
/*
espaco_fila(Fila, Esp, h) :-
    findall(I, (member(I, Fila, Elem), is_list(Elem))


espaco_fila(Fila, Esp, h) :-
    n_espaco(Fila, N_list),
    sublist(Sub, List, Num), maplist(is_list(), S_list), faz_espaco(Soma, S_list, Esp).



sublist(Sl, L, N) :- append([_, Sl, _], L), length(List, Int).

n_espaco(List, N) :- n_espaco(List, 0, N).

n_espaco([], N, N).
n_espaco([P|R], N) :-
    \+ is_list(P), N_1 ,
    n_espaco(R, N_1), N is N_1.
n_espaco([_|R], N) :-
    n_espaco(R, N).


espaco_fila(Fila, Esp, H_V) :-
    member(P, Fila), is_list(P), n_espaco(Fila, L_esp).
*/


/*
find_space([], []).

find_space([P,Q|_], Fila, Esp) :-
    write_ln(P), write_ln(Q), nth1(Min, Fila, P), nth1(4, Fila, Q), Esp = [Min, 4].%setof(I, (between(Min, Max, I)), Esp).




espaco_fila(Fila, Esp) :-
    findall(P, (member(P, Fila), is_list(P)), L_I), find_space(L_I, Fila, Esp).

teste(Fila, Esp) :-
    findall(I, (member(P, Fila), is_list(P), nth1(I, Fila, P)), Esp).
*/

sublist(SL, L, N) :- append([_, SL, _], L), length(SL, N).


get_soma(List, h, Soma) :-
    nth1(2, List, Soma).

get_soma(List, v, Soma) :-
    nth1(1, List, Soma).

find_space([], Esp, _, Esp).
    %write_ln("here0").

find_space([P,Q|_], Esp, _, Esp) :-
    \+ is_list(P), is_list(Q).% write_ln("1"), write_ln(Esp), write_ln("1").

find_space([P,Q|R], Esp_Aux, H_V, Esp) :-
    is_list(P), \+ is_list(Q), subsumes_term(Esp_Aux, []), 
%    write_ln("2"), write_ln(Esp_Aux), write_ln([P,Q|R]), write_ln("2"), 
    get_soma(P, H_V, S), find_space([Q|R], [S,Q|Esp_Aux], H_V, Esp).

find_space([P,Q|R], Esp_Aux, H_V, Esp) :-
%    write_ln("3"), write_ln(Esp_Aux), write_ln([P,Q|R]), write_ln("3"),
    \+ is_list(P), \+ is_list(Q), \+ subsumes_term(Esp_Aux, []), append(Esp_Aux, [Q], Aux), find_space([Q|R], Aux, H_V, Esp).

find_space([P|R], Esp, _, Esp) :-
%    write_ln("4"), write_ln(Esp), write_ln([P|R]), write_ln("4"),
    \+ is_list(P), R == [].

find_space([_|R], Esp_Aux, H_V, Esp) :-
%    write_ln("5"), write_ln(Esp_Aux), write_ln([P|R]), write_ln("5"),
 %   write_ln(Esp_Aux), write_ln("here51"), 
    subsumes_term(Esp_Aux, []), find_space(R, Esp_Aux, H_V, Esp).



cria_espaco([P|R], Esp) :-
    faz_espaco(P, R, Esp).

nao_lista(Elem) :-
    \+ is_list(Elem).

verificar_sublista(Fila, [_|R], L) :-
    L_1 is L-1, sublist(R, Fila, L_1), maplist(nao_lista(), R).

espaco_fila(Fila, Esp, H_V) :-
    find_space(Fila, [], H_V, Esp_Aux), 
    length(Esp_Aux, L), L > 1, 
    nth1(1, Esp_Aux, S), number(S),
    espaco_fila_aux(Fila, Esp_Aux, Esp, L).
%    verificar_sublista(Fila, Esp_Aux, L),
 %   write_ln("here6"),
  %  cria_espaco(Esp_Aux, Esp),
   % write_ln("here7").
 %   verificar_sublista(Fila, Esp_Aux, L).

espaco_fila_aux(_, Esp_Aux, Esp, _) :-
    cria_espaco(Esp_Aux, Esp).
    %verificar_sublista(Fila, Esp_Aux, L).



%espacos_fila(H_V, Fila, Espacos)
espacos_fila(_, Fila, []) :-
    maplist(is_list(), Fila), !.

espacos_fila(h, Fila, Espacos) :-
    bagof(Esp, espaco_fila(Fila, Esp, h), Espacos).

espacos_fila(v, Fila, Espacos) :-
    bagof(Esp, espaco_fila(Fila, Esp, v), Espacos).

%espacos_fila(_, _, []).


%espacos_puzzle(Puzzle, Espacos)
space(h, Fila, Espacos) :-
    espacos_fila(h, Fila, Espacos).

space(v, Fila, Espacos) :-
    espacos_fila(v, Fila, Espacos).

espacos_puzzle(Puzzle, Espacos) :-
    mat_transposta(Puzzle, Transp), maplist(space(h), Puzzle, H_space), maplist(space(v), Transp, V_space), append(H_space, AH_space), append(V_space, AV_space), 
    append(AH_space, AV_space, Espacos).


%espacos_com_posicoes_comuns(Espacos, Esp, Esps_com)

espacos_com_posicoes_comuns(Espacos, Esp, Esps_com) :-
    espaco_de(Esp, Pos_esp), maplist(find_comum(Espacos, Esp), Pos_esp, Pos_esps), append(Pos_esps, Esps_com).

find_comum(Espacos, Esp, Pos, Pos_Com) :-
    bagof(E, (member(E, Espacos), E \== Esp, espaco_de(E, Esp_L), membro(Pos, Esp_L)), Pos_Com).

membro(X, [P|_]) :- X == P.

membro(X, [_|R]) :- membro(X, R).

%permutacoes_soma_espacos(Espacos, Perms_soma)
permutacoes_soma_espacos(Espacos, Perms_soma) :-
    maplist(find_perms(), Espacos, Perms_soma).

find_perms(Espaco, Esp_Perm) :-
    soma_de(Espaco, Soma), 
    espaco_de(Espaco, Esp), 
    length(Esp, Len), 
    permutacoes_soma(Len, [1,2,3,4,5,6,7,8,9], Soma, Perms),
    include(subsumes_term(Esp), Perms, Perms_filt),
    append([Espaco], [Perms_filt], Esp_Perm).

filter_var(Var1, Var2) :-
    Var1 == Var2.


perms_compativeis([], []).

perms_compativeis([Esp|R1], [Num|R2]) :-
    subsumes_term(Esp, Num), !,
    perms_compativeis(R1, R2).

%permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma)

permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma) :-
    %write_ln("here222"),
    %write_ln(Espacos),
    permutacoes_soma_espacos([Esp], Esp_perm_aux),
    nth1(1, Esp_perm_aux, Esp_perm),
    %write_ln("espaco e perms possiveis"),
    %write_ln(Esp_perm),
    %exclude(membro(Esp_perm), Perms_soma, Perms_soma_aux), 
    nth1(2, Esp_perm, Perms),
    %write_ln("perms possiveis"),
    %write_ln(Perms),
    %exclude(unificavel(Esp), Perms, Perms_Esp),
    %write_ln("Apenas os que sao possiveis de acordo com o que ja la esta"), 
    %write_ln(Perms_Esp),
    espaco_de(Esp, Espaco),
    espacos_com_posicoes_comuns(Espacos, Esp, Esps_com),
    ter(Esps_com, Perms_soma, Perms_soma_com),
    %write_ln("espacos e perms em comum"),
    %write_ln(Perms_soma_com),
    perms_possiveis(Espaco, Perms, Perms_soma_com, Perm).


unificavel(Esp, Perm) :-
    soma_de(Esp, Soma), faz_espaco(Soma, Perm, Esp_Aux), 
   % write_ln("comparacao"),
   % write_ln(Esp),
   % write_ln(Esp_Aux),
    Esp == Esp_Aux.

%perms_possiveis(Esp, Perms_Esp, Espacos, Perms_soma, Perm)
/*
perms_possiveis(_, [], _, _, _).

perms_possiveis(Esp, Espaco, [Perm|_], Espacos, [Perm_soma|_], Perm) :-
    %nth1(1, Espaco, )
    nth1(2, Perm_soma, Outro_perms),
    maplist(in_perms(Espaco, Espacos, Outro_perms, Perm), Perm, Compativel), 
    \+ member([], Compativel).

perms_possiveis(Esp, [_|R], Espacos, Perms_soma, Perm) :-
    perms_possiveis(Esp, R, Espacos, Perms_soma, Perm).
*/

perms_possiveis(Espaco, [Perm|_], Perms_soma_com, Perm) :-
    compativeis(Espaco, Perm, Perms_soma_com).

perms_possiveis(Espaco, [_|R], Perms_soma_com, Perm) :-
    perms_possiveis(Espaco, R, Perms_soma_com, Perm).

/*
compativeis([Var], [Num], [Esp_perm], Compativeis) :-
    nth1(1, Esp_perm_com, Esp), 
    nth1(2, Esp_perm_com, Perms_poss), 
    espaco_de(Esp, Esp_vars),
    search_var_pos(Var, Esp_vars, N),
    bagof(Perm, (member(Perm, Perms_poss), nth1(N, Perm, Num)), Compativeis),
    \+ member([], Compativeis).
*/

compativeis([], [], []).

compativeis([Var|R1], [Num|R2], [Esp_perm_com|R3]) :-
    nth1(1, Esp_perm_com, Esp),
    %write_ln(Esp),
    nth1(2, Esp_perm_com, Perms_poss), 
    %write_ln(Perms_poss),
    espaco_de(Esp, Esp_vars),
    %write_ln(Esp_vars),
    search_var_pos(Var, Esp_vars, N_var),
    %write_ln(N_var),
    nth1(1, N_var, N),
    %write_ln(N),
    bagof(Perm, (member(Perm, Perms_poss), nth1(N, Perm, Num)), _),
    %write_ln(Comp),
    %write_ln("here"),
    compativeis(R1, R2, R3).




in_perms([], _, _, _, _, _, _).

in_perms(Espaco, _, Outro_perms, Perm, Elem, Possiveis) :-
    %search_var_pos(Var, [Var|R], N).
    nth1(N, Perm, Elem),
    nth1(N, Espaco, Elem),
    bagof(Perms, (membro(Perms, Outro_perms), subsumes_term(Espaco, Perms)), Possiveis).


ter(Esps_com, Perms_soma, Perms_soma_com) :-
    bagof(Elem, (membroo(Esps_com, Perms_soma, Elem)), Perms_soma_com).

membroo(Esps_com, Perms_soma, Elem) :-
    member(El, Esps_com), member(Elem, Perms_soma), member(El, Elem).


search_var_pos(Var, Espaco, N) :-
    exclude(filter_var(Var), Espaco, Filt), bagof(X, (nth1(X, Espaco, Var), \+ membro(Var, Filt)), N).


meme(Esp, Perm) :-
    write_ln(Esp),
    write_ln(Perm),
    membro(Esp, Perm),
    write("lllllllllllllllllllllllllllllllllllllooooooooooooooooollllllllllllll").



%permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp, Perms_poss).

permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp, Perms_poss) :-
    espaco_de(Esp, Esp_vars),
    findall(Perm, permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma), L_perms),
    append([Esp_vars], [L_perms], Perms_poss).

%permutacoes_possiveis_espacos(Espacos, Perms_poss_esps)

permutacoes_possiveis_espacos(Espacos, Perms_poss_esps) :-
    permutacoes_soma_espacos(Espacos, Perms_soma),
    maplist(permutacoes_possiveis_espaco(Espacos, Perms_soma), Espacos, Perms_poss_esps).

%numeros_comuns(Lst_Perms, Numeros_comuns)

numeros_comuns(Lst_Perms, Numeros_comuns) :-
    nth1(1, Lst_Perms, Perm),
    length(Perm, N),
    build_list(N, List_Ind),
    findall(Par, (member(Ind, List_Ind), ind_comum(Lst_Perms, Ind, Par)), Numeros_comuns).

ind_comum(Lst_Perms, Ind, Par) :-
    maplist(find_nth(Ind), Lst_Perms, List_El),
    el_equal(List_El, El),
    Par = (Ind, El).

el_equal(List_el, El) :-
    nth1(1, List_el, El),
    exclude(compare(=,El), List_el, List_el_aux),
    length(List_el_aux, N),
    N = 0.

find_nth(Ind, Perm, El) :-
    nth1(Ind, Perm, El).

build_list(Ind, List_Ind) :-
    build_list_aux(Ind, 1, List_Ind).

build_list_aux(Ind, N, []) :-
    N > Ind, !.

build_list_aux(Ind, N, [N|R]) :-
    N_1 is N + 1,
    build_list_aux(Ind, N_1, R).

%atribui_comuns(Lst_Perms, Numeros_comuns)

atribui_comuns(Perms_Possiveis) :-
    maplist(atribui_comuns_aux(), Perms_Possiveis).

atribui_comuns_aux(Esp_Perm) :-
    nth1(1, Esp_Perm, Esp),
    nth1(2, Esp_Perm, Perms),
    numeros_comuns(Perms, Numeros_comuns),
    maplist(atribui_val(Esp), Numeros_comuns).

atribui_val(Esp, Par) :-
    ind_de(Par, Ind),
    val_de(Par, Val),
    nth1(Ind, Esp, Val).

%retira_impossiveis(Perms_Possiveis, Novas_Perms_Possiveis)

retira_impossiveis(Perms_Possiveis, Novas_Perms_Possiveis) :-
    %maplist(make_espaco(), Perms_Possiveis, Espacos),
    %write_ln(Espacos),
    %permutacoes_possiveis_espacos(Espacos, Novas_Perms_Possiveis).
    maplist(find_new_perms(), Perms_Possiveis, Novas_Perms_Possiveis).

find_new_perms(Esp_Perms, Novas_Perms_Possiveis) :-
    nth1(1, Esp_Perms, Esp),
    nth1(2, Esp_Perms, Perms),
    include(subsumes_term(Esp), Perms, Perms_filt),
    append([Esp], [Perms_filt], Novas_Perms_Possiveis).

make_espaco(Esp_Perm, Espaco) :-
    nth1(1, Esp_Perm, Esp),
    nth1(2, Esp_Perm, Perms),
    nth1(1, Perms, Perm),
    sum_list(Perm, Soma),
    faz_espaco(Soma, Esp, Espaco).


%simplifica(Perms_Possiveis, Novas_Perms_Possiveis)

simplifica(Novas_Perms_Possiveis, Novas_Perms_Possiveis) :-
    atribui_comuns(Novas_Perms_Possiveis),
    retira_impossiveis(Novas_Perms_Possiveis, Novas_Perms_Possiveis_aux),
    subsumes_term(Novas_Perms_Possiveis, Novas_Perms_Possiveis_aux), !.

simplifica(Perms_Possiveis, Novas_Perms_Possiveis) :-
    atribui_comuns(Perms_Possiveis),
    retira_impossiveis(Perms_Possiveis, Novas_Perms_Possiveis_aux),
    simplifica(Novas_Perms_Possiveis_aux, Novas_Perms_Possiveis).


%inicializa(Puzzle, Perms_Possiveis)

inicializa(Puzzle, Perms_Possiveis) :-
    espacos_puzzle(Puzzle, Espacos),
    permutacoes_possiveis_espacos(Espacos, Perms_Possiveis_aux),
    simplifica(Perms_Possiveis_aux, Perms_Possiveis), !.


%escolhe_menos_alternativas(Perms_Possiveis, Escolha)

escolhe_menos_alternativas(Perms_Possiveis, Escolha) :-
    maplist(conta_n_perms(), Perms_Possiveis, N_Perms),
    find_min(N_Perms, Min_Perms),
    find_escolha(Perms_Possiveis, N_Perms, Min_Perms, Escolha).

conta_n_perms(Esp_Perms, N_perm) :-
    nth1(2, Esp_Perms, Perms),
    length(Perms, N_perm).

find_min(N_Perms, Min_Perms) :-
    include(compare(<,1), N_Perms, N_Perms_1),
    min_list(N_Perms_1, Min_Perms).

find_escolha([Escolha|_], [Min|_], Min, Escolha) :- !.

find_escolha([_|R_E], [_|R_N], Min, Escolha) :-
    find_escolha(R_E, R_N, Min, Escolha).


%experimenta_perm(Escolha, Perms_Possiveis, Novas_Perms_Possiveis)

experimenta_perm(Escolha, Perms_Possiveis, Novas_Perms_Possiveis) :-
    nth1(1, Escolha, Esp),
    nth1(2, Escolha, Lst_Perms),
    %nth1(N, Perms_Possiveis, Escolha),
    member(Perm, Lst_Perms),
    Esp = Perm,
    substitui_perms_possiveis(Escolha, Perms_Possiveis, Esp, Perm, Novas_Perms_Possiveis).


substitui_perms_possiveis(Escolha, [Escolha|R], Esp, Perm, [[Esp,[Perm]]|R]) :- !.

substitui_perms_possiveis(Escolha, [Esp_Perms|R1], Esp, Perm, [Esp_Perms|R2]) :-
    Escolha \= Esp_Perms,
    substitui_perms_possiveis(Escolha, R1, Esp, Perm, R2).

%resolve_aux(Perms_Possiveis, Novas_Perms_Possiveis)

resolve_aux(Novas_Perms_Possiveis, Novas_Perms_Possiveis) :-
    \+ escolhe_menos_alternativas(Novas_Perms_Possiveis, _).

resolve_aux(Perms_Possiveis, Novas_Perms_Possiveis) :-
    escolhe_menos_alternativas(Perms_Possiveis, Escolha),
    experimenta_perm(Escolha, Perms_Possiveis, Exp_Perms_Possiveis),
    simplifica(Exp_Perms_Possiveis, Novas_Perms_Possiveis_aux),
    resolve_aux(Novas_Perms_Possiveis_aux, Novas_Perms_Possiveis), !.

%resolve(Puz)

resolve(Puz) :-
    inicializa(Puz, Perms_Possiveis),
    resolve_aux(Perms_Possiveis, _).



%permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma)
/*
permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma) :-
    permutacoes_soma_espacos([Esp], Esp_perm_aux),
    nth1(1, Esp_perm_aux, Esp_perm),
    nth1(2, Esp_perm, Perms),
    espacos_com_posicoes_comuns(Espacos, Esp, Esps_com),
    ter(Esps_com, Perms_soma, Perms_soma_com),
    perms_possiveis(Perms, Perms_soma_com, Perm).


unificavel(Esp, Perm) :-
    soma_de(Esp, Soma), faz_espaco(Soma, Perm, Esp_Aux), 
    Esp == Esp_Aux.

perms_possiveis([Perm|_], Perms_soma_com, Perm) :-
    compativeis(Perm, Perms_soma_com).

perms_possiveis([_|R], Perms_soma_com, Perm) :-
    perms_possiveis(R, Perms_soma_com, Perm).

compativeis(_, []).

compativeis([Num|R2], [Esp_perm_com|R3]) :-
    nth1(2, Esp_perm_com, Perms_poss),
    %write_ln(Num),
    %write_ln(Esp_perm_com),
    %write_ln(Perms_poss), 
    num_in_perms(Num, Perms_poss),
    compativeis(R2, R3).

num_in_perms(Num, [Perms|_]) :-
    member(Num, Perms), !.

num_in_perms(Num, [_|R]) :-
    num_in_perms(Num, R).



in_perms([], _, _, _, _, _, _).

in_perms(Espaco, _, Outro_perms, Perm, Elem, Possiveis) :-
    %search_var_pos(Var, [Var|R], N).
    nth1(N, Perm, Elem),
    nth1(N, Espaco, Elem),
    bagof(Perms, (membro(Perms, Outro_perms), subsumes_term(Espaco, Perms)), Possiveis).


ter(Esps_com, Perms_soma, Perms_soma_com) :-
    bagof(Elem, (membroo(Esps_com, Perms_soma, Elem)), Perms_soma_com).

membroo(Esps_com, Perms_soma, Elem) :-
    member(El, Esps_com), member(Elem, Perms_soma), member(El, Elem).


search_var_pos(Var, Espaco, N) :-
    exclude(filter_var(Var), Espaco, Filt), bagof(X, (nth1(X, Espaco, Var), \+ membro(Var, Filt)), N).
*/