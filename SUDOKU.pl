:- include('SUDOKU').
%Pedro Custodio n86496


%tira_num_aux significa que N_Puz e o puzzle resultante
%de tirar o numero Num da posicao Pos do puzzle Puz

tira_num_aux(Num,Puz,Pos,N_Puz) :- 
		puzzle_ref(Puz,Pos,Cont),
		member(Num,Cont),
		not(length(Cont,1)),
		subtract(Cont,[Num],List),
		puzzle_muda_propaga(Puz, Pos, List, N_Puz),!.
tira_num_aux(_,Puz,_,Puz).

%tira_num significa que N_Puz e o puzzle resultante de
%tirar o numero Num de todas as posicoes em Posicoes do
%puzzle Puz

tira_num(Num,Puz,Posicoes,N_Puz) :-
		percorre_muda_Puz(Puz,tira_num_aux(Num),Posicoes,N_Puz).

%puzzle_muda_propaga faz o mesmo que o predicado puzzle_muda,
%mas, no caso de Cont ser uma lista unitaria, propaga a 
%mudanca, isto e, retira o numero em Cont de todas as 
%posicoes relacionadas com Pos,isto e, todas as posicoes na
%mesma linha,coluna ou bloco

puzzle_muda_propaga(Puz,Pos,Cont,N_Puz) :-
		length(Cont,1),!,
		posicoes_relacionadas(Pos,Posicoes),
		member(Num1,Cont),
		puzzle_muda(Puz,Pos,[Num1],N_Puz2),
		tira_num(Num1,N_Puz2,Posicoes,N_Puz).

puzzle_muda_propaga(Puz,Pos,Cont,N_Puz) :-
		puzzle_muda(Puz,Pos,Cont,N_Puz).

%possibilidades_aux afirma que a lista Conteudos e a lista L (numeros) tem os elementos Poss diferentes

possibilidades_aux([],Poss,Poss).
possibilidades_aux([P|R],L,Poss) :-
		length(P,1),
		subtract(L,P,L2),
		possibilidades_aux(R,L2,Poss).
possibilidades_aux([_|R],L,Poss) :- possibilidades_aux(R,L,Poss).

%possibilidades significa que Poss e a lista de numeros
%possiveis para a posicao Pos, do puzzle Puz.

possibilidades(Pos,Puz,Poss) :-
		numeros(L),
		posicoes_relacionadas(Pos,Posicoes),
		conteudos_posicoes(Puz,Posicoes,Conteudos),
		possibilidades_aux(Conteudos,L,Poss),!.

%inicializa_aux significa que N_Puz e o puzzle resultante
%de colocar na posicao Pos do puzzle Puz a lista com os
%numeros possiveis para essa posicao. Caso o conteudo da
%posicao Pos do puzzle Po

inicializa_aux(Puz,Pos,N_Puz) :-
		puzzle_ref(Puz,Pos,Cont),
		length(Cont,N),
		N \= 1,
		possibilidades(Pos,Puz,Poss),
		puzzle_muda_propaga(Puz,Pos,Poss,N_Puz),!.
inicializa_aux(Puz,_,Puz).

%inicializa significa que N_Puz  o puzzle resultante
%de inicializar o puzzle Puz.

inicializa(Puz,N_Puz) :-
		todas_posicoes(Todas_Posicoes),
		percorre_muda_Puz(Puz,inicializa_aux,Todas_Posicoes,N_Puz).

%occ_1 afirma que o numero Num ocorre apenas 
%uma vez na lista dada (comecando o contador a Contador)

occ_1([],_,1,1).
occ_1([P|R],Num,Contador,Res) :-
		member(Num,P),!,
		Contador1 is Contador+1,
		occ_1(R,Num,Contador1,Res).
occ_1([P|R],Num,Contador,Res) :- 
		not(member(Num,P)),
		occ_1(R,Num,Contador,Res).

%devolve_pos afirma que a primeira posicao da lista de 
%conteudos em que o numero Num aparece e Pos_Num

devolve_pos(Num,[_|R],[H|T],Pos_Num) :-
		not(member(Num,H)),
		devolve_pos(Num,R,T,Pos_Num),!.
devolve_pos(_,[P|_],_,P).

%so_aparece_uma_vez afirma que o numero Num so aparece uma vez na
%lista de posicoes Posicoes e que aparece na posicao Pos_Num

so_aparece_uma_vez(Puz,Num,Posicoes,Pos_Num) :- 
		conteudos_posicoes(Puz,Posicoes,Conteudos),
		occ_1(Conteudos,Num,0,Res),
		Res == 1,
		devolve_pos(Num,Posicoes,Conteudos,Pos_Num).

%inspecciona_num significa que N_Puz e o resultado de
%inspeccionar o grupo cujas posicoes sao Posicoes , para
%para o numero Num

inspecciona_num(Posicoes,Puz,Num,N_Puz) :- 
		so_aparece_uma_vez(Puz,Num,Posicoes,Pos_Num),
		puzzle_ref(Puz,Pos_Num,Cont),
		not(length(Cont,1)),
		puzzle_muda_propaga(Puz,Pos_Num,[Num],N_Puz).
inspecciona_num(_,Puz,_,Puz).


%inspecciona_grupo inspecciona o grupo cujas posicoes sao as
%posicoes da lista Gr, do puzzle Puz para cada um dos numeros
%possiveis, sendo o resultado o puzzle N_Puz

inspecciona_grupo(Puz,Gr,N_Puz):-
		numeros(L),
		inspecciona_grupo_aux(Puz,Gr,L,N_Puz).

%inspecciona_grupo_aux afirma que no puzzle Puz, no grupo Gr

inspecciona_grupo_aux(N_Puz,_,[],N_Puz).
inspecciona_grupo_aux(Puz,Posicoes,[P|R],N_Puz):-
		inspecciona_num(Posicoes,Puz,P,N_Puz2),
		inspecciona_grupo_aux(N_Puz2,Posicoes,R,N_Puz).

%inspecciona inspecciona cada um dos grupos do puzzle Puz,
%para cada um dos numeros possiveis, sendo o resultado o 
%puzzle N_Puz

inspecciona(Puz,N_Puz) :- 
		grupos(Gr),
		percorre_muda_Puz(Puz,inspecciona_grupo,Gr,N_Puz).

%grupo_correcto significa que em que Puz e um puzzle, significa que o grupo
%de Puz cujas posicoes sao da lista Gr esta correcto, isto e,
%que contem todos os numeros Nums,sem repeticoes

grupo_correcto(Puz,Nums,Gr) :-
	conteudos_posicoes(Puz,Gr,Conteudos),
	grupo_correcto_aux(Nums,Conteudos).

%grupo_correcto_aux afirma que os numeros Nums aparecem so uma vez na lista Conteudos

grupo_correcto_aux([],_).
grupo_correcto_aux([P|R],Conteudos) :-
		occ_1(Conteudos,P,0,Res),
		Res == 1,
		grupo_correcto_aux(R,Conteudos).

%solucao significa que o puzzle Puz e uma solucao, isto e,
%que todos os seus grupos contem todos os numeros possiveis,
%sem repeticoes

solucao(Puz) :-
		grupos(Gr),
		numeros(L),
		solucao_aux(Puz,Gr,L).

%solucao_aux afirma que os numeros L so
%aparecem uma vez nos grupos Gr do puzzle Puz

solucao_aux(_,[],_).
solucao_aux(Puz,[P|R],L) :-
		grupo_correcto(Puz,L,P),
		solucao_aux(Puz,R,L),!.
%resolve significa que o puzzle Sol e um/a solucao do puzzle
%Puz, comecando por inicializar e inspeccionar Puz e so dps 
%procurar um/a solucao

resolve(Puz,Sol) :- 
		inicializa(Puz,N_Puz),
		inspecciona(N_Puz,N_Puz2),
		procura_sol(N_Puz2,Sol),!.

%escolhe_pos afirma que Pos e a primeira posicao nao unitaria do puzzle Puz

escolhe_pos(Puz,Pos) :-
		todas_posicoes(Todas_Posicoes),
		escolhe_pos_aux(Puz,Todas_Posicoes,Pos), !.

% escolhe_pos_aux afirma que a primeira posicao nao unitaria de uma lista de
%posicoes do puzzle Puz e Pos 

escolhe_pos_aux(Puz,[P|_],Pos) :- 
		puzzle_ref(Puz,Pos,Cont),
		not(length(Cont,1)),
		Pos = P,!.
escolhe_pos_aux(Puz,[_|R],Pos) :-
		escolhe_pos_aux(Puz,R,Pos).

%procura_sol afirma que Sol e a solucao do puzzle Puz

procura_sol(Puz,Sol) :-
		solucao(Puz),
		Sol = Puz,!.
procura_sol(Puz,Sol) :-
		escolhe_pos(Puz,Pos),
		puzzle_ref(Puz,Pos,Cont),
		atribui(Puz,Sol,Cont,Pos).

%atribui afirma que o puzzle Sol e o resultado de
%atribuir o(s) elemento(s) de Cont ao puzzle Puz

atribui(Puz,Sol,[P|_],Pos) :- 
		puzzle_muda_propaga(Puz,Pos,[P],N_Puz),
		procura_sol(N_Puz,Sol).

atribui(Puz,Sol,[_|R],Pos) :- 
		atribui(Puz,Sol,R,Pos).