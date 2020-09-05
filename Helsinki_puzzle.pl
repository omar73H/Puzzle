grid_build(N,M):-
				length(M,N),
				build(N,M).
build(_,[]).
build(N,[H|T]):-
				length(H,N),
				build(N,T).
				
				
				
notmember(X,L):-
				\+member(X,L).
				
notmember2(_,[]).					
notmember2(X,[H|T]):-
				X\==H,
				notmember2(X,T).
					
					
					
acceptable_permutation(L,R):-
					acceptable_permutation(L,R,L,[]).
acceptable_permutation([],[],_,_).
acceptable_permutation([H|T],[HP|TP],L,Acc):-
						member(HP,L),
						HP\==H,
						notmember2(HP,Acc),
						acceptable_permutation(T,TP,L,[HP|Acc]).
					
	
grid_gen(N,M):-
                 grid_build(N,M),
				 trans(M,MT),
				 acceptable_permutation(M,MT),flatten(M,MF),
				 sort(MF,Variables),num_gen(1,N,ValidRange),assign(Variables,ValidRange),
				 distinct_rows(M),max_list(Variables,Max),traverse1D(Max,Variables).

assign([],_).
assign([H|T],Range):-
					member(H,Range),
					assign(T,Range).

% we make the common cases fast
					
trans([],[]).
trans([[A]],[[A]]).
trans([[A,B],[C,D]],[[A,C],[B,D]]).
trans([[A,B,C],[D,E,F],[G,H,I]],[[A,D,G],[B,E,H],[C,F,I]]).
trans([[A,B,C,D],[E,F,G,H],[I,J,K,L],[M,N,O,P]],[[A,E,I,M],[B,F,J,N],[C,G,K,O],[D,H,L,P]]).
trans([[A,B,C,D,E],[F,G,H,I,J],[K,L,M,N,O],[P,Q,R,S,T],[U,V,W,X,Y]],[[A,F,K,P,U],[B,G,L,Q,V],[C,H,M,R,W],[D,I,N,S,X],[E,J,O,T,Y]]).

% the general case is a bit slower O(n) instead of O(1) for the above common cases

trans([F|Fs], Ts) :-
	size([F|Fs],N),
	N>5,
    transpose(F, [F|Fs], Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).


size([],0).
size([_|T],S):-
				size(T,S1),
				S is S1+1.
				
				
num_gen(F,F,[F]).
num_gen(F,L,R):-
				F<L,
				F1 is F+1,
				num_gen(F1,L,R1),
				R=[F|R1].		

check_num_grid([]).
check_num_grid(G):-
				  size(G,N),
                  flatten(G,G_1D),
                  max_list(G_1D,X),
				  X =< N,
				  traverse1D(X,G_1D).

traverse1D(1,_).
traverse1D(X,G):-
                     X>1,
                     X1 is X-1,
					 \+notmember(X1,G),
					 traverse1D(X1,G).
					 				
				
				
acceptable_distribution(G):-
				trans(G,Ts),
				check_list_oflist(G,Ts).
				
check_list_oflist([],[]).
check_list_oflist([F1|R1],[F2|R2]):-
			    F1 \== F2,
				check_list_oflist(R1,R2).
				
				
				
row_col_match(G):-
						trans(G,Gt),
						match(G,Gt).

match([],_).
match([H|T],Gt):-
				member(H,Gt),
				match(T,Gt).
	
distinct_rows([]).
distinct_rows([H|T]):-
					\+member(H,T),
					distinct_rows(T).
						
distinct_columns(G):-
					trans(G,Gt),
					distinct_rows(Gt).
						
						
						
helsinki(N,M):-
				grid_gen(N,M).











% predicts used by us for tracing only and to confirm that we generate all correct answers

count_solutions(N,ResultsCount):-
							setof(M,helsinki(N,M),L),
							size(L,ResultsCount).


check1(N,C):-
			setof(M,helsinki(N,M),L),
			all_true(L),
			size(L,C).

all_true([]).
all_true([H|T]):-
				distinct_rows(H),
				distinct_columns(H),
				row_col_match(H),
				acceptable_distribution(H),
				check_num_grid(H),
				trans(H,HT),
				acceptable_permutation(H,HT),
				all_true(T).
					
					
					
					
					
					