:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(terms)).

%:-compile('botti.pl').
%:- compile('threeauthors.pl').

main(N) :-
   main(N,_,_),!.
main(_) :-
    statistics(runtime,[_,Time]),
    write('No solutions: RunTime: '),write(Time),nl.
   
main(N, ACTIONSOCC,STATES):-                               
    setof(F, fluent(F), Lf), setof(A, action(A), La),
	%nl,nl,write('Actions-> '),%nl,nl,nl,write(La), 	
    setof(F, initially(F), Init), setof(F, goal(F), Goal), 
    make_states(N,Lf,STATES),
	%nl,nl,nl,write('Number N -> '),nl,nl,nl,write('States -> '),nl,nl,nl,write(STATES),nl,nl,
    make_action_occurrences(N,La,ACTIONSOCC),%nl,nl,write('ActionsOcc-> '),nl,nl,nl,write(ACTIONSOCC),
    set_initial(Init,STATES),
    %write('Initial State:'),nl, STATES=[S|_],write_state(S),nl,
    set_goal(Goal,STATES), 
    %write('Final State:'),nl, append(_,[Last],STATES),write_state(Last),
    set_transitions(ACTIONSOCC,STATES),
    %write('****transitions set****'),nl, %%dump_result(ACTIONSOCC,STATES),
    %%%
    set_executability(ACTIONSOCC,STATES),    %%% Faster if here !!!!!!!!
    %write('****executability set****'),nl, %%dump_result(ACTIONSOCC,STATES),
    %%%
%%%%%%% write %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    term_variables(ACTIONSOCC,Va), length(Va,LVA),
    term_variables(STATES,Vs),length(Vs,LVS), L is LVA + LVS,
    write('there are '),write(L),write(' variables'),nl,
%   search(AllActions,0,occurrence,indomain_min,dbs(2,bbs(200)),[]),
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    get_all_actions(ACTIONSOCC, AllActions),   
    statistics(runtime,_),
    labeling([ff],AllActions),
    statistics(runtime,[_,Time]),
    write('*************************************************************************'),nl,
    dump_result(ACTIONSOCC,STATES),
    write('Solution found in Time: '),write(Time),nl.
    
    
make_states(0,_,[]):-!.
make_states(N,List,[S|STATES]) :-
    N1 is N-1, make_states(N1,List,STATES),
    make_one_state(List,S).
make_one_state([],[]).
make_one_state([F|Fluents],[fluent(F,VarF)|VarFluents]) :-
    make_one_state(Fluents,VarFluents),
    VarF in 0..1.


make_action_occurrences(1,_,[]):-!.
make_action_occurrences(N,List,[Act|ActionsOcc]) :-
    N1 is N-1, make_action_occurrences(N1,List,ActionsOcc),
    make_one_action_occurrences(List,Act),
    get_action_list(Act,AList),
    sum(AList,#=,1).
make_one_action_occurrences([],[]).
make_one_action_occurrences([A|Actions],[ action(A,OccA)|OccActions]) :-
    make_one_action_occurrences(Actions,OccActions),
    OccA in 0..1.


set_initial(List,[InitialState|_]) :-
    set_state(List,InitialState),
    complete_state(InitialState,InitialState).


complete_state([],_).
complete_state( [fluent(Fluent,EV)| Fluents],InitialState) :-
    ( integer(EV), ! ;
    set_one_static_fluent(Fluent, EV, InitialState) ),
    complete_state(  Fluents ,InitialState).

set_one_static_fluent(Name, EV, State) :-
    findall(Po1, caused(Po1,Name), StatPos),
    findall(Ne1, caused(Ne1,mneg(Name)), StatNeg),
    build_sum_stat(StatPos, State, PStatPos,EV,p),
    build_sum_stat(StatNeg, State, PStatNeg,EV,n),
    sum(PStatPos,#=,PosFired),
    sum(PStatNeg,#=,NegFired),
    PosFired #> 0 #==> EV #=1,
    NegFired #> 0 #==> EV #=0.

set_goal(List,States) :-
    last(States,FinalState),
    set_state(List,FinalState).
set_state([],_).
set_state([Fluent|Rest],State) :-
     (Fluent=mneg(F),!,member(fluent(F,0),State);
      member(fluent(Fluent,1),State)),
     set_state(Rest,State).


set_transitions(_Occurrences,[_States]) :- !.
set_transitions([O|Occurrences],[S1,S2|Rest]) :-
    set_transition(O,S1,S2,S1,S2),
    set_transitions(Occurrences,[S2|Rest]).

set_transition(_Occ,[],[],_,_).
set_transition(Occ,[fluent(Fluent,IV)|R1],[fluent(Fluent,EV)|R2],FromState,ToState):-
    set_one_fluent(Fluent,IV,EV,Occ,FromState,ToState),
    set_transition(Occ, R1, R2,FromState,ToState).


set_one_fluent(Name,IV,EV, Occurrence,FromSt,ToSt) :-
    findall([X,L],causes(X,Name,L),Pos),
    findall([Y,M],causes(Y,mneg(Name),M),Neg),
    build_sum_prod(Pos, Occurrence, FromSt,PFormula,EV,p),
    build_sum_prod(Neg, Occurrence, FromSt,NFormula,EV,n),
    findall(Po1, caused(Po1,Name), StatPos),
    findall(Ne1, caused(Ne1,mneg(Name)), StatNeg),
    build_sum_stat(StatPos, ToSt, PStatPos,EV,p),
    build_sum_stat(StatNeg, ToSt, PStatNeg,EV,n),
    append(PFormula,PStatPos,Positive),
    append(NFormula,PStatNeg,Negative),
    sum(Positive,#=,PosFired),
    sum(Negative,#=,NegFired),
%%% Redundant constraint (it doesn't seem useful)
    PosFired * NegFired #= 0,
    EV #<==> (PosFired + IV - IV * NegFired  #> 0).



build_sum_prod([],_,_, [],_,_).
build_sum_prod([[Name,Prec]|Rest],Occurrence, State,[ Flag |PF1],EV,Mode):-
    get_precondition_vars(Prec,State,ListPV),
    length(Prec,NPrec),
    sum(ListPV,#=,SumPrec),
    member(action(Name,VA),Occurrence),
    (VA #= 1 #/\ (SumPrec #= NPrec)) #<==> Flag,
    %%% Redundant (but useful) constraints:
    (Mode == p -> EV #>= Flag;
     Mode == n -> EV #=< 1 - Flag),
    build_sum_prod(Rest,Occurrence, State,PF1,EV,Mode).


build_sum_stat([],_,[],_,_).
build_sum_stat([ Cond | Others], State, [Flag |Fo],EV,Mode) :-
    get_precondition_vars(Cond,State,List),
    length(List, NL),
    sum(List,#=,Result),
    (Result #= NL) #<==> Flag ,
    %%% Redundant (but useful) constraints:
    (Mode == p -> EV #>= Flag;
     Mode == n -> EV #=< 1 - Flag),
    build_sum_stat(Others,State,Fo,EV,Mode).


set_executability(ActionsOcc,States) :-
    findall([Act,C],executable(Act,C),Conds),
    group_cond(Conds,GroupedConds),
    set_executability1(ActionsOcc,States,GroupedConds).

set_executability1([],[_],_).
set_executability1([AStep|ARest],[State|States],Conds) :-
    set_executability_sub(AStep,State,Conds),
    set_executability1(ARest,States,Conds).

set_executability_sub(_Step,_State, []).
set_executability_sub(Step,State, [[Act,C]|CA]) :-
    member(action(Act,VA),Step),
    get_all_preconditions(C, State, NCs,Temps),
    formula(NCs,Temps,Phi),
    (VA #=1) #==> Phi #= 0,
    set_executability_sub(Step,State,CA).



group_cond([],[]).
group_cond([[Action,C]|R],[[Action,[C|Cs]]|S]) :-
     findall(L,(member([Action,L],R)),Cs),
     filter(Action,R,Others),
     group_cond(Others,S).
filter(_,[],[]).
filter(A,[[A,_]|R],S) :-
      !, filter(A,R,S).
filter(A,[C|R],[C|S]) :-
      !, filter(A,R,S).
get_all_preconditions([],_,[],[]).
get_all_preconditions([C|R],State,[NC|NCs],[Temp|Temps]) :-
      get_precondition_vars(C,State,Cs),
      length(Cs,NC),
      sum(Cs,#=,Temp),
      get_all_preconditions(R,State, NCs,Temps).

formula([],[],1).
formula([N|Rest],[V|Val],Phi) :-
    formula(Rest,Val,Phi1),
    Phi #<==> Phi1 #/\ (N #> V).


%%%%%% AUXILIARY predicates

get_precondition_vars([],_,[]).
get_precondition_vars([P1|Rest],State,[F|LR]) :-
    get_precondition_vars(Rest,State,LR),
    (P1 = mneg(FluentName),!,
     member(fluent(FluentName,A),State), F #= 1-A;
     member(fluent(P1,F),State)).
get_all_actions([],[]).
get_all_actions([A|B],List) :-
    get_action_list(A,List1),
    get_all_actions(B,List2),
    append(List1,List2,List).
get_action_list([],[]).
get_action_list([action(_,V)|Rest],[V|MRest]) :-
    get_action_list(Rest,MRest).

%%% END MAIN CODE %%%%%%%

neq(A,B) :- A \== B.
diff(A,B) :- A \== B.
diff(A,B,C) :- A \== B, A \== C, C \== B.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% PRINT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dump_result(A,S) :-
    state_plan(A),
    nl,nl,
    dump_result1(A,S).

state_plan([]) :- write('stop.'),nl.
state_plan([A|As]) :-
    find_action(A,Name),
    format("~q -> ",[Name]),
    state_plan(As).

dump_result1([],[S]) :-
    write_state(S).
dump_result1([A|B],[S|Rest]) :-
    write_state(S),
    write_action(A),
    dump_result1(B,Rest).

write_state([]) :- nl.
write_state([fluent(Name,Value)|Rest]) :-
    ( var(Value) -> format("~q: unknown  ",[Name]);
%%                Value == 1 ->  format("~q: true   ",[Name]);
%%                format("~q: false   ",[Name])),
                  Value == 1 ->  format("~q  ",[Name]);
                  true ),
    write_state(Rest).

write_action(A) :-
    find_action(A,Name),
    format(" ---->>   ~q ",[Name]),nl.

find_action([],unknown).
find_action([action(Name,Value)|_], Name) :-
    Value == 1,!.
find_action([_|Rest],Name) :-
    find_action(Rest,Name).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    

