ground_robot(ugv1).
ground_robot(ugv2).

flying_robot(uav1).

victim(victim1).
victim(victim2).
victim(victim3).

place(courtyard1).
place(room1).
place(room2).
place(room3).
place(room4).
place(room5).
place(room6).
place(corridor1).

door(door0).
door(door1).
door(door2).
door(door3).
door(door4).
door(door5).
door(door6).
door(door7).
door(door8).
window(window1).
window(window2).

blocked(door2).
blocked(door3).
blocked(door4).

connecting(courtyard1,room1,door0).
connecting(room1,courtyard1,door0).

connecting(room1,corridor1,door1).
connecting(corridor1,room1,door1).

connecting(room2,corridor1,door2).
connecting(corridor1,room2,door2).

connecting(room3,corridor1,door3).
connecting(corridor1,room3,door3).

connecting(room4,corridor1,door4).
connecting(corridor1,room4,door4).

connecting(room5,corridor1,door5).
connecting(corridor1,room5,door5).

connecting(room5,room6,door6).
connecting(room6,room5,door6).

connecting(room6,corridor1,door7).
connecting(corridor1,room6,door7).

connecting(room2,room3,door8).
connecting(room3,room2,door8).

connecting(courtyard1,room5,window2).
connecting(room5,courtyard1,window2).

connecting(courtyard1,room4,window1).
connecting(room4,courtyard1,window1).

path(Robot,From,To):- path3(Robot,From,To,_,_,_).								
path3(Robot,From,To,Path,Door,L) :- path_2(Robot,From,To,[],[],L1,Path,Door),length(Path,L1), L is L1 - 1.
path_2(Robot,From,To,PVisited,DVisited,L,[From|Path],[Portal|Door]):- place(Place),
									((ground_robot(Robot),door(Portal)) ; (flying_robot(Robot),(door(Portal);window(Portal)))),
									not(blocked(Portal)),
								    not(member(Place,PVisited)),connecting(From,Place,Portal),not(member(Portal,DVisited)),
									path_2(Robot,Place,To,[Place|PVisited],[Portal|DVisited],L,Path,Door).
path_2(_,To,To,_,_,_,[To],[]).

% Actions specification
action(move(Agent,From,To)):-ground_robot(Agent),place(From),place(To),diff(From,To).
action(fly(Agent,From,To)):-flying_robot(Agent),place(From),place(To),diff(From,To).

% Fluents specification
fluent(in(Agent,In)):- (ground_robot(Agent);flying_robot(Agent);victim(Agent)),place(In).
fluent(saved(Victim)):-victim(Victim).

% Causes formulae 
causes(move(Agent,From,To),in(Agent,To),[]):-	ground_robot(Agent),place(From),place(To),diff(From,To).
causes(move(Agent,From,To),mneg(in(Agent,From)),[]):-	ground_robot(Agent),place(From),place(To),diff(From,To).

causes(fly(Agent,From,To),in(Agent,To),[]):-	flying_robot(Agent),place(From),place(To),diff(From,To).
causes(fly(Agent,From,To),mneg(in(Agent,From)),[]):-	flying_robot(Agent),place(From),place(To),diff(From,To).

% Caused formulae
caused([in(Agent,To)],mneg(in(Agent,From))):- (ground_robot(Agent);flying_robot(Agent)),place(From),place(To),diff(From,To).
caused([in(Agent,To),in(Victim,To)],saved(Victim)):- (ground_robot(Agent);flying_robot(Agent)),victim(Victim),place(To).

% Action precondition
executable(move(Agent,From,To),[in(Agent,From)]):- ground_robot(Agent),place(From),place(To),diff(From,To).
executable(fly(Agent,From,To),[in(Agent,From)]):- flying_robot(Agent),place(From),place(To),diff(From,To).

% Init State
%initially_p(in(ugv1,room1)).
%initially_p(in(ugv2,room1)).
%initially_p(in(uav1,room1)).
%initially_p(in(victim1,room6)).
%initially_p(in(victim2,room5)).
%initially_p(in(victim3,room4)).

%initially(F) :- fluent(F), initially_p(F).
%initially(mneg(F)) :- fluent(F), \+initially_p(F).

initially(in(ugv1,room1)).
initially(in(ugv2,room1)).
initially(in(uav1,room1)).
initially(in(victim1,room6)).
initially(in(victim2,room5)).
initially(in(victim3,room4)).
initially(mneg(saved(Victim))):-victim(Victim).

% Goal State
goal(saved(Victim)):-victim(Victim).
