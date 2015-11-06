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





% path(From,To,Path) :- path_1(From,To,[],Path).
% path_1(To,To,_,[To]).
% path_1(From,To,Visited,[From|Path]):- place(Place),portal(Portal),
%								    not(member(Place,Visited)),connecting(From,Place,Portal),
%									path_1(Place,To,[Place|Visited],Path).
%	

path(Robot,From,To):- path3(Robot,From,To,_,_,_).								
path3(Robot,From,To,Path,Door,L) :- path_2(Robot,From,To,[],[],L1,Path,Door),length(Path,L1), L is L1 - 1.
path_2(_,To,To,_,_,_,[To],[]).
path_2(Robot,From,To,PVisited,DVisited,L,[From|Path],[Portal|Door]):- place(Place),
									((ground_robot(Robot),door(Portal)) ; (flying_robot(Robot),(door(Portal);window(Portal)))),
									not(blocked(Portal)),
								    not(member(Place,PVisited)),connecting(From,Place,Portal),not(member(Portal,DVisited)),
									path_2(Robot,Place,To,[Place|PVisited],[Portal|DVisited],L,Path,Door).








% path(PSTART,PSTART,_,_,_,_,_,0):-!,fail.
% path(PSTART,PEND,[PEND],[PORTAL],[PEND],[PORTAL],INITL,L) :- portal(PORTAL),
%															 (connecting(PSTART,PEND,PORTAL);connecting(PEND,PSTART,PORTAL)),L is INITL+1,!,fail.
% path(PSTART,PEND,VPLACES,VCONNS,PLACES,CONNECTIONS,INITL,L) :- place(PLACE),portal(PORTAL),dif(PSTART,PLACE),
%										  \+ member(PLACE,VPLACES),
%										  \+ member(PORTAL,VCONNS),
%										  (connecting(PSTART,PLACE,PORTAL);connecting(PLACE,PSTART,PORTAL)),
%										  L1 is INITL + 1,
%										  path(PLACE,PEND,[PLACE|VPLACES],[PORTAL|VCONNS],PLACES,CONNECTIONS,L1,L).
% path(_,_,PLACES,CONNECTIONS,PLACES,CONNECTIONS,L,L).


fluent(in(Entity,Place)):- (ground_robot(Entity);flying_robot(Entity);victim(Entity)),place(Place).						
fluent(saved(Victim)):-victim(Victim).
						
action(move(Robot,Place)):-ground_robot(Robot),place(Place).
action(fly(Robot,Place)):-flying_robot(Robot),place(Place).

causes(move(Robot,Place),in(Robot,Place),[] ):- ground_robot(Robot),place(Place).
causes(move(Robot,Place),saved(Victim),[in(Victim,Place)] ):- ground_robot(Robot),victim(Victim),place(Place).
causes(fly(Robot,Place),in(Robot,Place),[] ):- flying_robot(Robot),place(Place).
causes(fly(Robot,Place),saved(Victim),[in(Victim,Place)] ):- flying_robot(Robot),victim(Victim),place(Place).

caused([in(Entity,Place1)],mneg(in(Entity,Place2))):- (ground_robot(Entity);flying_robot(Entity);victim(Entity)),place(Place1),place(Place2),diff(Place1,Place2).
caused([in(Robot1,Place)],mneg(in(Robot2,Place))):-(ground_robot(Robot1);flying_robot(Robot1)),(ground_robot(Robot2);flying_robot(Robot2)),place(Place),diff(Robot1,Robot2).

executable(move(Robot,Place2),[in(Robot,Place1),mneg(in(Robot,Place2))]):-
ground_robot(Robot),place(Place1),place(Place2),diff(Place1,Place2),path(Robot,Place1,Place2).

executable(fly(Robot,Place2),[in(Robot,Place1),mneg(in(Robot,Place2))]):-
flying_robot(Robot),place(Place1),place(Place2),diff(Place1,Place2),path(Robot,Place1,Place2).

initially(in(ugv1,room1)).
initially(in(ugv2,room1)).
initially(in(uav1,room1)).
initially(in(victim1,room6)).
initially(in(victim2,room5)).
initially(in(victim3,room4)).
initially(mneg(saved(Victim))):-victim(Victim).

goal(mneg(in(ugv1,room1))).
goal(mneg(in(ugv2,room1))).
goal(mneg(in(uav1,room1))).

goal(saved(victim1)).
goal(saved(victim2)).
goal(saved(victim3)).

