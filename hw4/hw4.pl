% computes the sum of the list by adding the head to the cdr for sum
sum(0, []).
sum(Sum, [X|Y]) :-
   sum(Other, Y ),
   Sum is Other + X.

% finds the min checks the head to the next element
% if the head is greater use the other element
% if the head is less or equal use head
	
%Usage: min(X, L).
min(Min, [Min]). 
min(X, [H,O|T]) :-
    H > O,                              
    min(X,[O|T]).  
          
min(X,[H,O|T]) :-
    H =< O,                            
    min(X,[H|T]).               

% ! is used to excape early if empty
% recursively calls list with min and next of list
% uses choose to return list with 
% of first list with element removed then matching the second list

min-sort([],[]):- !.
min-sort(L,[M|LX]):-
min(M,L),
choose(M,L,LM),
min-sort(LM,LX).


choose(A, [A|B], B).
    choose(A, [B, C|D], [B|E]) :-
	    choose(A, [C|D], E).


sentence(X):- sentence(X,Z).
sentence(X,Z):-  nounphrase(X,Y),  verbphrase(Y,Z). 
 art([the|W],W). 
   art([a|W],W). 
    
   noun([girl|W],W). 
   noun([dog|W],W). 
    
   verb([sees|W],W).
   verb([pets|W],W).
    
   nounphrase(X,Z):-  art(X,Y),  noun(Y,Z). 
    
   verbphrase(X,Z):-    verb(X,Y),  nounphrase(Y,Z). 
    
 
    
  


% Can you use the same program to generate all possible sentences that can be
% derived from the grammar? 
% Sentence( X) should be able to give all sentences created by the grammer
% Does the order of the subgoals in your rules make a
% difference?
% The Order of the Subgoals does not matter
% problem 1 1 hour problem 2 1 hour
% problem 3 1 hour problem 4 2 hours
