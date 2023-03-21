% Knowledge Base
friends(laura, michael).
friends(laura, bob).
friends(laura, mary).

friends(michael, laura).
friends(michael, sarah).
friends(michael, mary).

friends(bob, peter).

woman(laura).
woman(mary).
woman(sarah).

man(michael).
man(bob).
man(peter).

% Horn Clause
lucky_man(X) :- man(X), woman(Y), friends(X, Y).

unhappy_woman(X) :- woman(X), friends(X, Y), man(Y).

happy_woman(X) :- woman(X), not(unhappy_woman(X)), friends(X, _).

joint_friend(A, C) :- friends(A, B), friends(B, C).

% Query
?- lucky_man(X)
?- happy_woman(X)

