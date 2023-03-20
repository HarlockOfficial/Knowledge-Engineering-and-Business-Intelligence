% Knowledge Base
person(john).
person(peter).
person(mary).
university(fhnw).
%            <person>,<university>
matriculated(john, fhnw).

% Horn clause
student(X) :- person(X), matriculated(X, Y), university(Y).

% Queries
?- student(john)
?- student(peter)

