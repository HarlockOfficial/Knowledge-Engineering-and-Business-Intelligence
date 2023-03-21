person(knut).
class(kebi).
%       <___>,<class>
attends(john, kebi).
student(X) :- attends(X, Y), class(Y).
?- student(john)

%     <person>,<class>
teach(knut, kebi).
teacher(X) :- teach(X, Y), class(Y).
?- teacher(knut)

