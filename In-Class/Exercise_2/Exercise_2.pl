person(knut).
class(kebi).
class(Y) :- teach(X, Y), teacher(X)
%      <person>,<class>
attends(john, kebi).
student(X) :- attends(X, Y), class(Y).
?- student(john)

teach(knut, kebi).
teacher(X) :- teach(X, Y), class(Y).
?- teacher(knut)

