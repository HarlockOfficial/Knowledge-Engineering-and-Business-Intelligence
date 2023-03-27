valid_bachelor(information_systems).
valid_bachelor(business_administration).
valid_bachelor(information_technology).

good_grade(a).
good_grade(b).

accdreditated(yes).

citizenship(swiss).
citizenship(european).

valid_residence(switzerland).
valid_residence(europe).

valid_months_professional_experience(X) :- number(X), X >= 6.

eligible(BACHELOR, ACCREDITATION) :- 
    not(BACHELOR = none),
    accreditated(ACCREDITATION).

acceptance(BACHELOR, ACCREDITATION, GRADE, MONTHS_PROFESSIONAL_EXPERIENCE):-
    eligible(BACHELOR, ACCREDITATION),
    valid_bachelor(BACHELOR),    
    good_grade(GRADE),
    valid_months_professional_experience(MONTHS_PROFESSIONAL_EXPERIENCE),

tuition_fee(BACHELOR, ACCREDITATION, GRADE, MONTHS_PROFESSIONAL_EXPERIENCE, CITIZENSHIP, RESIDENCE, 700) :-
    acceptance(BACHELOR, ACCREDITATION, GRADE, MONTHS_PROFESSIONAL_EXPERIENCE),
    citizenship(CITIZENSHIP).

tuition_fee(BACHELOR, ACCREDITATION, GRADE, MONTHS_PROFESSIONAL_EXPERIENCE, CITIZENSHIP, RESIDENCE, 700) :-
    acceptance(BACHELOR, ACCREDITATION, GRADE, MONTHS_PROFESSIONAL_EXPERIENCE),
    valid_residence(RESIDENCE).

tuition_fee(BACHELOR, ACCREDITATION, GRADE, MONTHS_PROFESSIONAL_EXPERIENCE, CITIZENSHIP, RESIDENCE, 7500) :-
    acceptance(BACHELOR, GRADE, MONTHS_PROFESSIONAL_EXPERIENCE).

