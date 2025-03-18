
% D�claration des pr�dicats dynamiques
:- dynamic etudiant/6.

% D�finition des constantes
salaire_minimum(10000).
grand_famille(3).
ecole_postal(90000).

% D�finir une structure pour repr�senter un �tudiant
etudiant(Nom, Revenu, CodePostal, NbFreres, Handicap, Score).

% R�gle 1: salaire 
points_salaire(Revenu, Points) :- 
    salaire_minimum(Min),
    (Revenu =< Min -> Points = 2 ;
     Revenu =< 2*Min -> Points = 1 ;
     Points = 0).

% R�gle 2: zone postal 
points_postal(CodePostal, Points) :- 
    ecole_postal(Code),
    (CodePostal = Code -> Points = 3 ;
     CodePostal =:= Code+1 -> Points = 2 ;
     CodePostal =:= Code-1 -> Points = 2;
     Points = 0).

% R�gle 3: nbr de fr�re et soeur
points_famille(NbFreres, Points) :- 
    grand_famille(Seuil),
    (NbFreres > Seuil -> Points = 1.5 ;
     Points = 0).

% R�gle 4: des handicap
points_handicap(true, 1.5).
points_handicap(false, 0).

% R�gle 5: calcul de score 
calculer_score(Revenu, CodePostal, NbFreres, Handicap, Score) :-
    points_salaire(Revenu, S1),
    points_postal(CodePostal, S2),
    points_famille(NbFreres, S3),
    points_handicap(Handicap, S4),
    Score is S1 + S2 + S3 + S4.

% Ajouter un �tudiant
ajouter_etudiant(Nom, Revenu, CodePostal, NbFreres, Handicap) :-
    calculer_score(Revenu, CodePostal, NbFreres, Handicap, Score),
    assertz(etudiant(Nom, Revenu, CodePostal, NbFreres, Handicap, Score)).

% Supprimer un �tudiant
supprimer_etudiant(Nom) :-
    retract(etudiant(Nom, _, _, _, _, _)).

% R�cup�rer tous les �tudiants
get_all_students(Students) :-
    findall(etudiant(Nom, Revenu, CodePostal, NbFreres, Handicap, Score),
            etudiant(Nom, Revenu, CodePostal, NbFreres, Handicap, Score),
            Students).

% Obtenir la liste tri�e
get_sorted_students(SortedStudents) :-
    findall(etudiant(Nom, Revenu, CodePostal, NbFreres, Handicap, Score)-Score,
            etudiant(Nom, Revenu, CodePostal, NbFreres, Handicap, Score),
            Students),
    sort(2, @>=, Students, SortedStudents).

% Sauvegarder les donn�es dans un fichier
save_data :-
    tell('student_data.pl'),
    listing(etudiant/6),
    told.

% Charger les donn�es depuis un fichier
load_data :-
    [student_data].
        