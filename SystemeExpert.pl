% donc dans notresysteme 
% on aura un etudiant avec des parametres bientot le salaire ,
% zone code postal , nbr de frere et handicape dans sa famille
% on doit etre capable  d ajouter un etudiant avec Write /READ
% assert is needed 
% la liste des etudiant doit etre en se base de merite 
% on aura une list ou on ajoute de nouveux etudiants  
% etudiant et inserer avec son salaire , zone postal , nbr des frere , handicape dans sa famille 
% leur score est calculer automatiquement et ajouter vers le system 


% Déclaration des prédicats dynamiques - AJOUTEZ CECI AU TOUT DÉBUT DU FICHIER
:- dynamic etudiant/6.

% Définition des constantes
salaire_minimum(10000).
grand_famille(3).
ecole_postal(90000).

%Définir une structure pour représenter un étudiant
etudiant(Nom, Revenu, CodePostal, NbFreres, Handicap, Score).

%Créer des règles pour calculer le score selon les critères donnés

%regle 1 : salaire 
points_salaire(Revenu, Points) :- 
    salaire_minimum(Min),
    (Revenu =< Min -> Points = 2 ;
     Revenu =< 2*Min -> Points = 1 ;
     Points = 0).

%regle 2 zone postal 
points_postal(CodePostal, Points) :- 
    ecole_postal(Code),
    (CodePostal = Code -> Points = 3 ;
     CodePostal =Code+1 -> Points = 2 ;
     CodePostal =Code-1 -> Points = 2;
     Points = 0).

%regle 3 nbr de frere et soeur
points_famille(NbFreres, Points) :- 
    grand_famille(Seuil),
    (NbFreres > Seuil -> Points = 1.5 ;
     Points = 0).

%regle 4  des handicape
points_handicap(true, 1.5 ).
points_handicap(false, 0 ).

% regle 5 calcule de score 
calculer_score(Revenu, CodePostal, NbFreres, Handicap, Score) :-
    points_salaire(Revenu, S1),
    points_postal(CodePostal, S2),
    points_famille(NbFreres, S3),
    points_handicap(Handicap, S4),
    Score is S1 + S2 + S3 + S4.


%Implémenter l interface utilisateur pour saisir les informations

% Interface pour ajouter un nouvel étudiant
ajouter_etudiant :-
    write('Entrez le nom de l\'étudiant: '),
    read(Nom),
    write('Entrez le revenu annuel de la famille: '),
    read(Revenu),
    write('Entrez le code postal du domicile: '),
    read(CodePostal),
    write('Entrez le nombre de frères/soeurs inscrits au centre: '),
    read(NbFreres),
    write('Y a-t-il un handicap dans la famille? (true/false): '),
    read(Handicap),
    
    % Calcul du score
    calculer_score(Revenu, CodePostal, NbFreres, Handicap, Score),
    
    % Ajouter à la base de connaissances avec assert
    assert(etudiant(Nom, Revenu, CodePostal, NbFreres, Handicap, Score)),
    
    write('Étudiant ajouté avec succès. Score: '),
    write(Score), nl,
    write('Voulez-vous ajouter un autre étudiant? (oui/non): '),
    read(Reponse),
    (Reponse = oui -> ajouter_etudiant ; true).

afficher_liste_merite :-
    findall(etudiant(Nom, Revenu, CodePostal, NbFreres, Handicap, Score)-Score, 
            etudiant(Nom, Revenu, CodePostal, NbFreres, Handicap, Score), 
            ListeEtudiants),
    sort(2, @>=, ListeEtudiants, ListeTriee),
    nl, write('=== LISTE DES ÉTUDIANTS PAR ORDRE DE MÉRITE ==='), nl,
    write('---------------------------------------------'), nl,
    afficher_liste_detaillee(ListeTriee),
    length(ListeTriee, NbEtudiants),
    write('---------------------------------------------'), nl,
    write('Nombre total d\'étudiants: '), write(NbEtudiants), nl.

% Prédicat auxiliaire pour afficher les détails
afficher_liste_detaillee([]) :- nl.
afficher_liste_detaillee([etudiant(Nom, Revenu, CodePostal, NbFreres, Handicap, Score)-_|Reste]) :-
    write('Nom: '), write(Nom), nl,
    write('  Score: '), write(Score), nl,
    write('  Revenu: '), write(Revenu), nl,
    write('  Code Postal: '), write(CodePostal), nl,
    write('  Nombre de frères/sœurs: '), write(NbFreres), nl,
    write('  Handicap: '), write(Handicap), nl,
    write('---------------------------------------------'), nl,
    afficher_liste_detaillee(Reste).
