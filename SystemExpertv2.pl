% Load the XPCE library
:- use_module(library(pce)).

% Dynamic predicates (keep these from your original code)
:- dynamic etudiant/6.
:- dynamic edge/2.

% Constants (keep these from your original code)
salaire_minimum(10000).
grand_famille(3).
ecole_postal(90000).

% Student definition and scoring rules (kept from original)
etudiant(Nom, Revenu, CodePostal, NbFreres, Handicap, Score).

points_salaire(Revenu, Points) :- 
    salaire_minimum(Min),
    (Revenu =< Min -> Points = 2 ;
     Revenu =< 2*Min -> Points = 1 ;
     Points = 0).

points_postal(CodePostal, Points) :- 
    ecole_postal(Code),
    (CodePostal = Code -> Points = 3 ;
     CodePostal =:= Code+1 -> Points = 2 ;
     CodePostal =:= Code-1 -> Points = 2;
     Points = 0).

points_famille(NbFreres, Points) :- 
    grand_famille(Seuil),
    (NbFreres > Seuil -> Points = 1.5 ;
     Points = 0).

points_handicap(true, 1.5).
points_handicap(false, 0).

calculer_score(Revenu, CodePostal, NbFreres, Handicap, Score) :-
    points_salaire(Revenu, S1),
    points_postal(CodePostal, S2),
    points_famille(NbFreres, S3),
    points_handicap(Handicap, S4),
    Score is S1 + S2 + S3 + S4.

% Tree and visualization predicates (kept from original)
reset_tree :- retractall(edge(_, _)).

log_edge(Parent, Child) :- assertz(edge(Parent, Child)).

log_ranking([]).
log_ranking([etudiant(Nom, _, _, _, _, _)-_|[]]).
log_ranking([etudiant(Nom1, _, _, _, _, _)-_ , etudiant(Nom2, _, _, _, _, _)-_|Rest]) :-
    log_edge(Nom1, Nom2),
    log_ranking([etudiant(Nom2, _, _, _, _, _)-_|Rest]).

export_ranking_dot :-
    open('ranking_graph.dot', write, Stream),
    write(Stream, 'digraph MeritRanking {\n'),
    write(Stream, '    node [shape=rectangle, style=filled, fillcolor=lightblue];\n'),
    forall(edge(Parent, Child),
           format(Stream, '    "~w" -> "~w";~n', [Parent, Child])),
    write(Stream, '}\n'),
    close(Stream).

generate_ranking_pdf :-
    export_ranking_dot,
    shell('dot -Tpdf ranking_graph.dot -o ranking_graph.pdf').

% New GUI code starts here
create_student_gui :-
    new(D, dialog('Système de Classement des Étudiants')),
    send(D, append, new(BG, dialog_group('Ajouter un Étudiant', group))),
    
    % Input fields for student data
    send(BG, append, new(NomText, text_item('Nom de l\'étudiant:'))),
    send(BG, append, new(RevenuText, text_item('Revenu annuel:'))),
    send(BG, append, new(CodeText, text_item('Code postal:'))),
    send(BG, append, new(FamilleText, text_item('Nombre de frères/soeurs:'))),
    send(BG, append, new(HandicapMenu, menu('Handicap dans la famille?', choice))),
    send_list(HandicapMenu, append, [true, false]),
    send(HandicapMenu, default, false),
    
    % Add student button
    send(BG, append, button('Ajouter Étudiant',
                             message(@prolog, add_student_from_gui,
                                     NomText?selection,
                                     RevenuText?selection,
                                     CodeText?selection,
                                     FamilleText?selection,
                                     HandicapMenu?selection,
                                     D))),
    
    % Button to display merit list
    send(D, append, button('Afficher Liste de Mérite',
                          message(@prolog, show_merit_list, D))),
    
    % Button to generate ranking visualization
    send(D, append, button('Générer Graphique de Classement',
                          message(@prolog, generate_and_show_ranking))),
    
    % Display area for results
    send(D, append, new(RT, text('Résultats apparaîtront ici...'))),
    send(RT, font, font(helvetica, roman, 10)),
    send(RT, length, 400),
    send(RT, height, 200),
    send(D, attribute, result_text, RT),
    
    % Finalize dialog
    send(D, default_button, 'Ajouter Étudiant'),
    send(D, open).

% Handler for adding a student from GUI
add_student_from_gui(Nom, RevenuAtom, CodePostalAtom, NbFreresAtom, Handicap, Dialog) :-
    % Convert string inputs to numbers
    atom_number(RevenuAtom, Revenu),
    atom_number(CodePostalAtom, CodePostal),
    atom_number(NbFreresAtom, NbFreres),
    
    % Calculate score
    calculer_score(Revenu, CodePostal, NbFreres, Handicap, Score),
    
    % Add to knowledge base
    assertz(etudiant(Nom, Revenu, CodePostal, NbFreres, Handicap, Score)),
    
    % Display confirmation
    get(Dialog, attribute, result_text, RT),
    format(atom(Msg), 'Étudiant ~w ajouté avec score: ~2f', [Nom, Score]),
    send(RT, value, Msg).

% Display merit list in GUI
show_merit_list(Dialog) :-
    % Get sorted student list
    findall(etudiant(Nom, Revenu, CodePostal, NbFreres, Handicap, Score)-Score, 
        (etudiant(Nom, Revenu, CodePostal, NbFreres, Handicap, Score), nonvar(Nom)), 
            ListeEtudiants),
    sort(2, @>=, ListeEtudiants, ListeTriee),
    
    % Generate text representation
    get(Dialog, attribute, result_text, RT),
    with_output_to(atom(ListText), 
        (write('=== LISTE DES ÉTUDIANTS PAR ORDRE DE MÉRITE ===\n'),
         format_merit_list(ListeTriee))),
    
    % Log ranking structure
    reset_tree,
    log_ranking(ListeTriee),
    
    % Update display
    send(RT, value, ListText).

% Format merit list for display
format_merit_list([]).
format_merit_list([etudiant(Nom, Revenu, CodePostal, NbFreres, Handicap, Score)-_|Reste]) :-
    format('Nom: ~w, Score: ~2f\n', [Nom, Score]),
    format('  Revenu: ~w, Code: ~w, Frères/soeurs: ~w, Handicap: ~w\n', 
           [Revenu, CodePostal, NbFreres, Handicap]),
    format('-----------------------------------------\n'),
    format_merit_list(Reste).

% Generate and display ranking visualization
generate_and_show_ranking :-
    export_ranking_dot,
    shell('dot -Tpdf ranking_graph.dot -o ranking_graph.pdf'),
    shell('xdg-open ranking_graph.pdf').  % Linux
    % For Windows use: shell('start ranking_graph.pdf').
    % For Mac use: shell('open ranking_graph.pdf').

% Launch the application
:- initialization(create_student_gui).