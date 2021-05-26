# Mise à jour 26/05/2021:

- Ajout `README.md`
- Suppression de l'extension Couleur qui provoquait un conflit menhir shift-reduce à cause d'un mauvais choix de format étant `Couleur expr1 expr2 expr2`,
on aurait dû contraindre le choix à trois couleurs comme ceci: `Couleur nomDuCouleur` (avec Rouge, Vert, Bleu).

# Compilation:
- Dans un terminal ouvert dans le répertoire courant, entrez `dune build`

# Exécution:
- Pour exécuter, entrez `./_build/default/run.exe < test_fic`

# Organisation du binôme:
RAPETERAMANA Sarobidy : Parser, Interpréteur, Run

SANGSIRI Nicolas : Lexer, checkSyntax, une petite partie de l'interpréteur

# Parties du projet réalisées:
- Projet de base
- Si Alors
- Epaisseur du dessin

