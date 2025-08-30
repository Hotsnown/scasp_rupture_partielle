
% ===========================================
% RUPTURE_PARTIELLE — Base règles s(CASP) / Prolog
% (version corrigée)
% ===========================================

% -------------------------
% Paramètres modifiables
% -------------------------
:- dynamic param/2.
param(delta_ca_significatif_pct, 10).       % % baisse CA jugé significatif
param(delta_tarif_significatif_pct, 15).    % % variation tarifaire jugée significative
param(preavis_suffisant_mois, 6).           % seuil de « préavis suffisant »
param(delta_marge_significatif_pct, 10).
param(durabilite_par_defaut, on).

% -------------------------------------------------
% Déclarations dynamiques (vocabulaire factuel)
% -------------------------------------------------
:- dynamic
   relation/1, evenement/1, partie/1, produit/1, territoire/1, marche/1,
   survient_dans/2, notifie_par/2, subi_par/2,
   duree_relation_mois/2, flux_stables/1, flux_fluctuants/1,

   % clauses/pratiques établies (renommé: stipulation/2)
   stipulation/2,               % ex: stipulation(Rel, exclusivite_territoriale).
   pratique_etablie/2,          % ex: pratique_etablie(Rel, remise_recurrente).

   % Caractéristiques générales
   unilaterale/1, mutuelle/1, durable/1, temporaire/1,
   negociation_prealable/1, discussion_prealable/1,
   preavis_donne_mois/2,
   justification_contractuelle/1, justification_objective/1, justification_economique/1,
   justification_invoquee/2,
   conforme_usages/1, contrarie_usages/1,
   notifie_lr_ar/1, notifie_email/1,

   % Impacts et causalité
   delta_ca_pct/2, delta_marge_pct/2, perte_clientele_pct/2,
   perte_marche/2, perte_territoire/2, couts_supplementaires/2, handicap_operationnel/1,
   lien_causal/2,

   % Documents
   doc/3, prouve/2,

   % Tags — OBJET_CHANGE
   objet_change/1, affecte_element/2,
   modif_objet_moyens_execution/1, modif_objet_perimetre_activite/1, modif_objet_conditions_commerciales/1,
   arrete_volet_activite/2, dereferencement_produits_pct/2, retrait_outillage/1,
   reduction_territoire/2, privation_acces_marche/2,
   pendant_preavis/1,

   % Tags — MISE_EN_CONCURRENCE
   mise_en_concurrence/1,
   suppression_exclusivite/2, ouverture_distribution/1, ajout_distributeur_concurrent/1,
   ouverture_points_vente_propre/1, passage_appel_offres/1, perte_statut_privilegie/1,

   % Tags — PAIEMENT_CHANGE
   paiement_change/1,
   exige_paiement_comptant/1, modification_delai_paiement_jours/2,
   suppression_facilites_paiement/1, abaissement_encours_autorise/2, exige_garantie_bancaire/1,

   % Tags — PRIX_CHANGE
   prix_change/1,
   augmentation_prix_pct/2, diminution_prix_pct/2, suppression_remise_pct/2,
   nouvelle_grille_tarifaire/1, ajustement_frais_annexes_pct/2, modification_avantages_charte/1,

   % Prédicats optionnels
   incident_paiement/1, risque_financier_avere/1, perte_garantie_assureur/1,
   negociation_effective/1, proportionne/1, indexation_matiere_premiere/1,
   alignement_prix_marche/1, correspond_hausse_couts/1,
   accord_prix_caduc/1, avantages_charte/1,
   exclusivite_arrivee_terme/1, abandon_reciproque_exclusivite/1.

% -------------------------------------------------
% Énumérations (sans variables) — OK
% -------------------------------------------------
element_essentiel(moyens_execution).
element_essentiel(perimetre_activite).
element_essentiel(conditions_commerciales).
element_essentiel(objet_contrat).
element_essentiel(exclusivite).
element_essentiel(territoire).
element_essentiel(zone_chalandise).
element_essentiel(acces_marche).

doc_type(contrat).
doc_type(avenant).
doc_type(lrar).
doc_type(email).
doc_type(facture).
doc_type(statistiques).
doc_type(planogramme).
doc_type(cgv).
doc_type(bon_commande).

% -------------------------------------------------
% Aides au seuil (mutualisées)
% -------------------------------------------------
seuil_ca_significatif(S)    :- param(delta_ca_significatif_pct, S).
seuil_marge_significatif(S) :- param(delta_marge_significatif_pct, S).
seuil_tarif_significatif(S) :- param(delta_tarif_significatif_pct, S).

% -------------------------------------------------
% Négociation + préavis suffisant (mutualisé)
% -------------------------------------------------
negociation_et_preavis_suffisant(Ev) :-
    negociation_prealable(Ev),
    preavis_donne_mois(Ev, M),
    param(preavis_suffisant_mois, S),
    M >= S.

% -------------------------------------------------
% Justification générique (contr./objective)
% -------------------------------------------------
justifie(Ev) :- justification_contractuelle(Ev).
justifie(Ev) :- justification_objective(Ev).

% =========================
% OBJET_CHANGE
% =========================

% 1) Composante essentielle affectée unilatéralement
affecte_composante_essentielle_unilateralement(Ev) :-
    objet_change(Ev),
    unilaterale(Ev),
    affecte_element(Ev, Elem),
    element_essentiel(Elem).

% 2) Impact significatif et quantifiable
impact_significatif(Ev) :-
    delta_ca_pct(Ev, P), seuil_ca_significatif(S), P >= S.
impact_significatif(Ev) :-
    delta_marge_pct(Ev, P), seuil_marge_significatif(S), P >= S.
impact_significatif(Ev) :- perte_marche(Ev, _).
impact_significatif(Ev) :- perte_territoire(Ev, _).

impact_inferieur_seuil(Ev) :-
    delta_ca_pct(Ev, P), seuil_ca_significatif(S), P < S,
    not perte_marche(Ev, _), not perte_territoire(Ev, _).

% 3) Non-justification (spécifique OBJET_CHANGE)
non_justifie_objet_change(Ev) :- not justifie(Ev).

% 4) Caractère durable
caractere_durable(Ev) :- durable(Ev).
caractere_durable(Ev) :- param(durabilite_par_defaut, on), not temporaire(Ev).

% 6) Schéma défaut + exceptions
prima_facie_objet_change(Ev) :-
    affecte_composante_essentielle_unilateralement(Ev),
    impact_significatif(Ev),
    non_justifie_objet_change(Ev),
    caractere_durable(Ev).

exception_objet_change(Ev) :- justifie(Ev).
exception_objet_change(Ev) :- temporaire(Ev).
exception_objet_change(Ev) :- impact_inferieur_seuil(Ev).
exception_objet_change(Ev) :- negociation_et_preavis_suffisant(Ev).
exception_objet_change(Ev) :- mutuelle(Ev).
exception_objet_change(Ev) :- conforme_usages(Ev).

rupture_partielle_objet_change(Ev) :-
    prima_facie_objet_change(Ev),
    not exception_objet_change(Ev).

% 7) Contraintes
:- durable(Ev),    temporaire(Ev).
:- unilaterale(Ev), mutuelle(Ev).

% =========================
% MISE_EN_CONCURRENCE
% =========================

% 1) Statut privilégié antérieur (de droit ou de fait)
statut_privilegie_preexistant(Rel) :- stipulation(Rel, exclusivite_territoriale).
statut_privilegie_preexistant(Rel) :- stipulation(Rel, exclusivite_de_fait).

absence_statut_privilegie(Rel) :- not statut_privilegie_preexistant(Rel).

% 2) Modification unilatérale de la protection
modif_protection_concurrentielle_unilaterale(Ev) :-
    mise_en_concurrence(Ev),
    unilaterale(Ev),
    (  suppression_exclusivite(Ev, _)
    ;  ouverture_distribution(Ev)
    ;  ajout_distributeur_concurrent(Ev)
    ;  ouverture_points_vente_propre(Ev)
    ;  passage_appel_offres(Ev)
    ;  perte_statut_privilegie(Ev)
    ).

% 3) Altération substantielle des conditions concurrentielles
exposition_nouvelle(Ev) :- suppression_exclusivite(Ev, _).
exposition_nouvelle(Ev) :- ouverture_distribution(Ev).
exposition_nouvelle(Ev) :- ajout_distributeur_concurrent(Ev).
exposition_nouvelle(Ev) :- ouverture_points_vente_propre(Ev).
exposition_nouvelle(Ev) :- passage_appel_offres(Ev).

modification_significative_position(Ev) :- perte_statut_privilegie(Ev).
modification_significative_position(Ev) :- reduction_territoire(Ev, _).
modification_significative_position(Ev) :- privation_acces_marche(Ev, _).

alteration_substantielle_concurrence(Ev) :- exposition_nouvelle(Ev).
alteration_substantielle_concurrence(Ev) :- modification_significative_position(Ev).

% 4) Causalité directe vers une dégradation économique
causalite_directe(Ev) :-
    impact_significatif(Ev),
    ( lien_causal(Ev, baisse_ca)
    ; lien_causal(Ev, baisse_rentabilite)
    ; lien_causal(Ev, perte_clientele)
    ; lien_causal(Ev, perte_marche)
    ; lien_causal(Ev, perte_territoire)
    ).

% 5) Accord préalable (spécifique MISE_EN_CONCURRENCE)
accord_prealable(Ev) :- negociation_prealable(Ev).
accord_prealable(Ev) :- mutuelle(Ev).

non_justifie_mise_en_concurrence(Ev) :-
    not justifie(Ev),
    not accord_prealable(Ev).

% 6) Schéma défaut + exceptions
prima_facie_mise_en_concurrence(Ev) :-
    survient_dans(Ev, Rel),
    statut_privilegie_preexistant(Rel),
    modif_protection_concurrentielle_unilaterale(Ev),
    alteration_substantielle_concurrence(Ev),
    non_justifie_mise_en_concurrence(Ev),
    causalite_directe(Ev).

exception_mise_en_concurrence(Ev) :- survient_dans(Ev, Rel), absence_statut_privilegie(Rel).
exception_mise_en_concurrence(Ev) :- survient_dans(Ev, Rel), exclusivite_arrivee_terme(Rel).
exception_mise_en_concurrence(Ev) :- survient_dans(Ev, Rel), abandon_reciproque_exclusivite(Rel).
exception_mise_en_concurrence(Ev) :- negociation_et_preavis_suffisant(Ev).
exception_mise_en_concurrence(Ev) :- conforme_usages(Ev).
exception_mise_en_concurrence(Ev) :- justifie(Ev).

rupture_partielle_mise_en_concurrence(Ev) :-
    prima_facie_mise_en_concurrence(Ev),
    not exception_mise_en_concurrence(Ev).

% 7) Contraintes
:- unilaterale(Ev), mutuelle(Ev).
:- survient_dans(Ev, Rel), absence_statut_privilegie(Rel), statut_privilegie_preexistant(Rel).

% =========================
% PAIEMENT_CHANGE
% =========================

% 1) Modification des moyens de paiement
modif_moyens_paiement(Ev) :-
    paiement_change(Ev),
    (  exige_paiement_comptant(Ev)
    ;  modification_delai_paiement_jours(Ev, _)
    ;  suppression_facilites_paiement(Ev)
    ;  abaissement_encours_autorise(Ev, _)
    ;  exige_garantie_bancaire(Ev)
    ).

% 2) Pratique établie (pratiques/clauses)
pratique_paiement_etablie(Rel) :- pratique_etablie(Rel, facilites_paiement).
pratique_paiement_etablie(Rel) :- pratique_etablie(Rel, delai_paiement).
pratique_paiement_etablie(Rel) :- stipulation(Rel, delai_paiement(_)).

absence_pratique_paiement_etablie(Rel) :- not pratique_paiement_etablie(Rel).

% 3) Affectation substantielle
affecte_substantiellement_conditions(Ev) :- handicap_operationnel(Ev).
affecte_substantiellement_conditions(Ev) :- impact_significatif(Ev).

non_substantiel(Ev) :- impact_inferieur_seuil(Ev), not handicap_operationnel(Ev).

% 4) Préavis insuffisant vs suffisant
preavis_insuffisant(Ev) :- not preavis_donne_mois(Ev, _).
preavis_insuffisant(Ev) :-
    preavis_donne_mois(Ev, M),
    param(preavis_suffisant_mois, S),
    M < S.

% 5) Justifications légitimes
justifie_financier(Ev) :- incident_paiement(Ev).
justifie_financier(Ev) :- risque_financier_avere(Ev).
justifie_financier(Ev) :- perte_garantie_assureur(Ev).

justifie_paiement_change(Ev) :- justification_contractuelle(Ev).
justifie_paiement_change(Ev) :- justification_objective(Ev).
justifie_paiement_change(Ev) :- justifie_financier(Ev).

non_justifie_paiement_change(Ev) :- not justifie_paiement_change(Ev).

% 6) Schéma défaut
prima_facie_paiement_change(Ev) :-
    paiement_change(Ev),
    unilaterale(Ev),
    modif_moyens_paiement(Ev),
    survient_dans(Ev, Rel),
    pratique_paiement_etablie(Rel),
    affecte_substantiellement_conditions(Ev),
    preavis_insuffisant(Ev),
    non_justifie_paiement_change(Ev).

% 7) Exceptions
exception_paiement_change(Ev) :- survient_dans(Ev, Rel), absence_pratique_paiement_etablie(Rel).
exception_paiement_change(Ev) :- justifie_paiement_change(Ev).
exception_paiement_change(Ev) :- negociation_et_preavis_suffisant(Ev).
exception_paiement_change(Ev) :- mutuelle(Ev).
exception_paiement_change(Ev) :- conforme_usages(Ev).
exception_paiement_change(Ev) :- non_substantiel(Ev).

% 8) Conclusion
rupture_partielle_paiement_change(Ev) :-
    prima_facie_paiement_change(Ev),
    not exception_paiement_change(Ev).

% 9) Contraintes
:- unilaterale(Ev), mutuelle(Ev).
:- preavis_donne_mois(Ev, M), param(preavis_suffisant_mois, S), M >= S, preavis_insuffisant(Ev).

% =========================
% PRIX_CHANGE
% =========================

% 1) Détection d'une modification tarifaire
modif_prix(Ev) :-
    prix_change(Ev),
    (  augmentation_prix_pct(Ev, _)
    ;  diminution_prix_pct(Ev, _)
    ;  suppression_remise_pct(Ev, _)
    ;  ajustement_frais_annexes_pct(Ev, _)
    ;  nouvelle_grille_tarifaire(Ev)
    ;  modification_avantages_charte(Ev)
    ).

% 2) Conditions tarifaires établies (relation)
conditions_tarifaires_etablies(Rel) :- pratique_etablie(Rel, conditions_tarifaires_stables).
conditions_tarifaires_etablies(Rel) :- pratique_etablie(Rel, remise_recurrente).
conditions_tarifaires_etablies(Rel) :- avantages_charte(Rel).

absence_conditions_tarifaires_etablies(Rel) :- not conditions_tarifaires_etablies(Rel).

% 3) Variation quantitative significative (> seuil)
variation_tarifaire_pct(Ev, P) :- augmentation_prix_pct(Ev, P).
variation_tarifaire_pct(Ev, P) :- diminution_prix_pct(Ev, P).
variation_tarifaire_pct(Ev, P) :- suppression_remise_pct(Ev, P).
variation_tarifaire_pct(Ev, P) :- ajustement_frais_annexes_pct(Ev, P).

variation_tarifaire_significative(Ev) :-
    variation_tarifaire_pct(Ev, P),
    seuil_tarif_significatif(S),
    P >= S.

variation_tarifaire_non_significative(Ev) :-
    variation_tarifaire_pct(Ev, P),
    seuil_tarif_significatif(S),
    P < S,
    not impact_significatif(Ev).

% 4) Atteinte démontrable à l'équilibre économique
atteinte_equilibre_economique(Ev) :- impact_significatif(Ev).
atteinte_equilibre_economique(Ev) :-
    delta_marge_pct(Ev, P), seuil_marge_significatif(S), P >= S.
atteinte_equilibre_economique(Ev) :- lien_causal(Ev, baisse_rentabilite).
atteinte_equilibre_economique(Ev) :- lien_causal(Ev, baisse_ca).

% 5) Négociation préalable effective
negociation_prealable_effective(Ev) :-
    negociation_prealable(Ev),
    negociation_effective(Ev).

absence_negociation_prealable_effective(Ev) :-
    not negociation_prealable_effective(Ev).

% 6) Justification économique objective OU proportionnalité
justifie_economiquement(Ev) :- justification_economique(Ev).
justifie_economiquement(Ev) :- proportionne(Ev).
justifie_economiquement(Ev) :- indexation_matiere_premiere(Ev).
justifie_economiquement(Ev) :- alignement_prix_marche(Ev).
justifie_economiquement(Ev) :- correspond_hausse_couts(Ev).

non_justifie_ou_non_proportionne(Ev) :- not justifie_economiquement(Ev).

% 7) Schéma défaut (conditions cumulatives)
prima_facie_prix_change(Ev) :-
    modif_prix(Ev),
    unilaterale(Ev),
    absence_negociation_prealable_effective(Ev),
    non_justifie_ou_non_proportionne(Ev),
    variation_tarifaire_significative(Ev),
    atteinte_equilibre_economique(Ev),
    survient_dans(Ev, Rel),
    conditions_tarifaires_etablies(Rel).

% 8) Exceptions
exception_prix_change(Ev) :- survient_dans(Ev, Rel), absence_conditions_tarifaires_etablies(Rel).
exception_prix_change(Ev) :- survient_dans(Ev, Rel), accord_prix_caduc(Rel).
exception_prix_change(Ev) :- negociation_prealable_effective(Ev).
exception_prix_change(Ev) :- negociation_et_preavis_suffisant(Ev).
exception_prix_change(Ev) :- justifie_economiquement(Ev).
exception_prix_change(Ev) :- variation_tarifaire_non_significative(Ev).
exception_prix_change(Ev) :- mutuelle(Ev).
exception_prix_change(Ev) :- conforme_usages(Ev).
exception_prix_change(Ev) :- temporaire(Ev).

% 9) Conclusion
rupture_partielle_prix_change(Ev) :-
    prima_facie_prix_change(Ev),
    not exception_prix_change(Ev).

% 10) Contraintes
:- unilaterale(Ev), mutuelle(Ev).

% ===========================================
% (Exemples d'instanciation — à placer ailleurs)
% -------------------------------------------
% relation(r1).
% evenement(e1).
% survient_dans(e1, r1).
% objet_change(e1).
% unilaterale(e1).
% affecte_element(e1, perimetre_activite).
% dereferencement_produits_pct(e1, 12).
% delta_ca_pct(e1, 18).
% preavis_donne_mois(e1, 1).
% notifie_lr_ar(e1).
% doc(d1, contrat, date(2017,05,02)).
% prouve(r1, d1).
% ===========================================
