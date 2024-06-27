% Expert system shell based on Luger
% To run, solve(diagnose(X),CF)
% solve(+,?)

solve(Goal, CF) :-
    print_instructions,
    retractall(known(_, _)),
    solve(Goal, CF, [], 20).


print_instructions :-
    nl, write('You will be asked a series of queries.'), nl,
    write('Your response must be either:'),
    nl, write('a. A number between -100 and 100 representing'), nl,
    write(' your confidence in the truth of the query'), nl,
    write('b. why.'), % added period
    nl, write('c. how(X), where X is a goal.'), nl.

% solve(+,?,+,+)
solve(Goal, CF, _, Threshold) :-
    known(Goal, CF), !,
    above_threshold(CF, Threshold).
solve(not(Goal), CF, Rules, Threshold) :- !,
    invert_threshold(Threshold, New_threshold),
    solve(Goal, CF_goal, Rules, New_threshold),
    negate_cf(CF_goal, CF).

solve((Goal1, Goal2), CF, Rules, Threshold) :- !,
    solve(Goal1, CF1, Rules, Threshold),
    above_threshold(CF1, Threshold),
    solve(Goal2, CF2, Rules, Threshold),
    above_threshold(CF2, Threshold),
    and_cf(CF1, CF2, CF).

solve(Goal, CF, Rules, Threshold) :-
    rule((Goal:-(Premise)), CF_rule),
    solve(Premise, CF_premise, [rule((Goal:-Premise), CF_rule) | Rules], Threshold),
    rule_cf(CF_rule, CF_premise, CF),
    above_threshold(CF, Threshold).

solve(Goal, CF, _, Threshold) :-
    rule(Goal, CF),
    above_threshold(CF, Threshold).

solve(Goal, CF, Rules, Threshold) :-
    askable(Goal),
    askuser(Goal, CF, Rules), !,
    assert(known(Goal, CF)),
    above_threshold(CF, Threshold).

above_threshold(CF, T) :- T >= 0, CF >= T.
above_threshold(CF, T) :- T < 0, CF =< T.

invert_threshold(Threshold, New_threshold) :-
    New_threshold is -1 * Threshold.

negate_cf(CF, Negated_CF) :-
    Negated_CF is -1 * CF.
and_cf(A, B, A) :- A =< B.
and_cf(A, B, B) :- B < A.

rule_cf(CF_rule, CF_premise, CF) :-
    CF is (CF_rule * CF_premise / 100).

% askuser(+,?,+)
askuser(Goal, CF, Rules) :-
    nl, write('Query : '),
    write(Goal), write(' ? '),
    read(Ans),
    respond(Ans, Goal, CF, Rules).

% respond(+,+,?,+)
respond(CF, _, CF, _) :-
    number(CF), CF =< 100, CF >= -100. % no response issued because the user enters a valid CF
respond(why, Goal, CF, [Rule | Rules]) :-
    nl, write_rule(Rule),
    askuser(Goal, CF, Rules).

respond(why, Goal, CF, []) :-
    nl, write('Back to the top of the rule stack.'), nl, askuser(Goal, CF, []).

respond(how(X), Goal, CF, Rules) :-
    build_proof(X, CF_X, Proof), !,
    nl, write('The goal '), write(X),
    write(' was concluded with certainty '), write(CF_X), write('.'), nl, nl, write('The proof of this is:'), nl,
    write_proof(Proof, 0), nl,
    askuser(Goal, CF, Rules).

respond(how(X), Goal, CF, Rules) :-
    write('The truth of '), write(X), nl,
    write('is not yet known.'), nl,
    askuser(Goal, CF, Rules).

respond(_, Goal, CF, Rules):-
    write('Unrecognized response.'), nl, askuser(Goal, CF, Rules).

% build_proof(+,?,?)
build_proof(Goal, CF, (Goal, CF:-given)) :-
    known(Goal, CF), !.
build_proof(not(Goal), CF, not(Proof)) :- !,
    build_proof(Goal, CF_goal, Proof), negate_cf(CF_goal, CF).
build_proof((Goal1, Goal2), CF, (Proof1, Proof2)) :-
    build_proof(Goal1, CF1, Proof1),
    build_proof(Goal2, CF2, Proof2), and_cf(CF1, CF2, CF).
build_proof(Goal, CF, (Goal, CF:-Proof)) :-
    rule((Goal:-Premise), CF_rule),
    build_proof(Premise, CF_premise, Proof),
    rule_cf(CF_rule, CF_premise, CF).
build_proof(Goal, CF, (Goal, CF:-fact)) :-
    rule(Goal, CF).

write_rule(rule((Goal:-(Premise)), CF)) :-
    write('I am trying to prove the following rule:'), nl, write(Goal),
    write(':-'), nl,
    write_premise(Premise),
    write('CF = '), write(CF), nl.
write_rule(rule(Goal, CF)) :-
    write('I am trying to prove the following goal:'), nl, write(Goal),
    write('CF = '), write(CF), nl.
write_premise((Premise1, Premise2)) :- !,
    write_premise(Premise1), write_premise(Premise2).
write_premise(not(Premise)) :- !,
    write(' '), write(not), write(' '), write(Premise), nl.
write_premise(Premise) :- !,
    write(' '), write(Premise), nl.

% write_proof(+,+)
write_proof((Goal, CF:-given), Level) :-
    indent(Level), write(Goal), write(' CF='), write(CF), write(' was given by the user'), nl, !.
write_proof((Goal, CF:-fact), Level) :-
    indent(Level), write(Goal), write(' CF='), write(CF), write(' was a fact in the KB'), nl, !.
write_proof((Goal, CF:-Proof), Level) :-
    indent(Level), write(Goal), write(' CF='), write(CF), write(':-'), nl,
    New_level is Level + 1,
    write_proof(Proof, New_level), !.
write_proof(not(Proof), Level) :-
    indent(Level), write((not)), nl,
    New_level is Level + 1,
    write_proof(Proof, New_level), !.
write_proof((Proof1, Proof2), Level) :-
    write_proof(Proof1, Level), write_proof(Proof2, Level), !.
indent(0).
indent(X) :-
    write(' '), X_new is X - 1, indent(X_new).

rule((diagnose(Diagnosis):-(disease(X), diagnose(X,Diagnosis))),100).

/* emergency diseases */
rule((disease(emergency_oral_cancer):-(is_emergency, examine_teeth(loose), examine_patient(lump_in_neck), examine_mouth(swollen_or_sore_lip))), 40).

rule((disease(emergency_acute_apical_abscesses):-(is_emergency, examine_teeth(swelling_at_root_of_tooth), ask_patient(throbbing_pain), examine_teeth(tenderness))), 60).

/* non-emergency diseases */
% connected diseases
rule((disease(irreversible_pulpitis):-(disease(caries), examine_patient(continuous_pain_intensified_by_heat_cold), examine_patient(pain_when_biting_down))), 70).
rule((disease(reversible_pulpitis):-(disease(caries), examine_patient(pain_goes_quickly_after_irritant_removed))), 70).
rule((disease(severe_gingivosis):-(disease(moderate_gingivosis), ask_patient(burning_sensation), examine_mouth(dry), examine_mouth(surface_epithelium_shredded))), 80).
rule((disease(moderate_gingivosis):-(disease(mild_gingivosis), examine_teeth(thermal_sensitivity), examine_mouth(patchy_gray_areas), ask_patient(painful_inhalation_of_air), examine_gums(peeling))), 80).
rule((disease(mild_gingivosis):-(symptom(inflamed_gums), examine_gums(smooth_texture))), 65).
rule((disease(gingival_hyperplasia):-(symptom(inflamed_gums), examine_gums(cover_tooth_crown), examine_gums(firm), examine_gums(dense), examine_gums(tender), examine_teeth(plaque_buildup), examine_mouth(bad_breath))), 70).
rule((disease(periodontitis):-(disease(gingivitis), examine_teeth(gaps_present), examine_teeth(loose))), 80).

/* gingivitis */
rule((disease(gingivitis):-(symptom(inflamed_gums), examine_gums(bleeding))), 80).
rule((disease(gingivitis):-(symptom(blue_and_swollen), examine_gums(bleeding))), 80).

/* symptoms only */
rule((symptom(inflamed_gums):- (examine_gums(red), examine_gums(swollen))), 80).
rule((symptom(blue_and_swollen):- (examine_gums(blue), examine_gums(swollen))), 80).

% unconnected diseases
rule((disease(caries):-(examine_mouth(bad_breath), examine_teeth(dark_spots), examine_teeth(sensitive), examine_teeth(has_pits), examine_teeth(poor_oral_hygiene))), 100).
rule((disease(burning_mouth_syndrome):-(ask_patient(bitter_taste_in_mouth), examine_mouth(dry_mouth), ask_patient(burning_sensation))), 80).
rule((disease(atrophic_glossitis):-(examine_mouth(change_in_texture_and_color), examine_mouth(swelling_of_tongue), examine_mouth(loss_of_papillae))), 80).
rule((disease(periodontal_atrophy):-(examine_mouth(exposure_of_root_surface), examine_teeth(thermal_sensitivity), examine_gums(thin_atrophic_gingiva), examine_gums(firm), examine_gums(pale))), 80).
rule((disease(herpetic_stomatitis):-(examine_mouth(white_or_yellow_patches), examine_mouth(patches_undefined_shape))), 70).
rule((disease(canker_sore):-(examine_mouth(white_or_yellow_patches), examine_mouth(patches_circular_shape))), 70).
rule((disease(nonemergency_acute_apical_abscesses):-(not(is_emergency), examine_teeth(swelling_at_root_of_tooth), ask_patient(throbbing_pain), examine_teeth(tenderness))), 60).
rule((disease(nonemergency_oral_cancer):-(not(is_emergency), examine_teeth(loose), examine_patient(lump_in_neck), examine_mouth(swollen_or_sore_lip))), 40).

% diagnose
rule(diagnose(periodontitis,'Based on the presented symptoms, the preliminary diagnosis indicates Periodontitis. However, this is a preliminary assessment, and a definitive diagnosis should be obtained through a thorough evaluation by a qualified healthcare professional. This information is not a substitute for professional medical advice, and it is recommended to seek the guidance of a healthcare professional for an accurate diagnosis and appropriate treatment.'),100).
rule(diagnose(gingivitis,'Based on the presented symptoms, the preliminary diagnosis indicates Gingivitis. However, this is a preliminary assessment, and a definitive diagnosis should be obtained through a thorough evaluation by a qualified healthcare professional. This information is not a substitute for professional medical advice, and it is recommended to seek the guidance of a healthcare professional for an accurate diagnosis and appropriate treatment.'),100).
rule(diagnose(caries, 'Based on the presented symptoms, the preliminary diagnosis indicates Caries. However, this is a preliminary assessment, and a definitive diagnosis should be obtained through a thorough evaluation by a qualified healthcare professional. This information is not a substitute for professional medical advice, and it is recommended to seek the guidance of a healthcare professional for an accurate diagnosis and appropriate treatment.'), 100).
rule(diagnose(mild_gingivosis, 'Based on the presented symptoms, the preliminary diagnosis indicates Mild Gingivosis. However, this is a preliminary assessment, and a definitive diagnosis should be obtained through a thorough evaluation by a qualified healthcare professional. This information is not a substitute for professional medical advice, and it is recommended to seek the guidance of a healthcare professional for an accurate diagnosis and appropriate treatment.'), 100).
rule(diagnose(moderate_gingivosis, 'Based on the presented symptoms, the preliminary diagnosis indicates Moderate Gingivosis. However, this is a preliminary assessment, and a definitive diagnosis should be obtained through a thorough evaluation by a qualified healthcare professional. This information is not a substitute for professional medical advice, and it is recommended to seek the guidance of a healthcare professional for an accurate diagnosis and appropriate treatment.'), 100).
rule(diagnose(severe_gingivosis, 'Based on the presented symptoms, the preliminary diagnosis indicates Severe Gingivosis. However, this is a preliminary assessment, and a definitive diagnosis should be obtained through a thorough evaluation by a qualified healthcare professional. This information is not a substitute for professional medical advice, and it is recommended to seek the guidance of a healthcare professional for an accurate diagnosis and appropriate treatment.'), 100).
rule(diagnose(gingival_hyperplasia, 'Based on the presented symptoms, the preliminary diagnosis indicates Gingival Hyperplasia. However, this is a preliminary assessment, and a definitive diagnosis should be obtained through a thorough evaluation by a qualified healthcare professional. This information is not a substitute for professional medical advice, and it is recommended to seek the guidance of a healthcare professional for an accurate diagnosis and appropriate treatment.'), 100).
rule(diagnose(reversible_pulpitis, 'Based on the presented symptoms, the preliminary diagnosis indicates Reversible Pulpitis. However, this is a preliminary assessment, and a definitive diagnosis should be obtained through a thorough evaluation by a qualified healthcare professional. This information is not a substitute for professional medical advice, and it is recommended to seek the guidance of a healthcare professional for an accurate diagnosis and appropriate treatment.'), 100).
rule(diagnose(irreversible_pulpitis, 'Based on the presented symptoms, the preliminary diagnosis indicates Irreversible Pulpitis. However, this is a preliminary assessment, and a definitive diagnosis should be obtained through a thorough evaluation by a qualified healthcare professional. This information is not a substitute for professional medical advice, and it is recommended to seek the guidance of a healthcare professional for an accurate diagnosis and appropriate treatment.'), 100).
rule(diagnose(burning_mouth_syndrome, 'Based on the presented symptoms, the preliminary diagnosis indicates Burning Mouth Syndrome. However, this is a preliminary assessment, and a definitive diagnosis should be obtained through a thorough evaluation by a qualified healthcare professional. This information is not a substitute for professional medical advice, and it is recommended to seek the guidance of a healthcare professional for an accurate diagnosis and appropriate treatment.'), 100).
rule(diagnose(atrophic_glossitis, 'Based on the presented symptoms, the preliminary diagnosis indicates Atrophic Glossitis. However, this is a preliminary assessment, and a definitive diagnosis should be obtained through a thorough evaluation by a qualified healthcare professional. This information is not a substitute for professional medical advice, and it is recommended to seek the guidance of a healthcare professional for an accurate diagnosis and appropriate treatment.'), 100).
rule(diagnose(periodontal_atrophy, 'Based on the presented symptoms, the preliminary diagnosis indicates Periodontal Atrophy. However, this is a preliminary assessment, and a definitive diagnosis should be obtained through a thorough evaluation by a qualified healthcare professional. This information is not a substitute for professional medical advice, and it is recommended to seek the guidance of a healthcare professional for an accurate diagnosis and appropriate treatment.'), 100).
rule(diagnose(herpetic_stomatitis, 'Based on the presented symptoms, the preliminary diagnosis indicates Herpetic Stomatitis. However, this is a preliminary assessment, and a definitive diagnosis should be obtained through a thorough evaluation by a qualified healthcare professional. This information is not a substitute for professional medical advice, and it is recommended to seek the guidance of a healthcare professional for an accurate diagnosis and appropriate treatment.'), 100).
rule(diagnose(canker_sore, 'Based on the presented symptoms, the preliminary diagnosis indicates Canker Sore. However, this is a preliminary assessment, and a definitive diagnosis should be obtained through a thorough evaluation by a qualified healthcare professional. This information is not a substitute for professional medical advice, and it is recommended to seek the guidance of a healthcare professional for an accurate diagnosis and appropriate treatment.'), 100).

/* extra tests */
rule(diagnose(nonemergency_acute_apical_abscesses, 'Based on the presented symptoms, the preliminary diagnosis indicates signs of Oral Cancer. In order to accurately diagnose the disease, the patient is to be referred to a large medical facility.'), 100).
rule(diagnose(emergency_acute_apical_abscesses, 'Based on the presented symptoms, the preliminary diagnosis indicates Acute Apical Abscesses. However, this is a preliminary assessment, and a definitive diagnosis should be obtained through a thorough evaluation by a qualified healthcare professional. This information is not a substitute for professional medical advice, and it is recommended to seek the guidance of a healthcare professional for an accurate diagnosis and appropriate treatment.'), 100).

rule(diagnose(nonemergency_oral_cancer, 'Based on the presented symptoms, the preliminary diagnosis indicates signs of Oral Cancer. In order to accurately diagnose the disease, the patient is to be referred to a large medical facility.'), 100).
rule(diagnose(emergency_oral_cancer, 'Based on the presented symptoms, the preliminary diagnosis indicates Oral Cancer. However, this is a preliminary assessment, and a definitive diagnosis should be obtained through a thorough evaluation by a qualified healthcare professional. This information is not a substitute for professional medical advice, and it is recommended to seek the guidance of a healthcare professional for an accurate diagnosis and appropriate treatment.'), 100).

% askables
askable(is_emergency).
askable(examine_gums(_)).
askable(examine_mouth(_)).
askable(examine_teeth(_)).
askable(ask_patient(_)).
askable(examine_patient(_)).
