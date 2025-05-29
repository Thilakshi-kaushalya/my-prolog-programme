% Facts
% General informations about exports spices.
% spice(Spice, Zone, Climate, RainfallRange, TempRange, Soil, SoilPh, Irrigation, Planting, Pests, Fertilizers, Diseases).

spice(cinnamon, wet_and_intermediate_zones, tropical_humid, rain(2000,2500), temp(20,30),
      'well-drained loamy or sandy', pH(5.5,6.5), drip_or_surface_irrigation, root_cuttings_or_saplings,
      [termites, scale_insects, leaf_spots],
      [organic_compost, npk_fertilizer],
      [leaf_spots-dry_brown_patches, scale_insects-white_spots_on_bark]).

spice(pepper, wet_and_intermediate_zones, tropical_humid, rain(2000,2500), temp(25,30),
      'well-drained loamy', pH(5.5,6.5), drip_or_surface_irrigation, cuttings_from_mature_vines,
      [leaf_spot, nematodes, thrips],
      [organic_compost, npk_fertilizer],
      [leaf_spot-yellow_patches, thrips-leaf_curling]).

spice(cloves, wet_and_intermediate_zones, tropical_humid, rain(2000,3000), temp(20,30),
      'well-drained loamy volcanic', pH(5.5,6.5), drip_irrigation, clove_buds_or_seedlings,
      [whitefly, root_rot, mealybugs],
      [organic_compost, balanced_fertilizer],
      [root_rot-plant_wilting, mealybugs-sticky_residue]).

spice(cardamom, wet_zones_mid_and_high_altitudes, tropical_humid, rain(2000,3500), temp(18,30),
      'rich well-drained loamy', pH(6.0,6.5), drip_irrigation_or_rainfall, seeds_or_rhizomes,
      [root_rot, thrips, aphids],
      [organic_compost, balanced_fertilizer],
      [root_rot-plant_wilting, aphids-sticky_leaves]).

spice(nutmeg, wet_zones_coastal_areas, tropical_humid, rain(2000,3000), temp(25,30),
      'well-drained sandy_loam', pH(5.5,6.5), drip_irrigation, seeds_or_grafting,
      [root_rot, mealybugs, scale_insects],
      [organic_compost, npk_fertilizer],
      [root_rot-root_blackening, mealybugs-white_clusters]).

spice(turmeric, wet_and_intermediate_zones, tropical_humid, rain(1500,2500), temp(20,30),
      'loamy_or_sandy_well_drained', pH(5.5,6.5), drip_irrigation, rhizomes,
      [root_rot, leaf_spot, aphids],
      [organic_compost, npk_fertilizer],
      [root_rot-rhizome_softening, leaf_spot-yellow_blotches]).

spice(ginger, wet_and_intermediate_zones, tropical_humid, rain(2000,3000), temp(25,30),
      'well-drained loamy_or_sandy', pH(5.5,6.5), drip_irrigation, rhizomes,
      [root_rot, aphids, leaf_spot],
      [organic_compost, npk_fertilizer],
      [root_rot-soft_rhizomes, aphids-sticky_leaves]).

spice(vanilla, wet_zones_tropical_lowlands, tropical_humid, rain(2000,3000), temp(25,30),
      'well-drained rich loamy', pH(5.5,6.5), drip_or_surface_irrigation, cuttings_of_vanilla_vines,
      [aphids, mealybugs, spider_mites],
      [organic_compost, balanced_fertilizer],
      [mealybugs-white_waxy_covering, spider_mites-webbing_on_leaves]).

% fertilizer(Spice, Nutrient_type, Fertilizers, Brands, Frequency, Tips).

fertilizer(cinnamon,
    [n, p, k, organic],
    [urea, tsp, mop, compost_or_cow_dung],
    [urea-lakpohora, urea-cic, tsp-ccf, compost-sevanagala],
    [march, may, october],
    'Apply around base. Use mulch after fertilizer.').

fertilizer(cloves,
    [n, p, k, organic],
    [urea, tsp, mop, organic_compost, poultry_manure],
    [poultry_manure-govi_aruna, poultry_manure-bio_lanka, others-cic, others-hayleys],
    [march, october],
    'Apply in circular trench. Avoid root disturbance.').

fertilizer(nutmeg,
    [n, p, k, organic],
    [urea, tsp, mop, farmyard_manure_or_compost],
    [organic-bio_green, organic-lak_sewana, fertilizers-lakpohora, fertilizers-cic],
    [april, september],
    'Use well-decomposed manure. Ensure drainage.').

fertilizer(ginger,
    [balanced_npk, organic],
    [npk_15_15_15, erp, compost],
    [npk-cic_triple_mix, erp-lanka_phosphate, compost-govipola],
    [at_planting, plus_45_days, plus_90_days],
    'Needs good drainage & mulching. Weed often.').

fertilizer(pepper,
    [high_k, mg, organic],
    [urea, mop, tsp_or_erp, dolomite, compost],
    [dolomite-lanka_minerals, compost-bio_lanka],
    [april, october],
    'Apply dolomite if soil is acidic. Train vines for airflow.').

fertilizer(cardamom,
    [n, p, k, organic],
    [npk_12_24_12_or_15_15_15, compost],
    [npk-hayleys_fertimix, npk-cic_cardamom_mix],
    [february, june, october],
    'Needs shade. Avoid water stagnation.').

fertilizer(vanilla,
    [balanced_npk, micronutrients],
    [npk_10_10_10, compost, foliar_feed],
    [seaweed-seasol, seaweed-bio_gold, npk-cic, npk-hayleys],
    [every_3_months, after_pruning],
    'Foliar feed every 2 weeks. Support vines with stakes.').

fertilizer(turmeric,
    [high_k, organic],
    [seaweed_liquid_foliar, urea, mop, erp, compost],
    [erp-eppawala_rock_phosphate, compost-sevanagala],
    [at_planting, plus_60_days],
    'Needs moist, well-drained soil. Use mulch.').

% pest_disease(Spice, Pest_and_Diseases, Solutions).

pest_disease(cinnamon,
    [termites, scale_insects, leaf_spots],
    [use_neem_oil_or_neem_based_sprays, prune_affected_branches, apply_bordeaux_mixture]).

pest_disease(pepper,
    [leaf_spot, nematodes, thrips],
    [apply_copper_based_fungicides, use_neem_cake_against_nematodes, encourage_natural_predators_ladybugs]).

pest_disease(cloves,
    [whitefly, root_rot, mealybugs],
    [spray_insecticidal_soap_or_neem_oil, ensure_well_drained_soil, use_sticky_traps_for_whiteflies]).

pest_disease(cardamom,
    [root_rot, thrips, aphids],
    [use_trichoderma_for_soil_treatment, control_humidity, use_insecticidal_soap_or_garlic_spray]).

pest_disease(nutmeg,
    [root_rot, mealybugs, scale_insects],
    [improve_drainage, apply_neem_or_horticultural_oil, introduce_predatory_insects]).

pest_disease(turmeric,
    [root_rot, leaf_spot, aphids],
    [use_organic_fungicides_trichoderma, practice_crop_rotation, use_yellow_sticky_traps_for_aphids]).

pest_disease(ginger,
    [root_rot, aphids, leaf_spot],
    [apply_biofungicides_pseudomonas_fluorescens, ensure_soil_drainage, prune_infected_leaves]).

pest_disease(vanilla,
    [aphids, mealybugs, spider_mites],
    [regular_foliar_spray_of_neem_or_seaweed_extract, maintain_airflow_and_hygiene, use_insecticidal_soap_for_mites]).

% Planting method facts
planting_method(cinnamon, root_cuttings_or_saplings).
planting_method(pepper, cuttings_from_mature_vines).
planting_method(cloves, clove_buds_or_seedlings).
planting_method(cardamom, seeds_or_rhizomes).
planting_method(nutmeg, seeds_or_grafting).
planting_method(turmeric, rhizomes).
planting_method(ginger, rhizomes).
planting_method(vanilla, cuttings_of_vanilla_vines).

% Dynamic predicates to hold user input
:- dynamic user_spice/1.

% === Main Program ===
start :-
    nl,
    write('=== Spice Cultivation Advisor ==='), nl,
    
    write('=== Welcome to the Sri Lankan Spice Advisory System ==='), nl, nl,
    write('=== Instruction to use applications ==='), nl,
    write('This application provides expert information on the following spices:'), nl,
    write('- cinnamon'), nl,
    write('- pepper'), nl,
    write('- cloves'), nl,
    write('- cardamom'), nl,
    write('- nutmeg'), nl,
    write('- turmeric'), nl,
    write('- ginger'), nl,
    write('- vanilla'), nl, nl,

    write('>>> How to Use the System:'), nl,
    write('1. Run the appropriate predicate based on what you need:'), nl,
    write('   - zone_climate_advice.        % To get zone, temperature, rainfall info'), nl,
    write('   - planting_fertilizer_info.   % To get planting methods and fertilizer advice'), nl,
    write('   - pest_disease_solutions.     % To get pest/disease solutions for a spice'), nl,
    write('   - recommend_irrigation.       % To get irrigation method for a spice'), nl, nl,

    write('2. Enter the spice name exactly as listed above (all lowercase).'), nl,
    write('3. The system will then give you detailed and reliable advice.'), nl, nl,

    write('Note: The system supports only the above spices. Typing other names'), nl,
    write('      will result in an error or "data not found" message.'), nl, nl,

    write('Which spice do you want advice on? (e.g., cinnamon, pepper, cloves, etc.)'), nl,
    read(Spice),
    assertz(user_spice(Spice)),
    nl,
    write('--- Cultivation Details ---'), nl,
    (spice(Spice, Zone, Climate, rain(MinR, MaxR), temp(MinT, MaxT), Soil, pH(PHMin, PHMax), Irrigation, Planting, _, _, _) ->
        write('Zone: '), write(Zone), nl,
        write('Climate: '), write(Climate), nl,
        write('Rainfall Range (mm): '), write(MinR), write('-'), write(MaxR), nl,
        write('Temperature Range (C): '), write(MinT), write('-'), write(MaxT), nl,
        write('Soil Type: '), write(Soil), nl,
        write('Soil pH: '), write(PHMin), write('-'), write(PHMax), nl,
        write('Irrigation Method: '), write(Irrigation), nl,
        write('Planting Method: '), write(Planting), nl
    ;
        write('No data available for this spice.'), nl
    ),
    nl,
    write('--- Fertilizer Information ---'), nl,
    (fertilizer(Spice, _, FertilizerList, Brands, Frequency, Tips) ->
        write('Fertilizers: '), write(FertilizerList), nl,
        write('Brands: '), write(Brands), nl,
        write('Application Months/Stages: '), write(Frequency), nl,
        write('Tips: '), write(Tips), nl
    ;
        write('No fertilizer information found.'), nl
    ),
    nl,
    write('--- Pests & Diseases ---'), nl,
    (pest_disease(Spice, Pests, Solutions) ->
        write('Common Pests/Diseases: '), write(Pests), nl,
        write('Control Measures: '), write(Solutions), nl
    ;
        write('No pest/disease data available.'), nl
    ),
    nl,
    retractall(user_spice(_)),
    write('Thank you for using the Spice Cultivation Advisor!'), nl.

% ====== Program 1: Recommend Spices by Zone ======
recommend_by_zone :-
    nl,
    write('Enter your zone (wet_and_intermediate_zones / wet_zones_mid_and_high_altitudes / wet_zones_coastal_areas / wet_zones_tropical_lowlands): '),
    read(Zone),
    nl,
    write('Spices suitable for your zone: '), nl,
    spice(Spice, Zone, _, _, _, _, _, _, _, _, _, _),
    write('- '), write(Spice), nl,
    fail.
recommend_by_zone.

% ====== Program 2: Recommend Spices by Temperature Range ======
recommend_by_temperature :-
    nl,
    write('Enter minimum temperature (°C): '),
    read(MinT),
    write('Enter maximum temperature (°C): '),
    read(MaxT),
    nl,
    write('Spices suitable for the given temperature range:'), nl,
    spice(Spice, _, _, _, temp(TempMin, TempMax), _, _, _, _, _, _, _),
    TempMin =< MinT,
    TempMax >= MaxT,
    write('- '), write(Spice), nl,
    fail.
recommend_by_temperature.

% ====== Program 3: Recommend Spices by Rainfall ======
recommend_by_rainfall :-
    nl,
    write('Enter minimum annual rainfall (mm): '),
    read(MinRain),
    write('Enter maximum annual rainfall (mm): '),
    read(MaxRain),
    nl,
    write('Spices suitable for the given rainfall range:'), nl,
    spice(Spice, _, _, rain(RMin, RMax), _, _, _, _, _, _, _, _),
    RMin =< MinRain,
    RMax >= MaxRain,
    write('- '), write(Spice), nl,
    fail.
recommend_by_rainfall.

% ====== Program 4: Recommend Spices by Climate ======
recommend_by_climate :-
    nl,
    write('Enter your climate type (e.g., tropical_humid): '),
    read(Climate),
    nl,
    write('Spices suitable for your climate:'), nl,
    spice(Spice, _, Climate, _, _, _, _, _, _, _, _, _),
    write('- '), write(Spice), nl,
    fail.
recommend_by_climate.

% ====== Program 5: Recommend Spices planting methods ======
planting_methods :-
    nl,
    write('Enter the name of the spice (lowercase, e.g., cinnamon): '), nl,
    read(Spice),
    assertz(current_spice(Spice)),
    ( planting_method(Spice, Method) ->
        write('Recommended planting method: '), write(Method), nl
    ;
        write('Sorry, no data found for this spice.'), nl
    ),
    retractall(current_spice(_)).

% ====== Program 6: Recommend fertilizer for spices ======
fertilizer_assistant :-
    nl,
    write('Enter the name of the spice (lowercase, e.g., ginger): '), nl,
    read(Spice),
    assertz(current_spice(Spice)),
    ( fertilizer(Spice, Nutrients, Types, Brands, Times, Tips) ->
        write('Nutrients required: '), write(Nutrients), nl,
        write('Fertilizer types: '), write(Types), nl,
        write('Local brands: '), write(Brands), nl,
        write('Application times: '), write(Times), nl,
        write('Tips: '), write(Tips), nl
    ;
        write('Sorry, no fertilizer data found for that spice.'), nl
    ),
    retractall(current_spice(_)).

% ====== Program 7: Recommend diseases and their solution for spices ======
pests_and_solutions :-
    nl,
    write('=== Spice Pest & Disease Helper ==='), nl,
    write('Enter the name of the spice (e.g., ginger): '), nl,
    read(Spice),
    assertz(current_spice(Spice)),
    
    % Show pests and diseases
    ( pest_disease(Spice, Pests, Solutions) ->
        write('Known pests and diseases for '), write(Spice), write(': '), nl,
        write(Pests), nl, nl,
        write('Recommended solutions: '), nl,
        write(Solutions), nl
    ;
        write('Sorry, we have no pest or solution data for this spice.'), nl
    ),
    
    % Cleanup
    retractall(current_spice(_)).

% ====== Program 8: Recommend irrigation methods for spices ======
recommend_irrigation :-
    nl,
    write('=== Spice Irrigation Recommender ==='), nl,
    write('Enter the name of the spice (e.g., turmeric): '), nl,
    read(Spice),
    assertz(current_spice(Spice)),

    ( spice(Spice, _, _, _, _, _, _, Irrigation, _, _, _, _) ->
        write('Recommended irrigation method for '), write(Spice), write(' is: '), nl,
        write(Irrigation), nl
    ;
        write('Sorry, no irrigation data available for that spice.'), nl
    ),

    % Cleanup
    retractall(current_spice(_)).

% File handling - Save output to a text file.

save_spice_advice_to_file(Spice) :-
    open('output.txt', write, Stream),
    (spice(Spice, Zone, Climate, rain(MinR, MaxR), temp(MinT, MaxT), Soil, pH(PHMin, PHMax), Irrigation, Planting, _, _, _) ->
	write(Stream, '=== Spice Cultivation Advisor ==='), nl(Stream),
	nl(Stream),
        write(Stream, '--- Cultivation Details ---\n'),
        write(Stream, 'Spice: '), write(Stream, Spice), nl(Stream),
        write(Stream, 'Zone: '), write(Stream, Zone), nl(Stream),
        write(Stream, 'Climate: '), write(Stream, Climate), nl(Stream),
        write(Stream, 'Rainfall Range (mm): '), write(Stream, MinR), write(Stream, '-'), write(Stream, MaxR), nl(Stream),
        write(Stream, 'Temperature Range (C): '), write(Stream, MinT), write(Stream, '-'), write(Stream, MaxT), nl(Stream),
        write(Stream, 'Soil Type: '), write(Stream, Soil), nl(Stream),
        write(Stream, 'Soil pH: '), write(Stream, PHMin), write(Stream, '-'), write(Stream, PHMax), nl(Stream),
        write(Stream, 'Irrigation Method: '), write(Stream, Irrigation), nl(Stream),
        write(Stream, 'Planting Method: '), write(Stream, Planting), nl(Stream),
	nl(Stream)
    ;
        write(Stream, 'No data available for this spice.\n')
    ),
    close(Stream).

save_fertilizer_info_to_file(Spice) :-
    open('output.txt', append, Stream),  % append so it doesn't overwrite
    (fertilizer(Spice, _, FertilizerList, Brands, Frequency, Tips) ->
        write(Stream, '--- Fertilizer Information ---\n'),
        write(Stream, 'Spice: '), write(Stream, Spice), nl(Stream),
        write(Stream, 'Fertilizers: '), write(Stream, FertilizerList), nl(Stream),
        write(Stream, 'Brands: '), write(Stream, Brands), nl(Stream),
        write(Stream, 'Application Months/Stages: '), write(Stream, Frequency), nl(Stream),
        write(Stream, 'Tips: '), write(Stream, Tips), nl(Stream),
	nl(Stream)
    ;
        write(Stream, 'No fertilizer information found.\n')
    ),
    close(Stream).

save_pest_disease_info_to_file(Spice) :-
    open('output.txt', append, Stream),
    (pest_disease(Spice, Pests, Solutions) ->
        write(Stream, '--- Pests & Diseases ---\n'),
        write(Stream, 'Known pests and diseases for '), write(Stream, Spice), write(Stream, ': '), nl(Stream),
        write(Stream, Pests), nl(Stream),
        write(Stream, 'Recommended solutions: '), nl(Stream),
        write(Stream, Solutions), nl(Stream),
	nl(Stream)
    ;
        write(Stream, 'No pest/disease data available.\n')
    ),
    close(Stream).

save_full_report(Spice) :-
    save_spice_advice_to_file(Spice),
    save_fertilizer_info_to_file(Spice),
    save_pest_disease_info_to_file(Spice),
    write('Report saved to output.txt.\n').
