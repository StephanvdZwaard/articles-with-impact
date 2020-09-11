# ------------------------------------------------------------------------------------------------------------------------ #
#                           Script for performing natural language processing on article keywords                          #
# ------------------------------------------------------------------------------------------------------------------------ #
#                                                                                                                          #
# Description:  Retrieve features from natural language processing on article keywords                                     #
# Authors:      Stephan van der Zwaard [stephan_vanderzwaard@live.nl]                                                      #
# Date:         25-12-2018                                                                                                 #
# Version:      1.0                                                                                                        #
# R.version:    3.5.1 (2018-07-02) 
#                                                                                                                          #
# ------------------------------------------------------------------------------------------------------------------------ #

articles_keywords <- function(data_features) {
  
  
# ------------------------------------------------------------------------------------------------------------------------ #
#                                               Settings & dependencies                                                    #
# ------------------------------------------------------------------------------------------------------------------------ #
  
  
  # Required libraries
  require(lattice)
  require(udpipe)
  require(dplyr)
  
  
# ------------------------------------------------------------------------------------------------------------------------ #
#                                       Perform text mining based on keywords                                              #
# ------------------------------------------------------------------------------------------------------------------------ #
  
  
  # Universal Part of Speech Tagging (UPOS)
  ud_model <- udpipe_download_model(language = "english")
  ud_model <- udpipe_load_model(ud_model$file_model)
  
  # -- Data preprocessing --
  # s <- data_features %>% mutate(keywords = paste(keywords_1,keywords_2,keywords_3,keywords_4,keywords_5,keywords_6,sep=",")) %>% filter(str_detect(keywords,"v̇")) %>% select(doi)
  # s <- data_features %>% filter(doi %in% s$doi) %>% select(doi,keywords_1:keywords_6)
  # writexl::write_xlsx(s,"v-dots.xlsx")
  data_replace <- readxl::read_excel("v-dots.xlsx") %>% mutate_all(~ if_else(is.na(.x),"",.x))
  data_features <- data_features %>% left_join(data_replace,by="doi") %>%
                                     mutate(keywords_1 = ifelse(doi %in% data_replace$doi, keywords_1.y, keywords_1.x),
                                            keywords_2 = ifelse(doi %in% data_replace$doi, keywords_2.y, keywords_2.x),
                                            keywords_3 = ifelse(doi %in% data_replace$doi, keywords_3.y, keywords_3.x),
                                            keywords_4 = ifelse(doi %in% data_replace$doi, keywords_4.y, keywords_4.x),
                                            keywords_5 = ifelse(doi %in% data_replace$doi, keywords_5.y, keywords_5.x),
                                            keywords_6 = ifelse(doi %in% data_replace$doi, keywords_6.y, keywords_6.x)) %>% select(-c(keywords_1.x:keywords_6.x,keywords_1.y:keywords_6.y)) 
  
  
  data_features <- data_features %>% 
                   # mutate_at(.vars = vars(keywords_1:keywords_6),
                   #           .funs = list(~ifelse(doi %in% s$doi, gsub("[^[:alnum:][:blank:]?&/\\-]","",.),.))) %>% # remove special characters such as "v̇"
                   mutate(keywords = paste(keywords_1,keywords_2,keywords_3,keywords_4,keywords_5,keywords_6,sep=",")) %>%
                   mutate(keywords = gsub(" ","_",keywords)) %>%
                   mutate(keywords = gsub("-","__", keywords)) %>%
                   mutate(keywords = gsub("\\(","", keywords)) %>%
                   mutate(keywords = gsub("\\)","", keywords)) %>%
                   mutate(keywords = gsub(",",", ",keywords)) %>% 
                   mutate(keywords = gsub("maximal_oxygen_uptake|maximal_vo2max|vo2max_biomarker|inborn_vo2max|peak_oxygen_uptake|vo2__max|vo2max_capacity|vo2_max|vo2_max|cardio__respiratory_fitness|cardiorespiratory_fitness|maximal_exercise_capacity|aerobic_capacity|aerobic_fitness|cardiorespiratory_fitness","vo2max",keywords)) %>%
                   mutate(keywords = gsub("muscle_fiber_type|muscle_fiber_types|fiber_type|fiber_types|myosin_heavy_chain|fiber_type__specific|fiber__type_shift|fiber__type|myosin_heavy_chain_isoform|muscle_fiber_type_distribution|fiber_type_transformation","muscle_fiber_type",keywords)) %>%
                   mutate(keywords = gsub("pulmonary_vo2_kinetics|vo2_slow_component|vo2_kinetics_slow_component|vo2_off__kinetics|vvo2_kinetics|o2_uptake_kinetics|oxygen_uptake_dynamics","vo2_kinetics",keywords)) %>%
                   mutate(keywords = gsub("human_skeletal_muscle|skeletal_muscle","skeletal_muscle",keywords)) %>%
                   mutate(keywords = gsub("csa|muscle_cross__sectional_area","cross__sectional_area",keywords)) %>%
                   mutate(keywords = gsub("adenosine_5′__monophosphate__activated_protein_kinase","ampk",keywords)) %>%
                   mutate(keywords = gsub("mammalian_target_of_rapamycin_complex_1|mammalian_target_of_rapamycin","mtor",keywords)) %>%
                   mutate(keywords = gsub("high__density_surface_emg|_synergistic_muscles|surface_emg|surface_electromyography|surface_electromyogram|electromyographic|electromyographic_activity|electromyography_rln|electromyography_root_mean_square","electromyography",keywords)) %>%
                   mutate(keywords = gsub("o2_on__kinetics|oxygen_consumption_kinetics|oxygen_uptake_kinetics","vo2_kinetics",keywords)) %>%
                   mutate(keywords = gsub("pgc__1α|pgc__1|peroxisome_proliferator__activated_receptor__γ_coactivator_1__α|peroxisome_proliferator-activated_receptor-γ_coactivator-1α","pgc__1α",keywords)) %>%
                   mutate(keywords = gsub("phosphorus__31_magnetic_resonance_spectroscopy|31p__magnetic_resonance_spectroscopy|phosphorus_magnetic_resonance_spectroscopy","p__mrs",keywords)) %>%
                   mutate(keywords = gsub("skeletal_muscle_cells|single_muscle_fiber","muscle_fibers",keywords)) %>%
                   mutate(keywords = gsub("weight_training|resistance_exercise_training|strength_training","resistance_training",keywords)) %>%
                   mutate(keywords = gsub("high_altitude|altitude_training|simulated_altitude|high__altitude","altitude",keywords)) %>%
                   mutate(keywords = gsub("aerobic_training|endurance__exercise_training|endurance_exercise_training|moderate__intensity_continuous_training|speed_endurance_training","endurance_training",keywords)) %>%
                   mutate(keywords = gsub("aerobic_exercise","endurance_exercise",keywords)) %>%
                   mutate(keywords = gsub("muscle_atrophy","atrophy",keywords)) %>%
                   mutate(keywords = gsub("physical_inactivity","physical_activity",keywords)) %>%
                   mutate(keywords = gsub("muscle_hypertrophy","hypertrophy",keywords)) %>%
                   mutate(keywords = gsub("mitochondrial_biogenesis|mitochondrial_capacity|mitochondrial_respiration|mitochondrial_dynamics","mitochondria",keywords)) %>%
                   mutate(keywords = gsub("chronic_hypoxia|hypobaric_hypoxia","hypoxia",keywords)) %>%
                   mutate(keywords = gsub("sympathetic_nerve_activity|sympathetic_activity","sympathetic_nervous_system",keywords)) %>%
                   mutate(keywords = gsub("chronic_sleep_apnea|sleep__disordered_breathing|sleep_apneas|obstructive_sleep_apnea_syndrome|obstructive_sleep_apnea_sleep_quality","sleep_apnea",keywords)) %>%
                   mutate(keywords = gsub("nirs","near__infrared_spectroscopy",keywords)) %>%
                   mutate(keywords = gsub("ultrasound_imaging|ultrasound_modulation|ultrasonography|transcranial_doppler_ultrasound|doppler_ultrasound|quantitative_ultrasound|3d_ultrasound|ultrafast_ultrasound|
                                          ultrasound_doppler_flowmetry|speckle_tracking_ultrasound|contrast__enhanced_ultrasound|transcranial_doppler_ultrasound|high__resolution_ultrasound|2d_ultrasound|ultrasound_contrast_agents|
                                          free_hand_ultrasound|dual__frequency_ultrasound|doppler_ultrasound|ultrasound","ultrasound",keywords)) %>%
                   mutate(keywords = gsub("chf","heart_failure",keywords)) %>%
                   mutate(keywords = gsub("glucose_uptake|glucose_metabolism|glucose_kinetics|glucose_tolerance|glucose_intolerance|continuous_glucose_monitoring|glucose_transport|glucose_control","glucose",keywords)) %>%
                   mutate(keywords = gsub("muscle_fatigue|quadriceps_muscle_fatigue|skeletal_muscle_fatigue|leg_fatigue|peripheral_fatigue_marker","peripheral_fatigue",keywords)) %>%
                   mutate(keywords = gsub("arterial_blood_pressure|blood_pressure_regulation|central_blood_pressure|aortopulmonary_blood_pressure_difference","blood_pressure",keywords)) %>%
                   mutate(keywords = gsub("cerebral_blood_flow_velocity|brain_blood_flow|ccerebral_blood_flow|cranial_blood_flowregional_cerebral_blood_flow|cerebral_blood_flow_regulation|cerebral_blood_flow_measurement","cerebral_blood_flow",keywords)) %>%
                   mutate(keywords = gsub("high__intensity_interval_training|intermittent_exercise_training|interval_training|interval_sprint_training|sprint_training|sprint_interval_training","high__intensity_training",keywords)) %>%
                   mutate(keywords = gsub("skeletal_muscle_blood_flow|quadriceps_muscle_blood_flow|muscle_blood_flow_heterogeneity","muscle_blood_flow",keywords)) %>%
                   mutate(keywords = gsub("very_heavy_intensity_exercise|severe__intensity_exercise|high__intensity_interval_exercise","high__intensity_exercise",keywords)) %>%
                   mutate(keywords = gsub("capillary|capillarization|capillarity|capillary_density|muscle_capillaries|capillary__to__fiber_ratio|functional_capillary_density|capillary_density","capillaries",keywords)) %>%    
                   mutate(keywords = gsub("chronic_obstructive_pulmonary_disease","copd",keywords)) %>%
                   mutate(keywords = gsub("hit,","high__intensity_training,",keywords)) %>%
                   mutate(keywords = gsub("endurance_exercise_training","endurance_training",keywords)) %>%
                   mutate(keywords = gsub("sympathetic_nervous_system|autonomic_nervous_system|muscle_sympathetic_nervous_system|central_nervous_system|parasympathetic_nervous_system|sympathetic_nervous_system_index|central_nervous_system_oxygen_toxicity|skin_sympathetic_nervous_system|nervous_system|renal_sympathetic_nervous_system|splanchnic_sympathetic_nervous_system|diastolic_arterial_pressure__muscle_sympathetic_nervous_system_coherence|autonomic_nervous_system_diseases|cardiac_vagal_parasympathetic_nervous_system|parasympathetic_nervous_system_index","nervous_system",keywords)) %>%                                
                   mutate(keywords = gsub("obstructive_sleep_apnea|central_sleep_apnea|pathophysiology_of_sleep_apnea|obstructive_sleep_apnea_pathogenesis","sleep_apnea",keywords)) %>%
                   mutate(keywords = gsub("3he_diffusion_magnetic_resonance_imaging|cardiac_magnetic_resonance_imaging|magnetic_resonance_spectroscopy|magnetic_resonance_imaging|magnetic_resonance","mri",keywords))
   
  
  # Create frequency table from all keywords of the articles published in 2009-2018
  x             <- udpipe_annotate(ud_model, x = tolower(data_features$keywords), tagger = "default", parser = ",") %>% as.data.frame() 
  stats         <- subset(x,!upos  %in% c("PUNCT") &
                           !lemma %in% c("p")) %>%
                   mutate(token = gsub(",","",token))
  stats         <- txt_freq(stats$token)
  t50_keywords  <- stats %>% head(50) %>% select(key) %>% t()
  
  
  # Add keyword features
  for (i in 1:dim(data_features)[1]) {
   
     x  <- udpipe_annotate(ud_model, x = tolower(data_features$keywords[i]), tagger = "default", parser = "none") %>% as.data.frame() 
     x  <- subset(x,!upos  %in% c("PUNCT") & !lemma %in% c("p")) 
     
     keywords <- ifelse(t50_keywords %in% x$token, 1, 0)
     keywords <- data.frame(t(keywords))
     colnames(keywords) <- paste0("key_t50_",t50_keywords)
       
     keywords <- keywords %>%
                 mutate(key_t50_vo2_kinetics  = ifelse(any(str_detect(x$token,"vo2_kinetics")),1,0),
                        key_respiratory       = ifelse(any(str_detect(x$token,"respirat|sleep_apnea|breathing|asthma|copd|diaphragm|ventilation")),1,0),
                        key_neuromuscular     = ifelse(any(str_detect(x$token,"neuromusc|electromyography")),1,0),
                        key_muscle            = ifelse(any(str_detect(x$token,"muscle|atrophy|hypertrophy|mitochondria")),1,0),
                        key_cardiovascular    = ifelse(any(str_detect(x$token,"cardiovascular|echocardiography|vasodilation|microcirculation|blood_flow|blood_pressure|cardiac_output")),1,0),
                        key_exercise          = ifelse(any(str_detect(x$token,"exercise|exercising")),1,0),
                        key_heat              = ifelse(any(str_detect(x$token,"heat|hypertherm|thermoregulation")),1,0),
                        key_gravity           = ifelse(any(str_detect(x$token,"gravit|spaceflight")),1,0),
                        key_altitude          = ifelse(any(str_detect(x$token,"altitude")),1,0),
                        key_hypoxia           = ifelse(any(str_detect(x$token,"hypoxia")),1,0),
                        key_disease           = ifelse(any(str_detect(x$token,"disease|failure|inflamm|copd")),1,0),
                        key_aging             = ifelse(any(str_detect(x$token,"age|ageing|aging")),1,0),
                        key_obesity           = ifelse(any(str_detect(x$token,"obese|obesity")),1,0),
                        key_activity          = ifelse(any(str_detect(x$token,"physical_activity|inactivity")),1,0),
                        key_training          = ifelse(any(str_detect(x$token,"training")),1,0),
                        key_performance       = ifelse(any(str_detect(x$token,"performance")),1,0),
                        key_endurance_or_HIT_training = ifelse(any(str_detect(x$token,"endurance_training|high__intensity_training")),1,0),
                        doi             = data_features$doi[i]) %>%
                 select(doi,everything())
     
     
     # Merge affiliations data for all the individual articles
     if (i == 1) {data_keywords <- keywords
     } else {     data_keywords <- rbind(data_keywords,keywords)
     }
     
  }
  
  # ------------------------------------------------------
  # Return data
  # ------------------------------------------------------   
  
  # Return object with data on affiliations
  data_features  <- left_join(data_features, data_keywords, by = c("doi"))
  
  # Save dataframe for keyword plot
  # save(stats,x, file = paste0(getwd(),"/data/keyword_plot_0520.RData"))
  
  return(data_features)
  
}

##############################################################################################################################
#                                                   End of syntax                                                            #
##############################################################################################################################