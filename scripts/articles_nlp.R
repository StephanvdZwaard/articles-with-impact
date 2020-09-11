# ------------------------------------------------------------------------------------------------------------------------ #
#                           Script for performing natural language processing on article title and abstract                #
# ------------------------------------------------------------------------------------------------------------------------ #
#                                                                                                                          #
# Description:  Retrieve features from natural language processing on article title and abstract                           #
# Authors:      Stephan van der Zwaard [stephan_vanderzwaard@live.nl]                                                      #
# Date:         25-12-2018                                                                                                 #
# Version:      1.0                                                                                                        #
# R.version:    3.5.1 (2018-07-02) 
#                                                                                                                          #
# ------------------------------------------------------------------------------------------------------------------------ #


articles_nlp <- function(data_features) {
  
  
# ------------------------------------------------------------------------------------------------------------------------ #
#                                               Settings & dependencies                                                    #
# ------------------------------------------------------------------------------------------------------------------------ #
  
  
  # Required libraries 
  require(lattice)
  require(udpipe)
  require(zoo)
  require(readxl)
  
  
# ------------------------------------------------------------------------------------------------------------------------ #
#                                 Perform text mining based on title and abstract                                          #
# ------------------------------------------------------------------------------------------------------------------------ #
  
  
  # ------------------------ NLP model ------------------------------
  
  # Universal Part of Speech Tagging (UPOS)
  # Obtain the NLP model using udpipe
  ud_model <- udpipe_download_model(language = "english")
  ud_model <- udpipe_load_model(ud_model$file_model)
  
  # --------------------- Top-25 used words -------------------------
  
  # Retrieve the 25 most used verbs and nouns in the title.
  title_all  <- udpipe_annotate(ud_model, x = data_features$title_full, tagger = "default", parser = "none") %>% as.data.frame()
  title_verb <- title_all %>% filter(upos=="VERB") %>% select(lemma) %>% mutate(lemma = tolower(lemma)) %>% table() %>% as.data.frame() %>% arrange(desc(Freq)) %>% head(25) %>% select(1)
  title_noun <- title_all %>% filter(upos=="NOUN") %>% select(lemma) %>% mutate(lemma = tolower(lemma)) %>% table() %>% as.data.frame() %>% arrange(desc(Freq)) %>% head(25) %>% select(1)
  
  # Retrieve the 25 most used verbs and nouns in the abstract.
  abstr_all  <- udpipe_annotate(ud_model, x = data_features$abstr_full, tagger = "default", parser = "none") %>% as.data.frame()
  abstr_verb <- abstr_all %>% filter(upos=="VERB") %>% select(lemma) %>% mutate(lemma = tolower(lemma)) %>% table() %>% as.data.frame() %>% arrange(desc(Freq)) %>% head(25) %>% select(1)
  abstr_noun <- abstr_all %>% filter(upos=="NOUN") %>% select(lemma) %>% mutate(lemma = tolower(lemma)) %>% table() %>% as.data.frame() %>% arrange(desc(Freq)) %>% head(25) %>% select(1)
  
  # ------------ Manual check human/animal studies ------------------
  
  human_animal_ref <- read_excel("data/validated_human_animal.xlsx")
  
  # ----------- Perform natural language processing ----------------
  
  # set progress bar
  pb <- txtProgressBar(0, dim(data_features)[1], style = 3)
  
  for (i in 1:dim(data_features)[1]) {

      # Perform NLP on title
      nlp_title_all       <- udpipe_annotate(ud_model, x = data_features$title_full[i], tagger = "default", parser = "none") %>%  as.data.frame()     # perform NLP on title
      col_ids             <- c("ADJ","ADP","ADV","AUX","CCONJ","DET","INTJ","NOUN","NUM","PART","PRON","PROPN","PUNCT","SCONJ","SYM","VERB","X")      # mention all UPOS elements
      nlp_title           <- txt_freq(nlp_title_all$upos) %>%                                                                                         # retrieve frequency of UPOS elements for title (% of total)
                                mutate(key = paste0("title_",key)) %>%
                                select(-freq) %>%
                                spread(key,freq_pct)
      missing             <- setdiff(paste0("title_",col_ids), names(nlp_title))                                                                      # find names of UPOS elements that are missing in title
      nlp_title[missing]  <- 0                                                                                                                        # add them, filled with '0's (0% of total)
      nlp_title           <- nlp_title %>% select(sort(names(.)))
      colnames(nlp_title) <- tolower(colnames(nlp_title))
      nlp_title           <- nlp_title_all %>% 
                             mutate(mention_top25_verbs = ifelse(tolower(lemma) %in% title_verb$. & upos == "VERB",1,0),
                                    mention_top25_nouns = ifelse(tolower(lemma) %in% title_noun$. & upos == "NOUN",1,0),
                                    noun_chunks         = ifelse(upos == "NOUN" & lag(upos,1) == "NOUN" & lag(upos,2) == "NOUN",1,0)) %>%
                             summarise(title_mention_top25_verbs = max(mention_top25_verbs),
                                       title_mention_top25_nouns = max(mention_top25_nouns),
                                       title_perc_top25_verbs    = sum(mention_top25_verbs)/data_features$title_n_words[i]*100,
                                       title_perc_top25_nouns    = sum(mention_top25_nouns)/data_features$title_n_words[i]*100,
                                       title_noun_chunks         = ifelse(all(is.na(noun_chunks)),0,max(noun_chunks))) %>%
                             cbind(nlp_title) %>%
                             select(tolower(colnames(nlp_title)),everything())

      # Perform NLP on abstract
      nlp_abstr_all       <- udpipe_annotate(ud_model, x = data_features$abstr_full[i], tagger = "default", parser = "none") %>% as.data.frame()      # perform NLP on title
      nlp_abstr           <- txt_freq(nlp_abstr_all$upos) %>%                                                                                         # mention all UPOS elements
                             mutate(key = paste0("abstr_",key)) %>%                                                                                   # retrieve frequency of UPOS elements for title (% of total)
                             select(-freq) %>%
                             spread(key,freq_pct)
      missing             <- setdiff(paste0("abstr_",col_ids), names(nlp_abstr))                                                                      # find names of UPOS elements that are missing in title
      nlp_abstr[missing]  <- 0                                                                                                                        # add them, filled with '0's (0% of total)
      nlp_abstr           <- nlp_abstr %>% select(sort(names(.)))
      colnames(nlp_abstr) <- tolower(colnames(nlp_abstr))
      
      # if (nlp_abstr_all$abstr_human_study == 1 & nlp_abstr_all$abstr_animal_study == 1) {
      #   
      #   human    <- c("woman","man","human","humans","people","subjects","infant","child","patient","athlete","cyclist","participant","astronaut","runner","runners",
      #                 "diver","elderly","oldest-old","oarsman","rower","lowlander","survivor") 
      #   human_II <- c("subject","volunteer")
      #   animal   <- c("animal","rat","mouse","rabbit","horse","dog","mice","bird","pig","sheep","goat","cow","cat","fish","primate","vertebrae","mammalian","amphibian","lamb","swine")
      #   
      #   
      #   check_animal_human <- nlp_abstr_all %>%
      #     filter(sentence_id != 1 & sentence_id != max(sentence_id)) %>%     #remove first and last sentence (that likely represents background and future implementation and not experimental setup)
      #     mutate(human_study   = ifelse(tolower(lemma) %in% human | c(tolower(lemma) %in% human_II & upos == "NOUN"),1,0),
      #            animal_study  = ifelse(tolower(lemma) %in% animal,1,0)) %>%  
      #     mutate(animal_study  = ifelse(tolower(lemma) == "mammalian" & tolower(lead(lemma,1)) == "target",0, animal_study)) %>% # if mTOR is mentioned this should not be associated with animals.
      #     summarise(abstr_human_study  = max(human_study),
      #               abstr_animal_study = max(animal_study))
      #   
      #   nlp_abstr_wording$abstr_human_study  <- check_animal_human$abstr_human_study
      #   nlp_abstr_wording$abstr_animal_study <- check_animal_human$abstr_animal_study
      #   
      # }
      # results <- rbind(results,cbind(test$doi[i],nlp_abstr_wording))
      # }
      # 
      # rows_id <- results %>% filter(abstr_human_study == 1 & abstr_animal_study == 1) %>% rename(doi = `test$doi[i]`)
      # test2 <- test %>% filter(doi %in% rows_id$doi) %>% dim() #writexl::write_xlsx("test_human_animal2.xlsx")
      
      
      
      # Get information on sentences (number of sentences, mean, max, sd in number of words, noun chunks (3 consecutive nouns))
      nlp_abstr_sentences <- nlp_abstr_all %>%
                             group_by(sentence_id) %>%
                             mutate(l_sentence    = str_count(sentence,"\\w+"),
                                    noun_chunks   = ifelse(upos == "NOUN" & lag(upos,1) == "NOUN" & lag(upos,2) == "NOUN",1,0)) %>%
                             summarise(l_sentence = mean(l_sentence),
                                       n_sentence = max(sentence_id),
                                       noun_chunks= max(noun_chunks)) %>%
                             summarise(abstr_sentence_n            = max(n_sentence),
                                       abstr_sentence_length_mean  = mean(l_sentence),
                                       abstr_sentence_length_max   = max(l_sentence),
                                       abstr_sentence_length_iqr   = IQR(l_sentence),
                                       abstr_sentence_length_sd    = sd(l_sentence),
                                       abstr_noun_chunks           = ifelse(all(is.na(noun_chunks)),0,max(noun_chunks,na.rm=T)),
                                       abstr_sentences_noun_chunks = ifelse(all(is.na(noun_chunks)),0,sum(noun_chunks,na.rm=T))) 
      
      # Get information on mentioned aim/purpose (wording used and sentence in which it appears)
      nlp_abstr_wording   <- nlp_abstr_all %>%
                             group_by(sentence_id) %>%
                             filter(row_number()==1) %>%   #select setence once
                             ungroup() %>%
                             mutate(   mention_sent_aim                 = ifelse(str_detect(sentence,"aim|goal|objective|purpose"),sentence_id,NA),
                                       mention_aim                      = ifelse(str_detect(sentence,"aim"),1,0),
                                       mention_goal                     = ifelse(str_detect(sentence,"goal"),1,0),
                                       mention_objctve                  = ifelse(str_detect(sentence,"objective"),1,0),
                                       mention_purpose                  = ifelse(str_detect(sentence,"purpose"),1,0),
                                       mention_investigate              = ifelse(str_detect(sentence,"investigate"),1,0),
                                       mention_examine                  = ifelse(str_detect(sentence,"examin"),1,0),
                                       mention_this_study               = ifelse(str_detect(tolower(sentence),"this study"),1,0)) %>%
                             summarise(abstr_position_mention_aim       = ifelse(any(!is.na(mention_sent_aim)),min(mention_sent_aim, na.rm = T)/max(sentence_id)*100,101),
                                       abstr_mention_aim                = max(mention_aim),
                                       abstr_mention_goal               = max(mention_goal),
                                       abstr_mention_objctve            = max(mention_objctve),
                                       abstr_mention_investigate        = max(mention_investigate),
                                       abstr_mention_examine            = max(mention_examine),
                                       abstr_mention_purpose            = max(mention_purpose),
                                       abstr_mention_this_study         = max(mention_this_study))
                             # mutate(   abstr_mention_study_nextword     = ifelse(abstr_mention_this_study==1 & nrow(nlp_abstr_nextword) != 0,nlp_abstr_nextword$nextword,NA),
                             #           abstr_mention_study_nexlemma     = ifelse(abstr_mention_this_study==1 & nrow(nlp_abstr_nextword) != 0,nlp_abstr_nextword$nextlemma,NA))
      
      # Get information on mentioned nouns and verbs in top-25 most mentioned, indicate whether it concerns an animal or human study 
      human    <- c("woman","man","human","humans","people","subjects","infant","child","adult","patient","athlete","cyclist","participant","astronaut","runner","runners","resident",
                    "adolescent","diver","elderly","oldest-old","oarsman","rower","lowlander","survivor") 
      human_II <- c("subject","volunteer","individual","male","female")
      animal   <- c("animal","rat","mouse","rabbit","horse","dog","mice","bird","pig","sheep","goat","piglet","cow","cat","fish","primate","vertebrae","sloth","mammalian","amphibian","lamb","swine")
      
      nlp_abstr_wording2  <- nlp_abstr_all %>%
                             mutate(   human_study                      = ifelse(tolower(lemma) %in% human | c(tolower(lemma) %in% human_II & upos == "NOUN"),1,0),
                                       animal_study                     = ifelse(tolower(lemma) %in% animal,1,0),
                                       mention_top25_verbs              = ifelse(tolower(lemma) %in% abstr_verb$. & upos == "VERB",1,0),
                                       mention_top25_nouns              = ifelse(tolower(lemma) %in% abstr_noun$. & upos == "NOUN",1,0)) %>% 
                             summarise(abstr_human_study                = max(human_study),
                                       abstr_animal_study               = max(animal_study),
                                       abstr_mention_top25_verbs        = max(mention_top25_verbs),
                                       abstr_mention_top25_nouns        = max(mention_top25_nouns),
                                       abstr_perc_top25_verbs           = sum(mention_top25_verbs)/data_features$abstr_n_words[i]*100,
                                       abstr_perc_top25_nouns           = sum(mention_top25_nouns)/data_features$abstr_n_words[i]*100)

      # Do an additional check if both animal and human references are made in the abstract. Generally animal and human references are mentioned in background section or discussion/interpretation (i.e. first and last sentence of the abstract)
      if (nlp_abstr_wording2$abstr_human_study == 1 & nlp_abstr_wording2$abstr_animal_study == 1) {
        
          human    <- c("woman","man","human","humans","people","subjects","infant","child","patient","athlete","cyclist","participant","astronaut","runner","runners","resident",
                        "diver","elderly","oldest-old","oarsman","rower","lowlander","survivor") 
          human_II <- c("subject","volunteer")
          animal   <- c("animal","rat","mouse","rabbit","horse","dog","mice","bird","pig","sheep","goat","cow","cat","fish","primate","vertebrae","mammalian","amphibian","lamb","swine")
            
          check_animal_human <- nlp_abstr_all %>%
                                filter(sentence_id != 1 & sentence_id != max(sentence_id)) %>%  #remove sentences that refer to background and discussion (first and last sentence)
                                mutate(human_study   = ifelse(tolower(lemma) %in% human | c(tolower(lemma) %in% human_II & upos == "NOUN"),1,0),
                                       animal_study  = ifelse(tolower(lemma) %in% animal,1,0)) %>%  
                                mutate(animal_study  = ifelse(tolower(lemma) == "mammalian" & tolower(lead(lemma,1)) == "target",0, animal_study)) %>% 
                                summarise(abstr_human_study  = max(human_study),
                                          abstr_animal_study = max(animal_study))
          
          nlp_abstr_wording2$abstr_human_study  <- check_animal_human$abstr_human_study
          nlp_abstr_wording2$abstr_animal_study <- check_animal_human$abstr_animal_study
        
      }
      
      # Abstract with both animal and human references have been checked manually
      if (data_features$doi[i] %in% human_animal_ref$doi) {
        ref <- human_animal_ref %>% filter(doi == data_features$doi[i])
        nlp_abstr_wording2$abstr_human_study  <- ref$abstr_human_study
        nlp_abstr_wording2$abstr_animal_study <- ref$abstr_animal_study
      }
                            
                             
      # Combining all features to add.
      f_nlp <- cbind(data.frame(doi=data_features$doi[i]),
                     nlp_title,
                     nlp_abstr,
                     nlp_abstr_sentences,
                     nlp_abstr_wording,
                     nlp_abstr_wording2)

      if (i==1) {
        features_nlp <- f_nlp
      } else {
        features_nlp <- rbind(features_nlp,f_nlp)
      }
      
      # Track progress
      setTxtProgressBar(pb, i)

  }
  
  # ------------------------------------------------------
  # Return data
  # ------------------------------------------------------   

  #load("data/features_nlp.RData")

  data_features <- data_features %>% left_join(features_nlp,by=c("doi"))

  
  return(data_features)

}

##############################################################################################################################
#                                                   End of syntax                                                            #
##############################################################################################################################
