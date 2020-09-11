# ------------------------------------------------------------------------------------------------------------------------ #
#                           Script for retrieving features related to author affiliations                                  #
# ------------------------------------------------------------------------------------------------------------------------ #
#                                                                                                                          #
# Description:  Retrieve features from author affiliations                                                                 #
# Authors:      Stephan van der Zwaard [stephan_vanderzwaard@live.nl]                                                      #
# Date:         25-12-2018                                                                                                 #
# Version:      1.0                                                                                                        #
# R.version:    3.5.1 (2018-07-02) 
#                                                                                                                          #
# ------------------------------------------------------------------------------------------------------------------------ #


get_affiliations_info <- function(data_processed) {

    
  #! Unfortunately, the website structure of PubMed has changed since the beginning of May.
  
  
#--------------------------------------------------------------------------------------------------------------------------#
#                                                   Prerequisites                                                          #
#--------------------------------------------------------------------------------------------------------------------------#
      
      # Function
      numbers_only <- function(x) !grepl("\\D", x)

      # Countrylist
      countrylist <- paste(codelist$country.name.en,collapse="|")
      countrylist <- gsub("Hamburg\\|","",countrylist)
      countrylist <- gsub("Brunswick\\|","",countrylist)
      statelist   <- paste(state.name,collapse="|")
      
      
      
# DUE TO CHANGES IN THE STRUCTURE OF THE PUBMED WEBSITE IN MAY 2020, THE CODE NEEDED TO BE SLIGHTLY ALTERED.
# THEREFORE, PRIOR DATA WAS JOINED TO THE data_processed dataframe INSTEAD.
      
      
      
    #   
    # #-------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
    # #                                                            Preprocessing and obtaining affiliations                                                                     #
    # #-------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
    #   
    #   for (i in 1:dim(data_processed)[1]) {  #
    # 
    #     # Retrieve affiliations associated with the journal article [i]
    #     affiliation <- data_processed$affiliations[i][[1]]
    # 
    #     ## !! 
    #     # Determine which rows are affiliations (all rows above the row with the PMID number [which contains only numbers])
    #     # row_last    <- min(which(apply(affiliation,1,numbers_only)))-1
    # 
    #     # Create data frame with all affiliations associated with the journal article and do some initial cleaning
    #     affl        <-                      # data.frame(affiliation[c(1:row_last),]) %>%
    #                                           data.frame(affiliation) %>%
    #                                              mutate(affiliation = gsub("USA","United States",affiliation),
    #                                                     affiliation = gsub("UK","United Kingdom",affiliation),
    #                                                     affiliation = gsub("Kindgom","Kingdom",affiliation),
    #                                                     affiliation = gsub("Czech Republic","Czechia",affiliation),
    #                                                     affiliation = gsub("Albany","Albania",affiliation),
    #                                                     affiliation = gsub("México","Mexico",affiliation),
    #                                                     affiliation = gsub("; Univer","Univer",affiliation),
    #                                                     affiliation = gsub("; Research","Research",affiliation),
    #                                                     affiliation = gsub('\\.', '', affiliation),                                                                 # remove dots
    #                                                     affiliation = gsub("\\S+@\\S+","",paste0(affiliation," ")),                                                 # remove email-address
    #                                                     affiliation = paste0(str_sub(affiliation,1,nchar(affiliation)-11),
    #                                                                          gsub("[[:punct:]]","",str_sub(affiliation,nchar(affiliation)-10),nchar(affiliation))), # remove punctuation in last 10 characters of string
    #                                                     affiliation = paste0(str_sub(affiliation,1,nchar(affiliation)-11),
    #                                                                          gsub(" and","",str_sub(affiliation,nchar(affiliation)-10),nchar(affiliation)))) %>%     # remove "and" in last 10 characters of string
    #                                              mutate(affiliation = strsplit(as.character(affiliation), ";")) %>%                                                 # check for multiple affiliations separated by ;
    #                                              unnest(affiliation) %>%
    #                                              mutate(affiliation = str_trim(affiliation, side="both")) %>%
    #                                            distinct(affiliation)                                                                                              # remove duplicates based on exact matching
    # 
    #     # For several affiliations, the country name is missing and has been added manually, based on a reference the city, state, university or department described within the affiliation.
    #     # Step 1: Identifiers (partially) related to the city, state, university or department are provided for each of the countries below:
    #     affl_add <-                          data.frame(Norway         = "Norwegian|Oslo|Atlantis Medical University College",
    #                                                     UK             = "Oxford|Exeter|Kent|Ulster|Berkshire|London|Newcastle|Manchester|University of Bradford|Loughborough|Cambridge University|Glasgow",
    #                                                     USA            = "Seattle|Stanford|Vanderbilt|PA|CA|WA|NY|Boston|Duke|Abboud|Drexel|Emory|Chicago|Pittsburgh|Jefferson|Departments of Biology|Howard Hughes|Department of Anatomy and Cell Biology, School of Dental Medicine|Christiana Care|Columbia MO|Cincinnati|Augusta|Departments of Applied Physiology and Kinesiology|Departments of Nutrition and Exercise Physiology|Department of Medicine, School of Medicine|East Carolina|Fraternal Order|FDA|S Army Research|S. Army Research|Louisville|Department of Physical Therapy and Rehabilitation Science|Department of Animal Sciences|Department of Physiology and Neurobiology|Departments of Pediatrics and Biomedical Engineering|Institute for Exercise and Environmental Medicine",
    #                                                     Japan          = "Waseda|Kyoto|Osaka|Shiga|Matsumoto|Tokyo|Toyo",
    #                                                     Denmark        = "Copenhagen|Aarhus University|Departments of Biomedical Sciences",
    #                                                     Brazil         = "Porto Alegre|São Paulo|Sao Paulo|Sergipe|Brasilia|Santa Catarina|Vitória|Minas Gerais",
    #                                                     Australia      = "Melbourne|Baker Heart and Diabetes Institute|Cowan University|James Cook University|Sydney|Queensland|South Wales|Prince of Wales|Flinders University|Sport and Exercise Science, School of Science and Health",
    #                                                     Slovenia       = "Jozef Stefan",
    #                                                     Germany        = "Giessen|Tuebingen|Tübingen|Aachen|Munich|Hamburg|Universität Kiel|Simplana GmbH",
    #                                                     Hungary        = "Debrecen",
    #                                                     Belgium        = "Ghent|Brussels|Leuven",
    #                                                     Netherlands    = "Maastricht|Eindhoven|Leiden|Amsterdam|Rotterdam|Vrije Universiteit Medical Center",
    #                                                     Switzerland    = "Lausanne|Berne|Zurich",
    #                                                     NZ             = "Massey",
    #                                                     France         = "Paris|Inserm|Montpellier|Clermont Ferrand|Movement To Health",
    #                                                     Russia         = "Moscow|Center for Hyperbaric Medicine and Environmental Physiology, and Departments of Anesthesiology",
    #                                                     Italy          = "Verona|Torino",
    #                                                     Israel         = "Jerusalem",
    #                                                     South_Korea    = "Seoul|Daegu",
    #                                                     Slovakia       = "Slovak",
    #                                                     Canada         = "Waterloo|British Columbia|Vancouver|Calgary|Alberta|Dalhousie|Fredericton|St Paul's Hospital|Department of Medicine, University and Division of Respirology|School of Kinesiology and Health Science",
    #                                                     Thailand       = "Bangkok",
    #                                                     Finland        = "Helsinki|University of Oulu|Jyväskylä")
    # 
    #     # Step 2: Country names are added if these are missing based on the dentifiers described above.
    #     affl <- affl %>% filter(affiliation != "3" & affiliation != "Department of Physiology" & affiliation != "Pulmo Trace Inc" & affiliation != "School of Kinesiology and Health Science and." & affiliation != "Graduate Institute of Clinical Medical Science") %>%
    #                      mutate(affiliation = case_when(grepl(affl_add$Norway,      affiliation)   &  !str_detect(affiliation,countrylist)  ~  paste0(affiliation,", Norway", sep=""),
    #                                                     grepl(affl_add$UK,          affiliation)   &  !str_detect(affiliation,countrylist)  ~  paste0(affiliation,", United Kingdom", sep=""),
    #                                                     grepl(affl_add$USA,         affiliation)   &  !str_detect(affiliation,countrylist)  ~  paste0(affiliation,", United States",sep=""),
    #                                                     grepl(affl_add$Japan,       affiliation)   &  !str_detect(affiliation,countrylist)  ~  paste0(affiliation,", Japan",sep=""),
    #                                                     grepl(affl_add$Denmark,     affiliation)   &  !str_detect(affiliation,countrylist)  ~  paste0(affiliation,", Denmark",sep=""),
    #                                                     grepl(affl_add$Brazil,      affiliation)   &  !str_detect(affiliation,countrylist)  ~  paste0(affiliation,", Brazil",sep=""),
    #                                                     grepl(affl_add$Australia,   affiliation)   &  !str_detect(affiliation,countrylist)  ~  paste0(affiliation,", Australia",sep=""),
    #                                                     grepl(affl_add$Slovenia,    affiliation)   &  !str_detect(affiliation,countrylist)  ~  paste0(affiliation,", Slovenia",sep=""),
    #                                                     grepl(affl_add$Germany,     affiliation)   &  !str_detect(affiliation,countrylist)  ~  paste0(affiliation,", Germany",sep=""),
    #                                                     grepl(affl_add$Hungary,     affiliation)   &  !str_detect(affiliation,countrylist)  ~  paste0(affiliation,", Hungary",sep=""),
    #                                                     grepl(affl_add$Belgium,     affiliation)   &  !str_detect(affiliation,countrylist)  ~  paste0(affiliation,", Belgium",sep=""),
    #                                                     grepl(affl_add$Netherlands, affiliation)   &  !str_detect(affiliation,countrylist)  ~  paste0(affiliation,", Netherlands",sep=""),
    #                                                     grepl(affl_add$Switzerland, affiliation)   &  !str_detect(affiliation,countrylist)  ~  paste0(affiliation,", Switzerland",sep=""),
    #                                                     grepl(affl_add$NZ,          affiliation)   &  !str_detect(affiliation,countrylist)  ~  paste0(affiliation,", New Zealand",sep=""),
    #                                                     grepl(affl_add$France,      affiliation)   &  !str_detect(affiliation,countrylist)  ~  paste0(affiliation,", France",sep=""),
    #                                                     grepl(affl_add$Russia,      affiliation)   &  !str_detect(affiliation,countrylist)  ~  paste0(affiliation,", Russia",sep=""),
    #                                                     grepl(affl_add$Italy,       affiliation)   &  !str_detect(affiliation,countrylist)  ~  paste0(affiliation,", Italy",sep=""),
    #                                                     grepl(affl_add$Israel,      affiliation)   &  !str_detect(affiliation,countrylist)  ~  paste0(affiliation,", Israel",sep=""),
    #                                                     grepl(affl_add$South_Korea, affiliation)   &  !str_detect(affiliation,countrylist)  ~  paste0(affiliation,", South Korea",sep=""),
    #                                                     grepl(affl_add$Slovakia,    affiliation)   &  !str_detect(affiliation,countrylist)  ~  paste0(affiliation,", Slovakia",sep=""),
    #                                                     grepl(affl_add$Canada,      affiliation)   &  !str_detect(affiliation,countrylist)  ~  paste0(affiliation,", Canada",sep=""),
    #                                                     grepl(affl_add$Thailand,    affiliation)   &  !str_detect(affiliation,countrylist)  ~  paste0(affiliation,", Thailand",sep=""),
    #                                                     grepl(affl_add$Finland,     affiliation)   &  !str_detect(affiliation,countrylist)  ~  paste0(affiliation,", Finland",sep=""),
    #                                                     TRUE ~ affiliation))
    # 
    #     # Step 3: Some affiliations have no countries and recognisable identifiers. These require manual adding of the country names based on DOI-numbers.
    #     affl <- affl %>% mutate(affiliation = case_when(grepl("Department of Integrative Physiology",                         affiliation)   & data_processed$doi[i] == "https://doi.org/10.1152/japplphysiol.00103.2016" ~ paste0(affiliation,", Japan",sep=""),
    #                                                     grepl("Division of Respiratory Disease",                              affiliation)   & data_processed$doi[i] == "https://doi.org/10.1152/japplphysiol.00326.2013" ~ paste0(affiliation,", Japan",sep=""),
    #                                                     grepl("Centre for Heart, Lung and Vascular Health",                   affiliation)   & data_processed$doi[i] == "https://doi.org/10.1152/japplphysiol.00667.2015" ~ paste0(affiliation,", Canada",sep=""),
    #                                                     grepl("Department of Obstetrics and Gynecology",                      affiliation)   & data_processed$doi[i] == "https://doi.org/10.1152/japplphysiol.00411.2015" ~ paste0(affiliation,", United States",sep=""),
    #                                                     grepl("Department of Mechanical and Industrial Engineering",          affiliation)   & data_processed$doi[i] == "https://doi.org/10.1152/japplphysiol.00113.2013" ~ paste0(affiliation,", United States",sep=""),
    #                                                     grepl("Department of Internal Medicine",                              affiliation)   & data_processed$doi[i] == "https://doi.org/10.1152/japplphysiol.00981.2015" ~ paste0(affiliation,", United States",sep=""),
    #                                                     grepl("Department of Pediatrics",                                     affiliation)   & data_processed$doi[i] == "https://doi.org/10.1152/japplphysiol.00004.2014" ~ paste0(affiliation,", United States",sep=""),
    #                                                     grepl("Division of Pulmonary, Critical Care and Sleep Medicine",      affiliation)   & data_processed$doi[i] == "https://doi.org/10.1152/japplphysiol.00975.2013" ~ paste0(affiliation,", United States",sep=""),
    #                                                     grepl("Department of Biomedical Sciences, College of Health Sciences",affiliation)   & data_processed$doi[i] == "https://doi.org/10.1152/japplphysiol.00132.2017" ~ paste0(affiliation,", United States",sep=""),
    #                                                     grepl("Departments of General Biology",                               affiliation)   & data_processed$doi[i] == "https://doi.org/10.1152/japplphysiol.00915.2014" ~ paste0(affiliation,", Brazil",sep=""),
    #                                                     grepl("Center for Environmental Medicine, Asthma, and Lung Biology",  affiliation)   & data_processed$doi[i] == "https://doi.org/10.1152/japplphysiol.00404.2014" ~ paste0(affiliation,", United States",sep=""),
    #                                                     grepl("Incarda Therapeutics",                                         affiliation)   & data_processed$doi[i] == "https://doi.org/10.1152/japplphysiol.00916.2017" ~ paste0(affiliation,", United States",sep=""),
    #                                                     TRUE ~ affiliation))
    # 
    # 
    #     # Remove duplicate affiliations based on fuzzy matching (<2.5% string difference)
    #     similar <- matrix(NA,dim(affl)[1],dim(affl)[1])
    # 
    #     for (j in 1:dim(affl)[1]) {
    # 
    #       for (k in 1:dim(affl)[1]) {
    # 
    #         if (k!= j) {
    # 
    #             if (length(agrep(affl[j,], affl[k,], ignore.case = TRUE, value = FALSE, max.distance = 0.025)) !=0) {
    # 
    #               similar[j,k] <- k
    #         }}
    #     }}
    # 
    #     rm_rows <- unique(similar[upper.tri(similar)])
    #     rm_rows <- na.omit(rm_rows)                         #find duplicate rows
    #     if (length(rm_rows) == 0 ) {rm_rows = 0}
    #     affl <- affl %>% filter(!row_number() %in% rm_rows) #remove duplicate rows
    # 
    #     # Find countries for each affiliation based on countrylist.
    #     affl <- affl %>% mutate(country = case_when(str_detect(affiliation,statelist)               ~ "United States",  # if USA state is mentioned and not the country, then United States is added as country.
    #                                                 str_detect(affiliation,countrylist) ~ str_extract(affiliation,countrylist), # Select country associated with affiliation.
    #                                                 TRUE ~ ""))
    # 
    #     # Get continent code of affiliated countries.
    #     affl[,"continents"] = countrycode(sourcevar    = affl[,"country"],
    #                                       origin       = "country.name",
    #                                       destination  = "continent")
    # 
    #     # Divide into seven-continent model (north and south americas separated)
    #     south_america_countries <- c('Brazil|Colombia|Argentina|Peru|Venezuela|Chile|Ecuador|Bolivia|Paraguay|Uruguay|Guyana|Suriname|French Guiana|Falkland Islands')
    #     affl <- affl %>% mutate(continents = case_when(continents == "Americas" &  str_detect(south_america_countries,country) ~ "South America",
    #                                                    continents == "Americas" & !str_detect(south_america_countries,country) ~ "North America",
    #                                                    TRUE ~ continents))
    # 
    #     # State when no affiliation is present.
    #     no_affl <- ifelse(row_last == 0 | all(affl$affiliation == ""), "yes","no")
    # 
    #     #   printed_text = affl %>% filter(is.na(continents)) %>% select(affiliation) %>% unnest()
    #     #   print(ifelse(nrow(printed_text)>0,paste(i,":",printed_text),""))
    #     # }
    # 
    #     # Extract relevant features from the affiliations data.
    #     d_affiliations <-                     data.frame(doi                       = data_processed$doi[i],
    #                                                      collab_nr_affl            = ifelse(no_affl == "yes", 0, dim(affl)[1]),
    #                                                      collab_countries          = ifelse(no_affl == "yes", "", paste(affl$country,collapse = ",")),
    #                                                      collab_continents         = ifelse(no_affl == "yes", "", paste(affl$continents,collapse = ","))) %>%
    #                                               mutate(collab_nr_countries       = ifelse(no_affl == "yes", 0,length(unique(affl$country))),
    #                                                      collab_nr_continents      = ifelse(no_affl == "yes", 0,length(unique(affl$continents))),
    #                                                      collab_africa             = ifelse(str_detect(collab_continents,"Africa"),1,0),
    #                                                      collab_north_america      = ifelse(str_detect(collab_continents,"North America"),1,0),
    #                                                      collab_south_america      = ifelse(str_detect(collab_continents,"South America"),1,0),
    #                                                      collab_asia               = ifelse(str_detect(collab_continents,"Asia"),1,0),
    #                                                      collab_europe             = ifelse(str_detect(collab_continents,"Europe"),1,0),
    #                                                      collab_oceania            = ifelse(str_detect(collab_continents,"Oceania"),1,0),
    #                                                      collab_international      = ifelse(collab_nr_countries  >= 2,1,0),
    #                                                      collab_intercontinental   = ifelse(collab_nr_continents >= 2,1,0),
    #                                                      collab_usa                = ifelse(str_detect(collab_countries,"United States"),1,0),
    #                                                      collab_canada             = ifelse(str_detect(collab_countries,"Canada"),1,0),
    #                                                      collab_australia          = ifelse(str_detect(collab_countries,"Australia"),1,0),
    #                                                      collab_uk                 = ifelse(str_detect(collab_countries,"United Kingdom"),1,0),
    #                                                      collab_japan              = ifelse(str_detect(collab_countries,"Japan"),1,0),
    #                                                      collab_france             = ifelse(str_detect(collab_countries,"France"),1,0),
    #                                                      collab_denmark            = ifelse(str_detect(collab_countries,"Denmark"),1,0),
    #                                                      collab_brazil             = ifelse(str_detect(collab_countries,"Brazil"),1,0),
    #                                                      collab_germany            = ifelse(str_detect(collab_countries,"Germany"),1,0),
    #                                                      collab_netherlands        = ifelse(str_detect(collab_countries,"Netherlands"),1,0)
    #                                               )
    # 
    #     # Merge affiliations data for all the individual articles
    #     if (i == 1) {data_affiliations <- d_affiliations
    #     } else {     data_affiliations <- rbind(data_affiliations,d_affiliations)
    #     }
    #   }
    #   
    #   # Determine top-10 most frequently reported countries within the affiliations
    #   countries <- str_split(paste0(data_affiliations$collab_countries,collapse=","),",") %>% unlist() %>% data.frame()
    #   colnames(countries) <- "country"
    #   top10 <- countries %>% group_by(country) %>% summarise(n=n()) %>% arrange(desc(n)) %>% ungroup() %>% filter(country != "") %>% head(10)
      
  
  # ------------------------------------------------------
  # Save preprocessed data
  # ------------------------------------------------------   
  
      load("data/data_affiliations.RData")
      
      # Return object with data on affiliations
      data_processed  <- left_join(data_processed, data_affiliations, by = c("doi"))
      
      
      return(data_processed)
      
}


#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
#                                                                         End of syntax                                                                                   #
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------#