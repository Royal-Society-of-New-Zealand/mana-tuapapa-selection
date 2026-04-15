# mana-tuapapa-fellow-selection: mana_tuapapa_fellow_selection
# Authors: Amy Marshall, Jason Gush, Anna Win-Mason
# Revised from mana-tuapapa-selection-excellence.r
# mana_tuapapa_fellow_selection.r 2026-04-14

# threshold value to be determined by the panel chair
mana_tuapapa_fellow_selection <- function(dataframe, ma = 4, pa = 2, ff = 10, fe = 20, threshold = 3.5){
  # exclude applicants with scores below threshold
  dataframe <- filter(dataframe, Score >= threshold)
  
  # shuffle dataframe once only
  dataframe <- dataframe[sample(1:nrow(dataframe)),]
  
  # draw Māori fellows, i.e. Ethnicity is flagged as Maori or Maori/Pacific Peoples
  maori_fellows <- dataframe %>% filter(str_detect(Ethnicity, "Ma")) %>% slice_head(n = ma) %>% mutate(ballot = "M\u101ori")
  
  # exclude already drawn fellows from the pool
  pool <- dataframe %>% anti_join(maori_fellows, by = c("Gender", "Ethnicity", "id"))
  
  # draw Pacific Peoples fellows, i.e. Ethnicity is flagged as Pacific Peoples or Maori/Pacific Peoples
  pacific_fellows <- pool %>% filter(str_detect(Ethnicity, "Pa")) %>% slice_head(n = pa) %>% mutate(ballot = "Pacific")
  
  # join already drawn fellows
  fellows <- bind_rows(maori_fellows, pacific_fellows)
  
  # exclude already drawn fellows from the pool
  pool <- pool %>% anti_join(fellows, by = c("Gender", "Ethnicity", "id"))
  
  # determine how many more female fellows are needed
  count_female = fellows %>% filter(Gender == "F") %>% nrow()
  female_fellows_to_select = ff - count_female	
  
  # select top sorted female and gender diverse applicants until the minimum target
  # of female fellows  has been drawn
  priv_pool <- pool %>% filter(Gender == "F" | Gender == "GD") 
  head_counter = female_fellows_to_select
  females_drawn = 0
  
  while (females_drawn < female_fellows_to_select && nrow(fellows) + head_counter < fe) {
    new_fellows <- priv_pool %>% slice_head(n = head_counter) %>% mutate(ballot = "Pool")
    females_drawn <- new_fellows %>% filter(Gender == "F") %>% nrow()
    # if the slice is too small, i.e., as one or more gender diverse fellows were drawn
    # in slice, increase the slice size by one and redraw
    head_counter = head_counter + 1
  }
  
  # combine the already drawn fellows
  fellows <- bind_rows(fellows, new_fellows)
  
  # exclude already drawn fellows from the pool
  pool <- pool %>% anti_join(fellows, by = c("Gender", "Ethnicity", "id"))
  
  # draw from the top of the sort all remaining fellows
  fellows_still_to_draw = fe - nrow(fellows)
  new_new_fellows <- pool %>% slice_head(n = fellows_still_to_draw) %>% mutate(ballot = "Pool")
  
  # combine all drawn fellows
  fellows <- bind_rows(fellows, new_new_fellows)
  # exclude all drawn fellows from the pool
  unchosen_applicant_list <<- pool %>% anti_join(fellows, by = c("Gender", "Ethnicity", "id"))
  
  return(fellows)
}

