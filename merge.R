source("shared.R")
library("openxlsx")

linkers %>% 
  group_by(form,semfield1_ed) %>%
  summarise_all(~ paste0(unique(.x), collapse="; ")
  ) -> newlinkers  
  # summarise(source = paste0(unique(source), collapse=", "),
  #           meaning = paste0(unique(meaning), collapse="; "),
  #           parts.num = paste0(unique(parts.num), collapse="; ")
  #           ) -> newlinkers
  
write.xlsx(newlinkers, "nov23-all-fix-merged.xlsx")

