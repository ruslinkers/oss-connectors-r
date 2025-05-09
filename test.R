library(tidyverse)
library(reactable)
library(dplyr)
library(htmltools)


# Load data
read.csv("letter-a.csv",stringsAsFactors = FALSE) -> linkers

# Change POS, semantic fields, classes to factors
mutate(linkers, source = as.factor(source),
       parts.num = as.factor(parts.num),
       parts.order = as.factor(parts.order),
       position = as.factor(position),
       pos = as.factor(pos),
       pos.type = as.factor(pos.type),
       other_pos = as.factor(other_pos),
       semfield1 = as.factor(semfield1),
       subfield1 = as.factor(subsense1),
       semfield2 = as.factor(semfield2),
       subfield2 = as.factor(subsense2),
       correl.oblig = as.factor(correl.oblig),
       comp.oblig = as.factor(comp.oblig)
       ) -> linkers

custom_aggr <- "
    function(values, rows) {
      // input:
      //  - values: an array of all values in the group
      //  - rows: an array of row data values for all rows in the group (optional)
      //
      // output:
      //  - an aggregated value, e.g. a comma-separated list
      set = [...new Set(values)];
      filtered = set.filter(function(e) { return e != null; });
      return filtered.join('; ');
    }
  "

# browsable(
# tagList(
#     tags$label(
#       tags$input(
#         type = "checkbox",
#         onclick = "Reactable.setFilter('linkertbl', 'linker', event.target.checked)"
#       ),
#       "Show missing values only"
#     ),
reactable(linkers,
          defaultPageSize = 100,
          filterable = TRUE,
          searchable = FALSE, 
          showSortable = TRUE, 
          showSortIcon = TRUE,
          groupBy = c("linker", "source"),
          columns = list(
            ptclass = colDef(show = FALSE),
            other_senses = colDef(show = FALSE),
            other_pos = colDef(show = FALSE),
            subsense1 = colDef(show = FALSE),
            subsense2 = colDef(show = FALSE),
            linker = colDef(name = "Коннектор",
                            filterMethod = JS("
                              
                            ")),
            source = colDef(name = "Источник"),
            pos = colDef(name = "Часть речи",aggregate = JS(custom_aggr)),
            sense = colDef(name = "Значение"),
            semfield1 = colDef(name = "Семантическое поле 1",aggregate = JS(custom_aggr)),
            semfield2 = colDef(name = "Семантическое поле 2", aggregate = JS(custom_aggr))
          ),
          elementId = "linkertbl")
#))