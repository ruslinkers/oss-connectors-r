library(tidyverse)
library(dplyr)
library(reactable)
library(htmltools)
library(openxlsx)
knitr::opts_chunk$set(echo=FALSE, message=FALSE)

# Load data
# read.csv("nov23-all.csv",stringsAsFactors = FALSE) -> linkers
read.xlsx("ossetic_linkers_syntax.xlsx", na.strings=c("NA","")) -> linkers

normalize_factor <- function(f) {
  out <- str_to_lower(f)
  out <- sub(":","",out)
  out <- gsub(" +"," ",out)
  return(out)
}

normalize_clause.pos <- function(f) {
  out <- sub(" зависимой клаузы","",f)
  out <- sub("постпозиция и препозиция","препозиция и постпозиция",out)
  out <- sub("свободный","свободная",out)
  return(out)
}

linebreaks <- function(x) {
  return(gsub("&#10;","<br>",x))
}

# Change POS, semantic fields, classes to factors
linkers %>% 
  mutate(
    across(all_of(names(.)), trimws)
  ) %>%
  separate_wider_delim('Позиция.коррелята', 
                       delim="&#10;", 
                       names=c('correl.pos','correl.pos.ex'),
                       too_few='align_start',
                       too_many='merge') %>%
  separate_wider_delim('Опущение.коррелята', 
                       delim="&#10;", 
                       names=c('correl.omit','correl.omit.ex'),
                       too_few='align_start',
                       too_many='merge') %>%
  separate_wider_delim('Позиция.клаузы.с.коннектором', 
                       delim="&#10;", 
                       names=c('clause.pos','clause.pos.ex'),
                       too_few='align_start',
                       too_many='merge') %>%
  separate_wider_delim('Позиция.коннектора.в.клаузе', 
                       delim="&#10;", 
                       names=c('conn.pos','conn.pos.ex'),
                       too_few='align_start',
                       too_many='merge') %>%
  separate_wider_delim('Модификация.коррелята', 
                       delim="&#10;", 
                       names=c('correl.mod','correl.mod.ex'),
                       too_few='align_start',
                       too_many='merge') %>%
  separate_wider_delim('Фокусирование', 
                       delim="&#10;", 
                       names=c('focus','focus.ex'),
                       too_few='align_start',
                       too_many='merge') %>%
  separate_wider_delim('Независимое.употребление.клаузы.с.коннектором', 
                       delim="&#10;", 
                       names=c('clause.indep','clause.indep.ex'),
                       too_few='align_start',
                       too_many='merge') %>%
  separate_wider_delim('Иллокутивное.употребление', 
                       delim="&#10;", 
                       names=c('illoc','illoc.ex'),
                       too_few='align_start',
                       too_many='merge') %>%
  mutate(
    # id = as.numeric(id),
    marker = as.character(marker),
    semfield = as.factor(normalize_factor(sem.field)),    
    gloss.ru = as.character(gloss.ru),
    corr = as.character(corr),
    example = as.character(example),
    tr.ru = as.character(tr.ru),
    clause.pos = as.factor(normalize_clause.pos(normalize_factor(clause.pos))),
    clause.pos.ex = as.character(linebreaks(clause.pos.ex)),
    conn.pos = as.factor(normalize_factor(conn.pos)),    
    conn.pos.ex = as.character(linebreaks(conn.pos.ex)),
    correl.pos = as.factor(normalize_factor(correl.pos)),
    correl.pos.ex = as.character(linebreaks(correl.pos.ex)),
    correl.omit = as.factor(normalize_factor(correl.omit)),
    correl.omit.ex = as.character(linebreaks(correl.omit.ex)),
    correl.mod = as.factor(normalize_factor(correl.mod)),
    correl.mod.ex = as.character(linebreaks(correl.mod.ex)),
    focus = as.factor(normalize_factor(focus)),
    focus.ex = as.character(linebreaks(focus.ex)),
    clause.indep = as.factor(normalize_factor(clause.indep)),
    clause.indep.ex = as.character(linebreaks(clause.indep.ex)),
    illoc = as.factor(normalize_factor(illoc)),
    illoc.ex = as.character(linebreaks(illoc.ex)),
    .keep = 'unused',
  ) %>%
  select(marker,
         semfield,
         gloss.ru,
         corr,
         example,
         tr.ru,
         clause.pos,
         clause.pos.ex,
         conn.pos,
         conn.pos.ex,
         correl.pos,
         correl.pos.ex,
         correl.omit,
         correl.omit.ex,
         correl.mod,
         correl.mod.ex,
         focus,
         focus.ex,
         clause.indep,
         clause.indep.ex,
         illoc,
         illoc.ex
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

filterList <- function(values, name) {
  tags$select(
    # Set to undefined to clear the filter
    onchange = sprintf("Reactable.setFilter('linkertbl', '%s', event.target.value || undefined)", name),
    # "All" has an empty value to clear the filter, and is the default option
    tags$option(value = "", "Все"),
    lapply(sort(unique(values)), tags$option),
    "aria-label" = sprintf("Filter %s", name),
    style = "width: 100%; height: 28px;"
  )
}

detailsFunc <- function(index, df) {
  htmltools::div(
    htmltools::p(
      htmltools::tags$b("Другие части речи: "), 
      df[index, ]$other_pos),
    htmltools::p(
      htmltools::tags$b("Другие значения: "), 
      df[index, ]$other_senses))
}