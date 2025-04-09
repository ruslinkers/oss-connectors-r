library(tidyverse)
library(dplyr)
library(reactable)
library(htmltools)
library(openxlsx)
knitr::opts_chunk$set(echo=FALSE, message=FALSE)

# Load data
# read.csv("nov23-all.csv",stringsAsFactors = FALSE) -> linkers
read.xlsx("ossetic_linkers_syntax_rev_2_0.xlsx", na.strings=c("NA","")) -> linkers

normalize_factor <- function(f) {
  out <- str_to_lower(f)
  out <- sub(":", "", out)
  out <- gsub(" +", " ", out)
  return(out)
}

normalize_clause.pos <- function(f) {
  out <- sub(" зависимой клаузы", "", f)
  out <- sub("постпозиция и препозиция","препозиция и постпозиция", out)
  out <- sub("свободный", "свободная", out)
  return(out)
}

linebreaks <- function(x) {
  return(gsub("&#10;", "<br>", x))
}

# Change POS, semantic fields, classes to factors
linkers %>% 
  mutate(
    across(all_of(names(.)), trimws)
  ) %>%
  mutate(
    marker = as.character(marker),
    semfield = as.factor(normalize_factor(sem.field)),
    gloss.ru = as.character(gloss.ru),
    corr = as.character(corr),
    example = as.character(example),
    tr.ru = as.character(tr.ru),

    clause.pos = as.factor(normalize_factor(clause.position)),
    clause.pos.ex = as.character(linebreaks(clause.position.ex)),
    clause.pos.comm = as.character(linebreaks(clause.position.comm)),

    conn.pos = as.factor(normalize_factor(linker.position)),
    conn.pos.ex = as.character(linebreaks(linker.position.ex)),
    conn.pos.comm = as.character(linebreaks(linker.position.comm)),

    correl.pos = as.factor(normalize_factor(corr.position)),
    correl.pos.ex = as.character(linebreaks(corr.position.ex)),
    correl.pos.comm = as.character(linebreaks(corr.position.comm)),

    correl.omit = as.factor(normalize_factor(no.corr)),
    correl.omit.ex = as.character(linebreaks(no.corr.ex)),
    correl.omit.comm = as.character(linebreaks(no.corr.comm)),

    correl.mod = as.factor(normalize_factor(corr.modification)),
    correl.mod.ex = as.character(linebreaks(corr.modification.ex)),

    focus = as.factor(normalize_factor(focusing)),
    focus.ex = as.character(linebreaks(focusing.ex)),

    clause.indep = as.factor(normalize_factor(independent.use)),
    clause.indep.ex = as.character(linebreaks(independent.use.ex)),
    clause.indep.comm = as.character(linebreaks(independent.use.comm)),

    illoc = as.factor(normalize_factor(speech.act.use)),
    illoc.ex = as.character(linebreaks(speech.act.use.ex)),
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
    clause.pos.comm,

    conn.pos,
    conn.pos.ex,
    conn.pos.comm,

    correl.pos,
    correl.pos.ex,
    correl.pos.comm,

    correl.omit,
    correl.omit.ex,
    correl.omit.comm,

    correl.mod,
    correl.mod.ex,

    focus,
    focus.ex,

    clause.indep,
    clause.indep.ex,
    clause.indep.comm,

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