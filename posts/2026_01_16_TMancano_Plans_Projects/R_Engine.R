#!/usr/bin/env Rscript
library(jsonlite)
library(glue)
library(purrr)
library(stringr)

BASE_DIR <- here::here("posts/2026_01_16_TMancano_Plans_Projects")
PIPELINE_JSON <- file.path(BASE_DIR, "TMancano_Plans_Projects.json")
PROJECTS_DIR <- file.path(BASE_DIR, "projects")

read_pipeline <- function() {
  fromJSON(PIPELINE_JSON, simplifyVector = FALSE)
}

extract_body <- function(file_path) {
  if (!file.exists(file_path)) return(NULL)
  content <- readLines(file_path, warn = FALSE, encoding = "UTF-8")
  content <- paste(content, collapse = "\n")
  parts <- str_split(content, "---", n = 3)[[1]]
  if (length(parts) >= 3) return(parts[3])
  return(NULL)
}

#' VERSÃO CORRIGIDA - sem usar glue dentro de glue
format_collaborators_yaml <- function(collaborators) {
  if (is.null(collaborators) || length(collaborators) == 0) return(" []")
  
  formatted <- map_chr(collaborators, function(c) {
    lines <- character(0)
    lines <- c(lines, sprintf('  - name: "%s"', c$name))
    lines <- c(lines, sprintf('    role: "%s"', c$role))
    
    if (!is.null(c$institution)) {
      lines <- c(lines, sprintf('    institution: "%s"', c$institution))
    }
    
    if (!is.null(c$contribution) && length(c$contribution) > 0) {
      contrib <- paste(sprintf('"%s"', c$contribution), collapse = ", ")
      lines <- c(lines, sprintf('    contribution: [%s]', contrib))
    }
    
    paste(lines, collapse = "\n")
  })
  
  paste0("\n", paste(formatted, collapse = "\n"))
}

format_milestones_yaml <- function(milestones) {
  if (is.null(milestones) || length(milestones) == 0) return(" []")
  
  formatted <- map_chr(milestones, function(m) {
    lines <- character(0)
    lines <- c(lines, sprintf('  - name: "%s"', m$name))
    
    deadline_val <- if (!is.null(m$deadline)) sprintf('"%s"', m$deadline) else "null"
    lines <- c(lines, sprintf('    deadline: %s', deadline_val))
    
    lines <- c(lines, sprintf('    completed: %s', tolower(as.character(m$completed))))
    
    paste(lines, collapse = "\n")
  })
  
  paste0("\n", paste(formatted, collapse = "\n"))
}

format_status_history_yaml <- function(history) {
  if (is.null(history) || length(history) == 0) return(" []")
  
  formatted <- map_chr(history, function(h) {
    lines <- character(0)
    lines <- c(lines, sprintf('  - status: "%s"', h$status))
    lines <- c(lines, sprintf('    date: "%s"', h$date))
    paste(lines, collapse = "\n")
  })
  
  paste0("\n", paste(formatted, collapse = "\n"))
}

safe_yaml <- function(x, quote = TRUE) {
  if (is.null(x)) return(" null")
  if (is.character(x) && (x == "null" || x == "")) return(" null")
  if (quote) return(sprintf(' "%s"', x))
  return(sprintf(' %s', x))
}

safe_list_yaml <- function(x) {
  if (is.null(x) || length(x) == 0) return(" []")
  items <- paste(sprintf('"%s"', x), collapse = ", ")
  sprintf(" [%s]", items)
}

format_target_journal_yaml <- function(journal) {
  if (is.null(journal) || is.null(journal$name)) return(" null")
  
  lines <- character(0)
  lines <- c(lines, sprintf('  name: "%s"', journal$name))
  
  if (!is.null(journal$url)) {
    lines <- c(lines, sprintf('  url: "%s"', journal$url))
  }
  
  paste0("\n", paste(lines, collapse = "\n"))
}

format_funding_yaml <- function(funding) {
  if (is.null(funding) || is.null(funding$source)) return(" null")
  
  lines <- character(0)
  
  source_val <- if (is.null(funding$source)) "null" else sprintf('"%s"', funding$source)
  lines <- c(lines, sprintf('  source: %s', source_val))
  
  if (!is.null(funding$amount)) {
    amount_val <- if (is.null(funding$amount)) "null" else sprintf('"%s"', funding$amount)
    lines <- c(lines, sprintf('  amount: %s', amount_val))
  }
  
  if (!is.null(funding$grant_number)) {
    grant_val <- if (is.null(funding$grant_number)) "null" else sprintf('"%s"', funding$grant_number)
    lines <- c(lines, sprintf('  grant_number: %s', grant_val))
  }
  
  paste0("\n", paste(lines, collapse = "\n"))
}

generate_qmd_template <- function(project, existing_body = NULL) {
  
  date_value <- project$created_date %||% format(Sys.Date(), "%Y-%m-%d")
  
  # Construir YAML linha por linha com sprintf (mais seguro)
  yaml_lines <- c(
    "---",
    sprintf('title: "%s"', project$title),
    sprintf('id: "%s"', project$id),
    sprintf('date: "%s"', date_value),
    sprintf('status: "%s"', project$status),
    sprintf('type: "%s"', project$type),
    sprintf('priority:%s', safe_yaml(project$priority, quote = FALSE)),
    sprintf('last_updated:%s', safe_yaml(project$last_updated, quote = FALSE)),
    sprintf('project_state:%s', safe_yaml(project$project_state)),
    sprintf('status_notes:%s', safe_yaml(project$status_notes)),
    sprintf('prospective_deadline:%s', safe_yaml(project$prospective_deadline, quote = FALSE)),
    sprintf('next_step:%s', safe_yaml(project$next_step)),
    sprintf('main_argument:%s', safe_yaml(project$main_argument)),
    sprintf('discipline:%s', safe_yaml(project$discipline)),
    sprintf('methodology:%s', safe_list_yaml(project$methodology)),
    sprintf('tags:%s', safe_list_yaml(project$tags)),
    sprintf('keywords:%s', safe_list_yaml(project$keywords)),
    sprintf('alternative_journals:%s', safe_list_yaml(project$alternative_journals)),
    sprintf('dependencies:%s', safe_list_yaml(project$dependencies)),
    sprintf('resources:%s', safe_list_yaml(project$resources)),
    sprintf('target_journal:%s', format_target_journal_yaml(project$target_journal)),
    sprintf('collaborators:%s', format_collaborators_yaml(project$collaborators)),
    sprintf('people_to_involve:%s', format_collaborators_yaml(project$people_to_involve)),
    sprintf('milestones:%s', format_milestones_yaml(project$milestones)),
    sprintf('status_history:%s', format_status_history_yaml(project$status_history)),
    sprintf('funding:%s', format_funding_yaml(project$funding)),
    sprintf('pending_tasks:%s', safe_list_yaml(project$pending_tasks)),
    "estimated_effort:",
    sprintf('  remaining_hours:%s', if (is.null(project$estimated_effort$remaining_hours)) " null" else sprintf(" %s", project$estimated_effort$remaining_hours)),
    sprintf('  complexity:%s', safe_yaml(project$estimated_effort$complexity, quote = FALSE)),
    "---"
  )
  
  frontmatter <- paste(yaml_lines, collapse = "\n")
  
  if (!is.null(existing_body)) {
    return(paste0(frontmatter, existing_body))
  }
  
  body <- sprintf('\n\n## Project Overview\n\n**Status:** %s  \n**Last Updated:** %s\n\n## Notes\n\n[Add your notes here]\n',
                  project$status,
                  project$last_updated %||% "Not tracked")
  
  return(paste0(frontmatter, body))
}

main <- function() {
  cat("=================================================\n")
  cat("Research Pipeline Generator\n")
  cat("=================================================\n\n")
  
  if (!file.exists(PIPELINE_JSON)) {
    stop("❌ ERRO: Arquivo não encontrado: ", PIPELINE_JSON)
  }
  
  data <- read_pipeline()
  projects <- data$research_projects
  
  cat("✓ Total de projetos:", length(projects), "\n\n")
  
  if (!dir.exists(PROJECTS_DIR)) {
    dir.create(PROJECTS_DIR, recursive = TRUE)
  }
  
  created <- 0
  updated <- 0
  
  for (project in projects) {
    file_path <- file.path(PROJECTS_DIR, paste0(project$id, ".qmd"))
    
    existing_body <- extract_body(file_path)
    content <- generate_qmd_template(project, existing_body)
    writeLines(content, file_path, useBytes = TRUE)
    
    if (!is.null(existing_body)) {
      updated <- updated + 1
    } else {
      created <- created + 1
    }
    cat("✓", basename(file_path), "\n")
  }
  
  cat("\n✅ Projetos criados:", created, "\n")
  cat("✅ Projetos atualizados:", updated, "\n")
}

if (!interactive()) {
  main()
} else {
  cat("Script carregado! Execute main()\n")
}
