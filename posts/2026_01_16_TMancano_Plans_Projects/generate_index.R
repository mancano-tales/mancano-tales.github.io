#!/usr/bin/env Rscript
#' Gera index.qmd com resumo dos projetos agrupados por status

library(jsonlite)
library(glue)
library(purrr)

# Configuração
BASE_DIR <- here::here("posts/2026_01_16_TMancano_Plans_Projects")
PIPELINE_JSON <- file.path(BASE_DIR, "TMancano_Plans_Projects.json")
INDEX_FILE <- file.path(BASE_DIR, "index.qmd")

#' Lê pipeline
read_pipeline <- function() {
  fromJSON(PIPELINE_JSON, simplifyVector = FALSE)
}

#' Formata lista de colaboradores como string
format_collaborators <- function(collaborators) {
  if (is.null(collaborators) || length(collaborators) == 0) return("Tales Mançano")
  paste(map_chr(collaborators, ~.x$name), collapse = ", ")
}

#' Formata lista de pessoas para envolver
format_people_to_involve <- function(people) {
  if (is.null(people) || length(people) == 0) return(NULL)
  paste(map_chr(people, ~.x$name), collapse = ", ")
}

#' Gera seção para um status específico
generate_status_section <- function(projects, status_value, section_title, emoji) {
  
  filtered <- projects[map_lgl(projects, ~.x$status == status_value)]
  
  if (length(filtered) == 0) {
    return(glue("\n## {emoji} {section_title}\n\n_Nenhum projeto neste status._\n"))
  }
  
  items <- map_chr(filtered, function(proj) {
    
    # Info básica
    collabs <- format_collaborators(proj$collaborators)
    to_involve <- format_people_to_involve(proj$people_to_involve)
    
    # Montar bullet points condicionalmente
    info_lines <- c()
    
    info_lines <- c(info_lines, sprintf("   - **Type:** %s", proj$type))
    info_lines <- c(info_lines, sprintf("   - **Authors:** %s", collabs))
    
    if (!is.null(to_involve)) {
      info_lines <- c(info_lines, sprintf("   - **People to Involve:** %s", to_involve))
    }
    
    if (!is.null(proj$project_state) && proj$project_state != "") {
      info_lines <- c(info_lines, sprintf("   - **State:** %s", proj$project_state))
    }
    
    if (!is.null(proj$target_journal) && !is.null(proj$target_journal$name)) {
      journal_link <- if (!is.null(proj$target_journal$url)) {
        sprintf("[%s](%s)", proj$target_journal$name, proj$target_journal$url)
      } else {
        proj$target_journal$name
      }
      info_lines <- c(info_lines, sprintf("   - **Target Journal:** %s", journal_link))
    }
    
    if (!is.null(proj$next_step) && proj$next_step != "") {
      info_lines <- c(info_lines, sprintf("   - **Next Step:** %s", proj$next_step))
    }
    
    if (!is.null(proj$prospective_deadline)) {
      info_lines <- c(info_lines, sprintf("   - **Deadline:** %s", proj$prospective_deadline))
    }
    
    if (!is.null(proj$main_argument) && proj$main_argument != "") {
      info_lines <- c(info_lines, sprintf("   - **Main Argument:** %s", proj$main_argument))
    }
    
    if (!is.null(proj$last_updated)) {
      info_lines <- c(info_lines, sprintf("   - **Last Updated:** %s", proj$last_updated))
    }
    
    # Montar seção completa
    result <- sprintf("\n### %s\n\n%s\n", proj$title, paste(info_lines, collapse = "\n"))
    return(result)
  })
  
  section_content <- paste(items, collapse = "\n---\n")
  
  return(sprintf("\n## %s %s\n%s\n", emoji, section_title, section_content))
}

#' Gera index completo
generate_index <- function() {
  
  data <- read_pipeline()
  projects <- data$research_projects
  
  # Contagens por status
  status_counts <- table(map_chr(projects, ~.x$status))
  
  # Frontmatter
  frontmatter <- sprintf('---
title: "Research Pipeline - Tales Mançano"
subtitle: "Planning Papers and Publications"
description: "Repositório central de ideias, working papers e prospecções de artigos"
date: "%s"
author: "Tales Mançano"
categories: [Planning, Research, Pipeline]
draft: true
toc: true
toc-depth: 2
---

## 📊 Overview

Sistema de gestão de projetos acadêmicos com **%d projetos** em diferentes estágios.

**Last Pipeline Update:** %s

### Projects by Status

', format(Sys.Date(), "%Y-%m-%d"), length(projects), data$metadata$last_backup)
  
  # Adicionar contagem por status
  status_table <- paste(sprintf("- **%s:** %d", names(status_counts), status_counts), collapse = "\n")
  frontmatter <- paste0(frontmatter, status_table, "\n\n---\n")
  
  # Gerar seções por status
  sections <- list(
    generate_status_section(projects, "Work in Progress", "Work in Progress", "🟢"),
    generate_status_section(projects, "Prospective", "Prospective Projects - Next in Line", "🟡"),
    generate_status_section(projects, "Prospective Idea", "Prospective Ideas", "💡"),
    generate_status_section(projects, "On Hold", "Working Papers on Hold", "🟠"),
    generate_status_section(projects, "Interview", "Interviews", "🎤"),
    generate_status_section(projects, "Abandoned", "Abandoned Projects", "🔴"),
    generate_status_section(projects, "Submitted", "Submitted for Review", "📮"),
    generate_status_section(projects, "Published", "Published", "✅")
  )
  
  # Footer
  footer <- sprintf('

---

## 🔧 System Info

Este pipeline é gerenciado através de um arquivo JSON centralizado (`TMancano_Plans_Projects.json`).

**Workflow:**
1. Editar JSON para adicionar/atualizar projetos
2. Executar `generate_index.R` para atualizar esta página
3. Render do site com `quarto render`

**Total de projetos:** %d  
**Schema version:** %s  
**Generated:** %s
', length(projects), data$schema_version, format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  
  # Combinar tudo
  full_content <- paste0(
    frontmatter,
    paste(sections, collapse = "\n"),
    footer
  )
  
  return(full_content)
}

#' Função principal
main <- function() {
  cat("=================================================\n")
  cat("Research Pipeline - Index Generator\n")
  cat("=================================================\n\n")
  
  if (!file.exists(PIPELINE_JSON)) {
    stop("❌ ERRO: Arquivo não encontrado: ", PIPELINE_JSON)
  }
  
  cat("✓ Lendo:", PIPELINE_JSON, "\n")
  
  # Gerar conteúdo
  content <- generate_index()
  
  # Escrever arquivo
  writeLines(content, INDEX_FILE, useBytes = TRUE)
  
  cat("✓ Index gerado:", INDEX_FILE, "\n\n")
  cat("✅ Concluído!\n")
  cat("\nPara visualizar, execute:\n")
  cat("  quarto preview", INDEX_FILE, "\n")
}

if (!interactive()) {
  main()
} else {
  cat("Script carregado! Execute main() para gerar index.qmd\n")
}
