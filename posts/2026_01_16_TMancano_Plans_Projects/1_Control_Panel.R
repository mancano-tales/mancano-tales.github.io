#!/usr/bin/env Rscript
#' Build completo do pipeline

# 1. Limpar arquivos antigos
unlink("posts/2026_01_16_TMancano_Plans_Projects/projects/*.qmd")

# 2. Executar
source("posts/2026_01_16_TMancano_Plans_Projects/R_Engine.R")
main()


# No R console
unlink("posts/2026_01_16_TMancano_Plans_Projects/projects/*.qmd")

# Verificar se deletou
list.files("posts/2026_01_16_TMancano_Plans_Projects/projects/")
# Deve retornar: character(0)


# 1. Gerar o index.qmd
source("posts/2026_01_16_TMancano_Plans_Projects/generate_index.R")
main()

# 2. Visualizar
quarto::quarto_preview("posts/2026_01_16_TMancano_Plans_Projects/index.qmd")
