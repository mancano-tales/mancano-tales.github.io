# Site Acadêmico — Tales Mançano

Este repositório contém o código-fonte do site acadêmico de **Tales Mançano**, construído com [Quarto](https://quarto.org/) e publicado via GitHub Pages.

## Visão geral

O site reúne:
- apresentação profissional e bio;
- publicações e produção acadêmica;
- projetos e grants;
- posts/notas;
- informações de contato.

## Stack

- **Quarto** (site estático)
- **Markdown / QMD**
- **GitHub Pages** (publicação a partir de `docs/`)

## Estrutura principal

- `_quarto.yml` → configuração global do site (navbar, footer, tema, output em `docs/`)
- `index.qmd` → página inicial
- `posts.qmd` + `posts/` → listagem e conteúdo de posts
- `publications.qmd` → página de publicações
- `projects.qmd` + `projects.yml` → projetos e financiamentos
- `contact.qmd` → página de contato
- `files/` → assets (imagens, PDF do CV, includes)
- `styles.css` / `custom.css` → estilos customizados
- `docs/` → saída renderizada usada pelo GitHub Pages

## Como rodar localmente

### 1) Instalar dependências

Instale o Quarto:

- https://quarto.org/docs/get-started/

### 2) Renderizar o site

Na raiz do repositório:

```bash
quarto render
```

### 3) Visualizar com hot reload

```bash
quarto preview
```

## Publicação

Este projeto está configurado para gerar os arquivos em `docs/` (`output-dir: docs` no `_quarto.yml`), compatível com publicação no GitHub Pages.

## Personalização rápida

- Atualize menu e metadados em `_quarto.yml`
- Edite os textos das páginas `.qmd`
- Adicione posts em `posts/`
- Atualize o CV e outros arquivos em `files/`

## Licença

Consulte o arquivo [LICENSE](LICENSE).
