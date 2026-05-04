# Duckstruct documentation site

This directory contains the **end-user** Duckstruct documentation, built with
[Docusaurus](https://docusaurus.io/docs) v3.

## Local development

```bash
cd docs
npm install
npm start
```

## Production build

```bash
cd docs
npm ci
npm run build
```

The static site output is written to `docs/build`.

## GitHub Pages deployment

This repository deploys the docs using **GitHub Actions** (recommended by
Docusaurus for GitHub Pages). After merging to `main`, the workflow publishes the
site to GitHub Pages.

Repository setting:

- GitHub → **Settings** → **Pages** → **Build and deployment** → Source:
  **GitHub Actions**

## Manual deploy (optional)

If you need to publish from a machine interactively, Docusaurus also supports
`docusaurus deploy` (typically pushing to `gh-pages`). Prefer CI for repeatable
deploys.
