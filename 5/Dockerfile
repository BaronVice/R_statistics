# ── Lab 5 – Titanic analysis ──────────────────────────────────────────────────
# Thin per-lab Dockerfile. All shared R / system layers live in Dockerfile.base
# (built as r-lab-base:latest via `make base` from the repo root).
#
# Prefer using the repo-root Makefile instead of building this file directly:
#   make l5          → docker compose up lab5
#   make l5-build    → docker compose up --build lab5
#
FROM r-lab-base:latest

COPY lab_05_titanic.R .
COPY titanic3.csv .

CMD ["Rscript", "lab_05_titanic.R"]
