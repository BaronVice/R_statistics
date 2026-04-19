# ── Lab 3 – Diamonds analysis ─────────────────────────────────────────────────
# Thin per-lab Dockerfile. All shared R / system layers live in Dockerfile.base
# (built as r-lab-base:latest via `make base` from the repo root).
#
# Prefer using the repo-root Makefile instead of building this file directly:
#   make l3          → docker compose up lab3
#   make l3-build    → docker compose up --build lab3
#
FROM r-lab-base:latest

COPY lab3_daimonds.R .

CMD ["Rscript", "lab3_daimonds.R"]
