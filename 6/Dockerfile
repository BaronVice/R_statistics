# ── Lab 6 ─────────────────────────────────────────────────────────────────────
# Thin per-lab Dockerfile. All shared R / system layers live in Dockerfile.base
# (built as r-lab-base:latest via `make base` from the repo root).
#
# Prefer using the repo-root Makefile instead of building this file directly:
#   make l6          → docker compose up lab6
#   make l6-build    → docker compose up --build lab6
#
FROM r-lab-base:latest

COPY lab_06.R .

CMD ["Rscript", "lab_06.R"]
