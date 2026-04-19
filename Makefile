.PHONY: base l3 l3-build l5 l5-build l6 l6-build

# ── Base image ────────────────────────────────────────────────────────────────
# Build the shared R base image once; cached by Docker on subsequent runs.
base:
	docker build -f Dockerfile.base -t r-lab-base:latest .

# ── Lab 3 ─────────────────────────────────────────────────────────────────────
l3: base
	docker compose up lab3

l3-build: base
	docker compose up --build lab3

# ── Lab 5 ─────────────────────────────────────────────────────────────────────
l5: base
	docker compose up lab5

l5-build: base
	docker compose up --build lab5

# ── Lab 6 ─────────────────────────────────────────────────────────────────────
l6: base
	docker compose up lab6

l6-build: base
	docker compose up --build lab6
