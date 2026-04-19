# ── Lab-parametric image ──────────────────────────────────────────────────────
# Inherits all shared R / system layers from r-lab-base (see Dockerfile.base).
# Build the base once with `make base`, then use this file for any lab by
# passing --build-arg SCRIPT=<script>.R with the lab directory as context.
#
# Used by docker-compose.yml for all lab services.
#
FROM r-lab-base:latest

# ── Lab-specific script ───────────────────────────────────────────────────────
ARG SCRIPT
ENV R_SCRIPT=${SCRIPT}

COPY ${SCRIPT} .

# ── Entry point ───────────────────────────────────────────────────────────────
CMD ["sh", "-c", "Rscript $R_SCRIPT"]
