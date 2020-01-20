FROM haskell:8.6.5 AS builder

RUN mkdir -p /app/user
WORKDIR /app/user
COPY stack.yaml stack.yaml.lock *.cabal ./

RUN stack build --dependencies-only

COPY ./ /app/user/
RUN stack install

FROM debian:stretch

RUN apt-get update -y && apt-get install -y \
  libgmp10 \
  sqlite3

RUN mkdir -p /app/user
WORKDIR /app/user

COPY ./ /app/user/
COPY --from=builder /root/.local/bin/servant-photo-gallery-exe /usr/local/bin/servant-photo-gallery

RUN sqlite3 gallery.db < migrations/01_initial.sql
CMD ["/usr/local/bin/servant-photo-gallery"]
