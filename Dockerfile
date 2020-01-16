FROM haskell:8.6.5

RUN apt-get update -y && apt-get install sqlite3 -y

RUN mkdir -p /app/user
WORKDIR /app/user
COPY stack.yaml stack.yaml.lock *.cabal ./

RUN export PATH=$(stack path --local-bin):$PATH
RUN stack build --dependencies-only

COPY ./ /app/user/
RUN stack install

RUN sqlite3 gallery.db < migrations/01_initial.sql
