FROM haskell:latest

WORKDIR /app

COPY . .

RUN cabal update
RUN cabal build

EXPOSE 3000

CMD ["cabal", "run"]
