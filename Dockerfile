FROM fpco/stack-build:lts-8.5
COPY . /app/
WORKDIR /app
RUN stack install --install-ghc && stack clean
CMD ["/bin/bash", "-c", "`stack path --local-bin`/authority-exe"]
