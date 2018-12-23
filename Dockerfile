FROM fpco/haskell-lang-prod
COPY ob-fun-ws18-exe /bin/ob-fun-ws18-exe
RUN useradd -u 1000 -m app
USER app
CMD ["/bin/ob-fun-ws18-exe"]
