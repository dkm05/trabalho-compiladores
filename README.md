## Como executar o lexer

Para executar o lexer, é necessário ter o [Clojure](https://clojure.org/guides/getting_started) e o comando `clj` instalados na máquina.

Na raiz do projeto, execute o comando abaixo para testar o lexer com o arquivo `test.cl`:

```bash
clj -M -m parser test.cl
```

No momento, o parser não faz nada. apenas mostra o token na cli, com a posição dele.
