# Compilador Cool → JVM Bytecode

Compilador da linguagem **Cool** escrito em **Clojure** que realiza análise léxica, sintática, semântica e gera **JVM Bytecode** (`.class`) executável diretamente na JVM.

## Pré-requisitos

- [Java (JDK 8+)](https://adoptium.net/) (`java` e `javac`)
- [Clojure CLI](https://clojure.org/guides/getting_started) (`clj` / `clojure`)

## Como compilar e executar

Na raiz do projeto, execute:

```bash
clj -M -m compiler test.cl -o out
java -cp out Main
```

- O diretório de saída padrão é `out/` (a flag `-o <dir>` é opcional).
- Também é possível testar com os exemplos em `tests/`, por exemplo `tests/exprs.cl` ou `tests/stress_self_type.cl`.

### Rodar apenas a análise semântica

```bash
clj -M -m semantic test.cl
```

## Testes

Na raiz do projeto:

```bash
# Suíte de testes semânticos (pasta tests/)
clj -M -m test-runner

# Testes unitários do runtime Java (cool.runtime.*)
clj -M:test -m runtime-test

# Testes end-to-end (compilação, verificação ASM e execução na JVM)
clj -M:test -m codegen-test
```

## Runtime Java

As classes básicas do Cool (`Object`, `IO`, `Int`, `String`, `Bool`) ficam em `runtime/cool/runtime/` e já estão pré-compiladas em `resources/cool/runtime/`. Caso altere algum arquivo `.java`, recompile com:

```bash
javac --release 8 -d resources runtime/cool/runtime/*.java
```
