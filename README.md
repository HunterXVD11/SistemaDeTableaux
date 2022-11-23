# Trabalho Linguagens de Programação - UFF - 2022.2

#
#
#
#

## Alunos

* Israel Navarro
* Luiz Claudio Willner

#### Professor Bruno Lopes


#
#
#
#


## Como usar o programa?


### Método 1: Pelo arquivo executável (padrão)
Rode o arquivo executável "Tableaux.exe" pelo terminal.


### Método 2: Modo interpretado pelo GHCI
No terminal, digite o comando "ghci" para entrar no modo interpretador do Haskell. Ao fazer isso, carregue o script através do comando ":load Tableaux.hs". Para executar a funcionalidade padrão, basta digitar "main" depois de carregado o script. O modo interpretado ajuda a executar funções intermediárias pelo terminal, facilitando a compreensão do código


### Considerações sobre a entrada
A notação das fórmulas é feita de forma pré fixada. Além disso, deve conter parênteses envolvendo TODAS as fórmulas, sejam elas compostas ou atômicas, e sem NENHUM espaço em branco. Desse modo, uma fórmula "a v b", com a notação descrita, ficaria "(v(a,b))". Alguns outros exemplos de fórmulas de entrada:

* (b v a) -> (c v a)    =====>  (>((v((b),(a))),(v((c),(a)))))
* b -> (a ∧ (b ∨ a))    =====>  (>((b),(^((a),(v((b),(a)))))))
* a -> (a -> (b -> a))  =====>  (>((a),(>((a),(>((b),(a)))))))
* ( p v (q ^ r) ) -> ( (p v q) ^ (p v r) )  ========>  (>((v((p),(^((q),(r))))),(^((v((p),(q))),(v((p),(r)))))))


### Simbologia das operações
* "e": ^
* "ou": v
* "se então": >
* "negação": ~
