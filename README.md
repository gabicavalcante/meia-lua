# meia-lua

## linguagem

`meia-lua` é uma linguagem inspirada em Lua, e criada durante a disciplina de Linguagem e Programção de Compiladores.
Nosso objetivo com a linguagem é fornecer flexibilidade e aperfeiçoamento para o domínio de Inteligência Artificial, com ênfase em Jogos Digitais.

Para descrever os aspectos léxicos da linguagem, usaremos a BNF estendida, na qual {a} significa 0 ou mais a's e [a] significa um a opcional. Não-terminais são mostrados como `non-terminal`, palavras-chave são mostradas como **kword** e outros símbolos terminais são mostrados como `=´. 

### Convenções Léxicas
Identificadores podem ser qualquer cadeia de letras, dígitos, e sublinhados que não começam com um dígito.

```
and       break     do        else      elseif
end       false     for       function  if
in        local     nil       not       or
repeat    return    then      true      until     while
```

A linguagem diferencia minúsculas de maiúsculas: `return` é uma palavra reservada, mas `Return` e `RETURN` são dois nomes válidos diferentes. 
 
Cadeias de caracteres literais podem ser delimitadas através do uso de aspas simples ou aspas duplas.

Um comentário começa com um hífen duplo (--) em qualquer lugar, desde que fora de uma cadeia de caracteres.

###  Valores e Tipos


```
var ::= expprefixo `[´ exp `]´
```

```
var ::= expprefixo `.´ Nome
```

### Variáveis

Variáveis são espaços usados para armazenar valores.

Existem três tipos de variáveis na nossa linguagem: **variáveis globais**, **variáveis locais** e **campos de tabelas**.

Um nome denota identificadores e são usados para nomear variáveis e campos de tabelas. 

```
var ::= Nome
```

`Nome` e é uma `string` de caracteres usadas para identificar variável global ou uma variávei local. Ele obedece a forma: uma letra seguida por uma `string` formada de letras, dígitos e underscore (`_`). 

`meia-lua` é **case sensitive**, ou seja, letras minúsculas e maiúsculas em nomes são diferentes. 

Toda variável é uma variável local, e para ser global ela precisa ser explicitamente declarada como [global](#declarações-locais). Variáveis locais podem ser acessadas somente por funçnoes definidas dentro do seu escopo. 

Antes da variável receber um valor através da atribuição, o seu valor default é **nil**.

### Palavras reservadas

Palavras especiais são palavras reservadas, ou seja, não podem ser utilizadas como um nome. 



### Comandos

```
trecho ::= {comando [`;´]}
```

#### Blocos

```
bloco ::= trecho
comando ::= do bloco end
```


#### Atribuição

```
comando ::= listavar `=´ listaexp
listavar ::= var {`,´ var}
listaexp ::= exp {`,´ exp}
```


#### Estruturas de Controle

```
comando ::= while exp do bloco end
comando ::= repeat bloco until exp
comando ::= if exp then bloco {elseif exp then bloco} [else bloco] end
```
#### Chamadas de Função como Comandos

#### Declarações Locais
```
comando ::= local listadenomes [`=´ listaexp]
```
### Expressões

#### Operadores Aritméticos

#### Operadores Relacionais

#### Operadores Lógicos

#### Concatenação

#### O Operador de Comprimento

#### Precedência

#### Construtores de Tabelas

#### Chamadas de Função

#### Definições de Funções

#### Regras de Visibilidade
