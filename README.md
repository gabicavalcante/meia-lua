# meia-lua

## linguagem

`meia-lua` é uma linguagem inspirada em Lua, e criada durante a disciplina de Linguagem e Programção de Compiladores.
Nosso objetivo com a linguagem é fornecer flexibilidade e aperfeiçoamento para o domínio de Inteligência Artificial, com ênfase em Jogos Digitais.

Para descrever os aspectos léxicos da linguagem, usaremos a BNF estendida, na qual {a} significa 0 ou mais a's e [a] significa um a opcional. Não-terminais são mostrados como non-terminal, palavras-chave são mostradas como **kword** e outros símbolos terminais são mostrados como `=´. 

### Convenções Léxicas
Identificadores podem ser qualquer cadeia de letras, dígitos, e sublinhados que não começam com um dígito.

```
and       break     do        else      elseif
end       false     for       function  if
in        local     nil       not       or
repeat    return    then      true      until     while
```

A linguagem diferencia minúsculas de maiúsculas: `return` é uma palavra reservada, mas `Return` e `RETURN` são dois nomes válidos diferentes. 
 
Cadeias de caracteres literais podem ser delimitadas através do uso de aspas simples ou aspas duplas(?)

Um comentário começa com um hífen duplo (--) em qualquer lugar, desde que fora de uma cadeia de caracteres.

###  Valores e Tipos

```
var ::= Nome
```

```
var ::= expprefixo `[´ exp `]´
```

```
var ::= expprefixo `.´ Nome
```

### Variáveis

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
