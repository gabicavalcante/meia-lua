# meia-lua

f
    -> bloco1
    -> bloco2
var -> type, id, value, scopo, tempo de vida e endereço

`meia-lua` é uma linguagem inspirada em Lua, e criada durante a disciplina de Linguagem e Programção de Compiladores.
Nosso objetivo com a linguagem é fornecer flexibilidade e aperfeiçoamento para o domínio de Inteligência Artificial, com ênfase em Jogos Digitais.

Para descrever os aspectos léxicos da linguagem, usaremos a BNF estendida, na qual {a} significa 0 ou mais a's e [a] significa um a opcional. Não-terminais são mostrados como `non-terminal`, palavras-chave são mostradas como **kword** e outros símbolos terminais são mostrados como `=´. 

### Convenções Léxicas
*Identificadores* ou *Nomes* podem ser qualquer cadeia de letras, dígitos, e sublinhados que não começam com um dígito. Os identificadores serão úteis para nomear variáveis e *tables*.

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

`meia-lua` é uma linguagem dinamicamente tipada, ou seja, as variáveis vão possuir o tipo do valor que elas armazenarem. Os tipos presentes em nossa linguagem são: *nil*, *boolean*, *number*, *integer*, *string* e *table*. 
O tipo *nil* é usado para representar um conteudo de valor não util. O *boolean* é o tipo dos valores **false** e **true**. O *number* representa os números reais e o *integer* representa os números inteiros. Tanto o **nil**, como o **false** e o número **0** são vistos na linguagem como valores que tornam a condição false. A *string* representa uma cadira de caracteres. 

O tipo *table* implementa um array associativo, indexados com valores do tipo string. Tables são heterogêneas, podendo armazenas valores de todos os tipos, menos o **nil**. Além disso, é o tipo da linguagem que pode ser utilizado para estruturar dados, como grafos, árvores, linguagem, etc.



### Variáveis

Variáveis são espaços usados para armazenar valores.

Existem dois tipos de variáveis na nossa linguagem: **variáveis globais** e **variáveis locais**.

Um nome denota identificadores e são usados para nomear variáveis e campos de tabelas. 

```
var ::= Nome
```

`Nome` e é uma `string` de caracteres usadas para identificar variável global ou uma variávei local. Ele obedece a forma: uma letra seguida por uma `string` formada de letras, dígitos e underscore (`_`). 

`meia-lua` é **case sensitive**, ou seja, letras minúsculas e maiúsculas em nomes são diferentes. 

Toda variável é uma variável local, e para ser global ela precisa ser explicitamente declarada como [global](#declarações-locais). Variáveis locais podem ser acessadas somente por funçnoes definidas dentro do seu escopo. 

Antes da variável receber um valor através da atribuição, o seu valor default é **nil**.