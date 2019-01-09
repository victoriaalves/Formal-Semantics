# Trabalho Final de Semântica Formal 2017/2

Devido a problemas com o interpretador de ocaml, nós criamos um único arquivo (FinalProjectSF.ml) com todas as funcionalidades pedidas no trabalho.
As funcionalidades são: 
* Um interpretador para L1 composto de um avaliador (big-step) e de um inferidor de tipos
* Um compilador de L1 para a linguagem de máquina abstrata SSM2
* Um interpretador para a linguagem SSM2 de acordo com a semântica operacional de SSM2

Adicionamos arquivos individuais contendo cada um uma das funcionalidades pedidas dentro de src/ para facilitar a correção do professor. O código nestes arquivos é exatamente o mesmo do existente em FinalProjectSF.ml. As funcionalidades estão nos arquivos individuais:
* src/interpreter_SSM2.ml -> Interpretador de SSM2 
* src/compiler_SSM2.ml -> Compilador de L1 para SSM2 
* src/typeinfer_L1.ml -> Inferidor de tipos para L1
* src/bigstep_L1.ml -> Avaliador big-step para L1

Os tipos definidos estão nos arquivos individuais:
* src/instruction_SSM2.ml -> Definição de instrução e valor armazenável em SSM2
* src/term_L1.ml -> Definição dos tipos e valores em L1

**EXTENÇÕES** Nós adicionamos o operador de divisão na linguagem L1, a SSM2 foi extendida para comportar essa mudança

Alunas:
* Lisiane Aguiar
* Marcely Zanon Boito
* Victoria Elizabetha Alves
