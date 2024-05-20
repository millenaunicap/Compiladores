import ply.lex as lex
import ply.yacc as yacc

reserved = {
    'if': 'IF',
    'else': 'ELSE',
    'while': 'WHILE',
    'for': 'FOR',
    'switch': 'SWITCH',
    'case': 'CASE',
    'default': 'DEFAULT',
    'struct': 'STRUCT',
    'break': 'BREAK',
    'continue': 'CONTINUE',
    'return': 'RETURN',
    'int': 'INT',
    'float': 'FLOAT',
    'double': 'DOUBLE',
    'char': 'CHAR',
    'boolean': 'BOOLEAN'
}

tokens = list(reserved.values()) + [
    'SEMICOLON',
    'ID',
    'NUMBER',
    'NUM_DEC',
    'TEXTO',
    'COMMA',
    'MINUS',
    'PLUS',
    'DIV',
    'MULT',
    'LPAREN',
    'RPAREN',
    'EQUALS',
    'LBRACE',
    'RBRACE',
    'RBRACKET',
    'LBRACKET',
    'DOT',
    'COMMENTS',
    'COLON',
    'AND',
    'OR',
    'MOD',
    'NOT',
    'INCREMENT',
    'DECREMENT',
    'MAIOR_QUE',
    'MENOR_QUE',
    'MA_IGUAL',
    'ME_IGUAL',
    'DIF_DE',
    'COMPARA',
    'ARROW'
]

t_ignore = ' \t\n'
t_SEMICOLON = r';'
t_NUM_DEC = r'\d+\.\d+'
t_MAIOR_QUE = r'>'
t_MENOR_QUE = r'<'
t_MA_IGUAL = r'>='
t_ME_IGUAL = r'<='
t_DIF_DE = r'!='
t_COMPARA = r'=='
t_COMMA = r','
t_TEXTO = r'".*?"'
t_MINUS = r'-'
t_PLUS = r'\+'
t_MULT = r'\*'
t_DIV = r'/'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_EQUALS = r'='
t_LBRACE = r'{'
t_RBRACE = r'}'
t_LBRACKET = r'\['
t_RBRACKET = r'\]'
t_DOT = r'\.'
t_COMMENTS = r'\/\/.*'
t_COLON = r':'
t_AND = r'&&'
t_OR = r'\|\|'
t_MOD = r'%'
t_NOT = r'!'
t_INCREMENT = r'\+\+'
t_DECREMENT = r'--'
t_ARROW = r'->'

def t_NUMBER(t):
    r'\d+'
    t.value = int(t.value)
    return t

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value, 'ID')
    return t

def t_error(t):
    print(f"Caractere inesperado: {t.value[0]}")
    t.lexer.skip(1)
    return t

lexer = lex.lex()

code = """
int main() {
    int a = 5; // comentario teste
    if (a > 0) {
        a = a + 1;
    }
}
"""

lexer.input(code)

precedence = (
    ('right', 'EQUALS'),
    ('left', 'OR'),
    ('left', 'AND'),
    ('nonassoc', 'MAIOR_QUE', 'MENOR_QUE', 'MA_IGUAL', 'ME_IGUAL', 'DIF_DE', 'COMPARA'),
    ('left', 'PLUS', 'MINUS'),
    ('left', 'MULT', 'DIV', 'MOD'),
    ('right', 'NOT'),
    ('left', 'INCREMENT', 'DECREMENT'),
    ('left', 'ARROW'),
)

def p_program(p):
    'Programa : ListaDeclaracoes'
    p[0] = {'declaracoes': p[1]}

def p_lista_declaracoes(p):
    '''
    ListaDeclaracoes : Declaracao
                     | ListaDeclaracoes Declaracao
    '''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = p[1] + [p[2]]

def p_declaracao(p):
    '''
    Declaracao : DeclaracaoVariavel
               | DeclaracaoFuncao
               | Comentario
               | DeclaracaoEstrutura
               | EstruturaControle
               | Expressao
    '''
    p[0] = p[1]

def p_declaracao_variavel(p):
    '''
    DeclaracaoVariavel : Tipo ID SEMICOLON
                       | Tipo ID EQUALS Expressao SEMICOLON
    '''
    if len(p) == 4:
        p[0] = {'tipo': p[1], 'id': p[2]}
    else:
        p[0] = {'tipo': p[1], 'id': p[2], 'valor': p[4]}

def p_tipo(p):
    '''
    Tipo : INT
         | FLOAT
         | DOUBLE
         | CHAR
         | BOOLEAN
    '''
    p[0] = p[1]

def p_declaracao_funcao(p):
    '''
    DeclaracaoFuncao : Tipo ID LPAREN Parametros RPAREN Bloco
    '''
    p[0] = {'tipo': p[1], 'id': p[2], 'parametros': p[4], 'bloco': p[6]}

def p_parametros(p):
    '''
    Parametros : Parametro
               | Parametro COMMA Parametros
               | vazio
    '''
    if len(p) == 2:
        p[0] = [p[1]] if p[1] is not None else []
    else:
        p[0] = [p[1]] + p[3]

def p_parametro(p):
    '''
    Parametro : Tipo ID
    '''
    p[0] = {'tipo': p[1], 'id': p[2]}

def p_bloco(p):
    '''
    Bloco : LBRACE ListaDeclaracoes RBRACE
    '''
    p[0] = {'declaracoes': p[2]}

def p_comentario(p):
    '''
    Comentario : COMMENTS
    '''
    p[0] = p[1]

def p_expressao(p):
    '''
    Expressao : Atribuicao
              | ExpressaoLogica
              | ExpressaoRelacional
              | ExpressaoAritmetica
              | Termo
              | Fator
    '''
    p[0] = p[1]

def p_atribuicao(p):
    '''
    Atribuicao : ID EQUALS Expressao
    '''
    p[0] = {'id': p[1], 'valor': p[3]}

def p_estrutura_controle(p):
    '''
    EstruturaControle : If
                      | While
                      | For
                      | Switch
                      | Break
                      | Continue
                      | Return
    '''
    p[0] = p[1]

def p_if(p):
    '''
    If : IF LPAREN Expressao RPAREN Bloco
       | IF LPAREN Expressao RPAREN Bloco ELSE Bloco
    '''
    if len(p) == 6:
        p[0] = {'condicao': p[3], 'bloco': p[5]}
    else:
        p[0] = {'condicao': p[3], 'bloco': p[5], 'else': p[7]}

def p_while(p):
    '''
    While : WHILE LPAREN Expressao RPAREN Bloco
    '''
    p[0] = {'condicao': p[3], 'bloco': p[5]}

def p_for(p):
    '''
    For : FOR LPAREN Expressao SEMICOLON Expressao SEMICOLON Expressao RPAREN Bloco
    '''
    p[0] = {'init': p[3], 'condicao': p[5], 'iteracao': p[7], 'bloco': p[9]}

def p_switch(p):
    '''
    Switch : SWITCH LPAREN Expressao RPAREN LBRACE CaseLista RBRACE
    '''
    p[0] = {'expressao': p[3], 'cases': p[6]}

def p_case_lista(p):
    '''
    CaseLista : CaseDecl
              | CaseLista CaseDecl
    '''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = p[1] + [p[2]]

def p_case_decl(p):
    '''
    CaseDecl : CASE Expressao COLON Bloco
             | DEFAULT COLON Bloco
    '''
    if len(p) == 5:
        p[0] = {'tipo': 'case', 'expressao': p[2], 'bloco': p[4]}
    else:
        p[0] = {'tipo': 'default', 'bloco': p[3]}

def p_break(p):
    '''
    Break : BREAK SEMICOLON
    '''
    p[0] = 'break'

def p_continue(p):
    '''
    Continue : CONTINUE SEMICOLON
    '''
    p[0] = 'continue'

def p_return(p):
    '''
    Return : RETURN Expressao SEMICOLON
    '''
    p[0] = {'return': p[2]}

def p_declaracao_estrutura(p):
    '''
    DeclaracaoEstrutura : STRUCT ID LBRACE ListaDeclaracoes RBRACE SEMICOLON
    '''
    p[0] = {'tipo': 'struct', 'id': p[2], 'declaracoes': p[4]}

def p_vazio(p):
    'vazio :'
    pass

def p_expressao_logica(p):
    '''
    ExpressaoLogica : ExpressaoRelacional
                    | ExpressaoLogica AND ExpressaoRelacional
                    | ExpressaoLogica OR ExpressaoRelacional
                    | NOT ExpressaoRelacional
    '''
    if len(p) == 2:
        p[0] = p[1]
    elif p[2] == '&&':
        p[0] = {'op': 'and', 'left': p[1], 'right': p[3]}
    elif p[2] == '||':
        p[0] = {'op': 'or', 'left': p[1], 'right': p[3]}
    elif p[1] == '!':
        p[0] = {'op': 'not', 'expr': p[2]}

def p_expressao_relacional(p):
    '''
    ExpressaoRelacional : ExpressaoAritmetica
                        | ExpressaoAritmetica MAIOR_QUE ExpressaoAritmetica
                        | ExpressaoAritmetica MENOR_QUE ExpressaoAritmetica
                        | ExpressaoAritmetica MA_IGUAL ExpressaoAritmetica
                        | ExpressaoAritmetica ME_IGUAL ExpressaoAritmetica
                        | ExpressaoAritmetica DIF_DE ExpressaoAritmetica
                        | ExpressaoAritmetica COMPARA ExpressaoAritmetica
    '''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = {'op': p[2], 'left': p[1], 'right': p[3]}

def p_expressao_aritmetica(p):
    '''
    ExpressaoAritmetica : ExpressaoAritmetica PLUS Termo SEMICOLON
                        | ExpressaoAritmetica MINUS Termo
                        | Termo
    '''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = {'op': p[2], 'left': p[1], 'right': p[3]}

def p_termo(p):
    '''
    Termo : Termo MULT Fator
          | Termo DIV Fator
          | Termo MOD Fator
          | Fator
    '''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = {'op': p[2], 'left': p[1], 'right': p[3]}

def p_fator(p):
    '''
    Fator : LPAREN Expressao RPAREN
          | NUMBER
          | NUM_DEC
          | ID
          | TEXTO
    '''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = p[2]

def p_error(t):
    print(f"Syntax Error: Illegal character '{t.value[0]}' at line {t.lineno}")
    t.lexer.skip(1)
    
parser = yacc.yacc()

result = parser.parse(code)
print(f"Resultado '{code}': {result}")

if result is not None:
    print("O código está correto!")
