import ply.lex as lex
import ply.yacc as yacc

# Definição dos tokens
tokens = [
    'NUM_INT',
    'NUM_DEC',
    'ID',
    'TEXTO',
    'PALAVRA_RESERVADA',
    'COMENTARIO',
    'OPERADOR',
    'SIMBOLO_ESPECIAL',
          'COMMA',
          'MINUS',
          'PLUS',
          'DIV',
          'MULT',
          'LPAREN',
          'RPAREN',
          'SEMICOLON',
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

# Palavras reservadas
reserved_words = {
    'int': 'PALAVRA_RESERVADA',
    'float': 'PALAVRA_RESERVADA',
    'char': 'PALAVRA_RESERVADA',
    'boolean': 'PALAVRA_RESERVADA',
    'void': 'PALAVRA_RESERVADA',
    'if': 'PALAVRA_RESERVADA',
    'else': 'PALAVRA_RESERVADA',
    'for': 'PALAVRA_RESERVADA',
    'while': 'PALAVRA_RESERVADA',
    'scanf': 'PALAVRA_RESERVADA',
    'println': 'PALAVRA_RESERVADA',
    'main': 'PALAVRA_RESERVADA',
    'return': 'RETURN',
    'struct': 'PALAVRA_RESERVADA',
    'double': 'PALAVRA_RESERVADA',
    'long': 'PALAVRA_RESERVADA',
    'short': 'PALAVRA_RESERVADA',
    'unsigned': 'PALAVRA_RESERVADA',
    'signed': 'PALAVRA_RESERVADA',
    'const': 'PALAVRA_RESERVADA',
    'static': 'PALAVRA_RESERVADA',
    'switch': 'PALAVRA_RESERVADA',
    'case': 'PALAVRA_RESERVADA',
    'default': 'PALAVRA_RESERVADA',
    'break': 'PALAVRA_RESERVADA',
    'continue': 'PALAVRA_RESERVADA',
    'do': 'PALAVRA_RESERVADA',
    'goto': 'PALAVRA_RESERVADA',
    'sizeof': 'PALAVRA_RESERVADA',
    'typedef': 'PALAVRA_RESERVADA',
    'volatile': 'PALAVRA_RESERVADA',
    'enum': 'PALAVRA_RESERVADA',
    'extern': 'PALAVRA_RESERVADA',
    'register': 'PALAVRA_RESERVADA',
    'union': 'PALAVRA_RESERVADA',
    'public': 'PALAVRA_RESERVADA',
    'void': 'PALAVRA_RESERVADA',
    'private': 'PALAVRA_RESERVADA'

}

t_MAIOR_QUE = r'\>'
t_MENOR_QUE = r'\<'
t_MA_IGUAL = r'\>\='
t_ME_IGUAL = r'\<\='
t_DIF_DE = r'\!\='
t_COMPARA = r'\=\='
t_COMMA = r'\,'
t_MINUS = r'-'
t_PLUS = r'\+'
t_MULT = r'\*'
t_DIV = r'\/(?!\/)'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_SEMICOLON = r';'
t_EQUALS = r'='
t_LBRACE = r'\{'
t_RBRACE = r'\}'
t_LBRACKET = r'\['
t_RBRACKET = r'\]'
t_DOT = r'\.'
t_COMMENTS = r'//.*'
t_COLON = r':'
t_AND = r'\&\&'
t_OR = r'\|\|'
t_MOD = r'\%'
t_NOT = r'\!'
t_INCREMENT = r'\+\+'
t_DECREMENT = r'\-\-'
t_ARROW = r'\-\>'

# Dicionário para armazenar identificadores
identifiers = {}

# Regras de expressão regular para tokens simples
def t_NUM_INT(t):
    r'\d+'
    t.value = int(t.value)
    return t

def t_NUM_DEC(t):
    r'\d+\.\d+'
    t.value = float(t.value)
    return t

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    t.type = reserved_words.get(t.value, 'ID')  # Use o ID ou PALAVRA_RESERVADA conforme apropriado
    if t.type == 'ID':
        if t.value in identifiers:
            original_token = identifiers[t.value]
            t.lineno = original_token.lineno
            t.lexpos = original_token.lexpos
        else:
            identifiers[t.value] = t
    return t

def t_TEXTO(t):
    r'\".*?\"'
    return t

def t_COMENTARIO(t):
    r'//.*'
    return t

def t_OPERADOR(t):
    r'(\+|\-|\*|\/|\%|\&\&|\|\||\!|\>|\<|\>=|\<=|\!=|\==|\=)'
    return t

def t_SIMBOLO_ESPECIAL(t):
    r'[\(\)\[\]\{\}\,\;]'
    return t

# Ignorar espaços e tabulações
t_ignore = ' \t'

# Tratamento de erros
def t_error(t):
    print(f"Caractere ilegal: '{t.value[0]}'")
    t.lexer.skip(1)

# Construção do lexer
lexer = lex.lex()

# Entrada de exemplo
lexer.input("123 123.456 variavellll \"texto\" int if if variavellll 123 struct double // comentario")

# Impressão dos tokens
for token in lexer:
    print(f'Type: {token.type}, Value: {token.value}, Line: {token.lineno}, Position: {token.lexpos}')

palavras_reservadas_tipo = {'int', 'float', 'char', 'boolean', 'jgchcg'}


# Grammar rules

def p_DeclaracaoVariavel(p):
    '''DeclaracaoVariavel : Tipo ID Finalizacao 
                          | Tipo ID Opatribuicao Expressao Finalizacao'''


def p_DeclaracaoFunc(p):
    '''DeclaracaoFunc : Tipo ID LPAREN Parametros RPAREN LBRACE Declaracao RBRACE
                          | Tipo ID LPAREN RPAREN LBRACE Declaracao RBRACE '''

def p_Declaracao(p):
    ''' Declaracao : DeclaracaoVariavel
               | DeclaracaoFuncao
               | Expressao '''

def p_Parametro(p):
    '''
    Parametro : Tipo ID
             | Tipo ID LBRACKET RBRACKET
             | Tipo DOT DOT DOT ID
    '''

def p_Parametros(p):
    '''
    Parametros : Parametro
               | Parametro COMMA Parametro
    '''

def p_Tipo(p):

    '''Tipo : PALAVRA_RESERVADA
        '''
    token = p.slice[1]  # Obtém o token correspondente à primeira produção da regra 'Tipo'
    if token is not None and token.type == 'PALAVRA_RESERVADA' and token.value in palavras_reservadas_tipo:
        return
    else:
        p_error(p)

def p_Expressao(p):
    '''Expressao : ID
                 | NUM_INT
                 | NUM_DEC'''
    
def p_Finalizacao(p):
    '''Finalizacao : SIMBOLO_ESPECIAL'''
    if p[1] != ';':
        p_error(p)

def p_Opatribuicao(p):
    '''Opatribuicao : OPERADOR'''
    if p[1] != '=':
        p_error(p)

def p_error(p):
    print("Erro de sintaxe")

# Build the parser
parser = yacc.yacc()

# Test the parser
entrada = "int x = 10;"
parser.parse(entrada)