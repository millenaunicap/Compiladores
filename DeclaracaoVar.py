import ply.lex as lex
import ply.yacc as yacc

tokens = [
    'NUM_INT',
    'NUM_DEC',
    'ID',
    'TEXTO',
    'PALAVRA_RESERVADA',
    'COMENTARIO',
    'OPERADOR',
    'SIMBOLO_ESPECIAL'
]

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
    'return': 'PALAVRA_RESERVADA'
}

t_ignore = ' \t'

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
    t.type = 'ID' if t.value not in reserved_words else 'PALAVRA_RESERVADA'
    return t

def t_TEXTO(t):
    r'\".*?\"'
    return t

def t_COMENTARIO(t):
    r'//.*'
    pass

def t_OPERADOR(t):
    r'(\+|\-|\*|\/|\%|\&\&|\|\||\!|\>|\<|\>=|\<=|\!=|\==|\=)'
    return t

def t_SIMBOLO_ESPECIAL(t):
    r'[\(\)\[\]\{\}\,\;]'
    return t

def t_error(t):
    print("Caractere ilegal: '%s'" % t.value[0])
    t.lexer.skip(1)

lexer = lex.lex()

palavras_reservadas_tipo = {'int', 'float', 'char', 'boolean', 'jgchcg'}


# Grammar rules
def p_DeclaracaoVariavel(p):
    '''DeclaracaoVariavel : Tipo ID Finalizacao
                          | Tipo ID Opatribuicao Expressao Finalizacao'''

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