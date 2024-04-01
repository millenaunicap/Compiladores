import ply.lex as lex

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
    return t

def t_OPERADOR(t):
    r'(\+|\-|\*|\/|\%|\&\&|\|\||\!|\>|\<|\>=|\<=|\!=|\==|\=)'
    return t

def t_SIMBOLO_ESPECIAL(t):
    r'[\(\)\[\]\{\}\,\;]'
    return t

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

def t_error(t):
    print("Caractere ilegal: '%s'" % t.value[0])
    t.lexer.skip(1)

lexer = lex.lex()

lexer.input("123 123.456 variavel \"texto\" int if // comentario")

for token in lexer:
    print(token)